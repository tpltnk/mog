package main

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

//go:generate stringer -type=TokenType
type TokenType int

func (tt TokenType) isExprStart() bool {
	_, okPrefix := prefixOps[tt]
	_, okLits := lits[tt]
	_, okGroup := groupOps[tt]
	return okPrefix || okLits || okGroup
}

const (
	TT_IDENT TokenType = iota
	TT_BYTE
	TT_STRING
	TT_INT
	TT_FLOAT

	TT_PLUS
	TT_MINUS
	TT_STAR
	TT_SLASH
	TT_PERCENT

	TT_BIT_AND
	TT_BIT_OR
	TT_BIT_NOT
	TT_BIT_LSH
	TT_BIT_RSH

	TT_AND
	TT_OR

	TT_LT
	TT_GT
	TT_LTE
	TT_GTE
	TT_EQ
	TT_NEQ

	TT_QMARK
	TT_EMARK

	TT_DOT
	TT_COLON
	TT_COMMA

	TT_RANGE

	TT_HASH

	TT_IS

	TT_LBRACK
	TT_RBRACK
	TT_LBRACE
	TT_RBRACE
	TT_LPAREN
	TT_RPAREN

	TT_KW_IF
	TT_KW_ELSE
	TT_KW_FOR
	TT_KW_FUN
	TT_KW_BREAK
	TT_KW_CONTINUE
	TT_KW_RETURN
	TT_KW_YIELD
	TT_KW_IMPORT
	TT_KW_CLASS
	TT_KW_MACHINE
	TT_KW_ASSERT
	TT_KW_IN

	TT_CMT_LINE
	TT_CMT_DOC

	TT_SPECIAL
	TT_INVALID
	TT_EOF
)

var (
	keywords = map[string]TokenType{
		"fun":      TT_KW_FUN,
		"for":      TT_KW_FOR,
		"if":       TT_KW_IF,
		"else":     TT_KW_ELSE,
		"yield":    TT_KW_YIELD,
		"return":   TT_KW_RETURN,
		"break":    TT_KW_BREAK,
		"continue": TT_KW_CONTINUE,
		"import":   TT_KW_IMPORT,
		"in":       TT_KW_IN,
		"class":    TT_KW_CLASS,
		"assert":   TT_KW_ASSERT,
		"machine":  TT_KW_MACHINE,
	}
	ops = map[string][]TokenType{
		"+":  {TT_PLUS},
		"-":  {TT_MINUS},
		"*":  {TT_STAR},
		"/":  {TT_SLASH},
		"%":  {TT_PERCENT},
		",":  {TT_COMMA},
		"..": {TT_DOT, TT_RANGE},
		":":  {TT_COLON},
		"#":  {TT_HASH},
		"~":  {TT_BIT_NOT},
		"<=": {TT_LT, TT_LTE},
		">=": {TT_GT, TT_GTE},
		"==": {TT_IS, TT_EQ},
		"?":  {TT_QMARK},
		"!=": {TT_EMARK, TT_NEQ},
		"&&": {TT_BIT_AND, TT_AND},
		"||": {TT_BIT_OR, TT_OR},
		"[":  {TT_LBRACK},
		"]":  {TT_RBRACK},
		"{":  {TT_LBRACE},
		"}":  {TT_RBRACE},
		"(":  {TT_LPAREN},
		")":  {TT_RPAREN},
	}
	prefixOps = map[TokenType]bool{
		TT_MINUS:   true,
		TT_BIT_NOT: true,
		TT_EMARK:   true,
		TT_STAR:    true,
		TT_COLON:   true,
	}
	groupOps = map[TokenType]TokenType{
		TT_LPAREN: TT_RPAREN,
	}
	suffixUnclosedOps = map[TokenType]bool{
		TT_EMARK: true,
		TT_QMARK: true,
		TT_COLON: true,
	}
	suffixClosedOps = map[TokenType]TokenType{
		TT_LBRACK: TT_RBRACK,
		TT_LPAREN: TT_RPAREN,
	}
	prefixBindPowers = map[TokenType]int{
		TT_PLUS:    31,
		TT_MINUS:   31,
		TT_BIT_NOT: 31,
		TT_EMARK:   31,
		TT_STAR:    31,
		TT_COLON:   31,
	}
	suffixBindPowers = map[TokenType]int{
		TT_EMARK:  32,
		TT_QMARK:  32,
		TT_LBRACK: 32,
		TT_LPAREN: 32,
		TT_COLON:  32,
	}
	rightBindPowers = map[TokenType]int{
		TT_IS:      3,
		TT_COMMA:   5,
		TT_OR:      7,
		TT_AND:     9,
		TT_BIT_OR:  11,
		TT_BIT_AND: 13,
		TT_NEQ:     15,
		TT_EQ:      15,
		TT_GT:      17,
		TT_LT:      17,
		TT_GTE:     17,
		TT_LTE:     17,
		TT_BIT_LSH: 19,
		TT_BIT_RSH: 19,
		TT_MINUS:   21,
		TT_PLUS:    21,
		TT_SLASH:   23,
		TT_STAR:    23,
		TT_RANGE:   25,
		TT_DOT:     27,
		TT_COLON:   29,
	}
	leftBindPowers = map[TokenType]int{
		TT_IS:      4,
		TT_COMMA:   6,
		TT_OR:      8,
		TT_AND:     10,
		TT_BIT_OR:  12,
		TT_BIT_AND: 14,
		TT_NEQ:     16,
		TT_EQ:      16,
		TT_GT:      18,
		TT_LT:      18,
		TT_GTE:     18,
		TT_LTE:     18,
		TT_BIT_LSH: 20,
		TT_BIT_RSH: 20,
		TT_MINUS:   22,
		TT_PLUS:    22,
		TT_SLASH:   24,
		TT_STAR:    24,
		TT_RANGE:   26,
		TT_DOT:     28,
		TT_COLON:   30,
	}
	lits = map[TokenType]bool{
		TT_STRING: true,
		TT_INT:    true,
		TT_BYTE:   true,
		TT_LBRACE: true, // aka block
		TT_IDENT:  true,
		TT_FLOAT:  true,
	}
)

type Token struct {
	tt  TokenType
	pos PositionVector
}

type SourceManager struct {
	parsers map[string]Parser
}

func NewSourceManager(lenHint int) SourceManager {
	return SourceManager{
		parsers: make(map[string]Parser, lenHint),
	}
}

func (sm *SourceManager) AddSource(srcName string, src string) error {
	if _, exists := sm.parsers[srcName]; exists {
		return errors.New("source with specified name already exists")
	}
	sm.parsers[srcName] = NewParser(src)
	return nil
}

func (sm *SourceManager) AddFile(path string) error {
	content, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	return sm.AddSource(path, string(content))
}

func (sm *SourceManager) AddDir(path string) error {
	return filepath.WalkDir(path, func(path string, dirEntry fs.DirEntry, err error) error {
		if dirEntry.Type().IsRegular() && filepath.Ext(dirEntry.Name()) == ".mog" {
			if err := sm.AddFile(path); err != nil {
				return err
			}
		}
		return nil
	})
}

func (sm *SourceManager) ParseSources() map[string]*Block {
	roots := make(map[string]*Block, len(sm.parsers))
	for srcName, parser := range sm.parsers {
		if _, exists := roots[srcName]; exists {
			panic("src with that name already exists")
		}
		roots[srcName] = parser.NextRoot()
	}
	return roots
}

type Parser struct {
	src string

	pos  int
	col  int
	line int

	currTok Token
	nextTok Token

	currBlk  *Block
	blkDepth int
}

func NewParser(src string) Parser {
	p := Parser{
		src:      src,
		pos:      0,
		col:      1,
		line:     1,
		blkDepth: 0,
	}
	p.advance()
	return p
}

func (p *Parser) NextRoot() *Block {
	root := p.NextBlock()
	if root.parent != nil {
		panic("improper nesting detected")
	}
	return root
}

func (p *Parser) NextBlock() *Block {
	p.blkDepth++
	if p.blkDepth > 32 {
		panic("max block depth exceeded")
	}
	p.currBlk = &Block{
		pos: PositionVector{
			start: p.currTok.pos.start,
		},
		stmts:  make([]Statement, 0),
		parent: p.currBlk,
	}
	for {
		stmt := p.nextStatement()
		if stmt == nil {
			break
		}
		p.currBlk.stmts = append(p.currBlk.stmts, stmt)
	}
	p.currBlk.pos.end = p.currTok.pos.end
	blk := p.currBlk
	p.currBlk = p.currBlk.parent
	p.blkDepth--
	return blk
}

func (p *Parser) nextStatement() Statement {
	p.advance()
	if p.currTok.tt == TT_EOF {
		return nil
	}
	switch p.currTok.tt {
	case TT_RBRACE:
		return nil
	case TT_KW_RETURN:
		return p.nextReturn()
	case TT_KW_BREAK:
		return p.nextBreak()
	case TT_KW_CONTINUE:
		return p.nextContinue()
	case TT_KW_YIELD:
		return p.nextYield()
	case TT_KW_ASSERT:
		return p.nextAssert()
	case TT_KW_IMPORT:
		return p.nextImport()
	case TT_KW_IF:
		return p.nextIf()
	case TT_KW_FOR:
		return p.nextFor()
	case TT_KW_FUN:
		return p.nextFunction()
	case TT_KW_CLASS:
		return p.nextClass()
	case TT_EOF:
		return nil
	default:
		return p.nextExpr(0)
	}
}

func (p *Parser) nextReturn() Statement {
	start := p.currTok.pos.start
	end := p.currTok.pos.end
	if p.nextTok.tt.isExprStart() {
		p.advance()
	}
	rhs := p.nextExpr(0)
	if rhs != nil {
		end = rhs.end()
	}
	return Return{PositionVector{start, end}, rhs}
}

func (p *Parser) nextBreak() Statement {
	start := p.currTok.pos.start
	end := p.currTok.pos.end
	if p.nextTok.tt.isExprStart() {
		p.advance()
	}
	rhs := p.nextExpr(0)
	if rhs != nil {
		end = rhs.end()
	}
	return Break{PositionVector{start, end}, rhs}
}

func (p *Parser) nextContinue() Statement {
	start := p.currTok.pos.start
	end := p.currTok.pos.end
	return Continue{PositionVector{start, end}}
}

func (p *Parser) nextYield() Statement {
	start := p.currTok.pos.start
	end := p.currTok.pos.end
	if p.nextTok.tt.isExprStart() {
		p.advance()
	}
	rhs := p.nextExpr(0)
	if rhs != nil {
		end = rhs.end()
	}
	return Yield{PositionVector{start, end}, rhs}
}

func (p *Parser) nextAssert() Statement {
	start := p.currTok.pos.start
	p.advance()
	rhs := p.nextExpr(0)
	if rhs == nil {
		panic("empty assert not allowed")
	}
	return Assert{PositionVector{start, rhs.end()}, rhs}
}

func (p *Parser) nextImport() Statement {
	start := p.currTok.pos.start
	p.advance()
	rhs := p.nextExpr(0)
	if rhs == nil {
		panic("empty import not allowed")
	}
	return Import{PositionVector{start, rhs.end()}, rhs}
}

func (p *Parser) nextClass() Class {
	start := p.currTok.pos.start
	p.advance()
	name := Identifier{p.currTok}
	p.advance()
	params := make([]Parameter, 0)
	for {
		switch p.nextTok.tt {
		case TT_EOF:
			goto out
		case TT_COMMA:
			p.advance()
			continue
		case TT_RBRACE:
			p.advance()
			goto out
		case TT_IDENT:
			p.advance()
			params = append(params, p.nextParameter())
			continue
		default:
			goto out
		}
	}
out:
	return Class{PositionVector{start, p.pos}, name, params}
}

func (p *Parser) nextIf() Statement {
	start := p.pos
	p.advance()
	cond := p.nextExpr(0)
	if cond == nil {
		panic("condition must be set")
	}
	p.advance()
	main := p.NextBlock()
	var alt *Block
	if p.nextTok.tt == TT_KW_ELSE {
		p.advance() // else
		p.advance() // {
		alt = p.NextBlock()
	}
	return If{PositionVector{start, p.pos}, cond, main, alt}
}

func (p *Parser) nextFor() Statement {
	start := p.currTok.pos.start
	p.advance()
	var spec *Specification
	if p.currTok.tt != TT_LBRACE {
		spec = p.nextSpecification()
	}
	var src Expr
	if spec != nil {
		if p.currTok.tt != TT_KW_IN {
			panic("missing 'in' keyword")
		}
		p.advance()
		src = p.nextExpr(0)
		if src == nil {
			panic("iterator source cannot be nil")
		}
		p.advance()
	}
	body := p.NextBlock()
	return For{PositionVector{start, p.currTok.pos.end}, spec, src, body}
}

func (p *Parser) nextFunction() Function {
	start := p.currTok.pos.start
	p.advance()
	sig := p.nextSignature()
	p.advance()
	body := p.NextBlock()
	return Function{PositionVector{start, p.pos}, sig, body}
}

func (p *Parser) nextSignature() Signature {
	start := p.currTok.pos.start
	var ident *Identifier
	var classIdent *Identifier
	if p.currTok.tt == TT_IDENT {
		ident = &Identifier{p.currTok}
		p.advance()
	}
	if p.currTok.tt == TT_DOT {
		if ident == nil {
			panic("class name missing before .")
		}
		classIdent = ident
		p.advance()
		if p.currTok.tt != TT_IDENT {
			panic("expected ident")
		}
		ident = &Identifier{p.currTok}
		p.advance()
	}
	if p.currTok.tt != TT_LPAREN {
		panic("signature must start with (")
	}
	params := make([]Parameter, 0)
	for {
		switch p.nextTok.tt {
		case TT_EOF:
			goto out
		case TT_COMMA:
			p.advance()
			continue
		case TT_RPAREN:
			p.advance()
			goto out
		case TT_IDENT:
			p.advance()
			params = append(params, p.nextParameter())
			continue
		default:
			goto out
		}
	}
out:
	if p.currTok.tt != TT_RPAREN {
		panic("signature ended improperly")
	}
	var returnTy Type
	if p.nextTok.tt == TT_COLON {
		p.advance()
		p.advance()
		returnTy = p.nextType()
	}
	return Signature{PositionVector{start, p.currTok.pos.end}, classIdent, ident, params, returnTy}
}

func (p *Parser) nextParameter() Parameter {
	spec := p.nextSpecification()
	var dfl Expr
	if p.nextTok.tt == TT_IS {
		p.advance()
		p.advance()
		dfl = p.nextExpr(0)
	}
	return Parameter{*spec, dfl}
}

// TODO: "Lift" (provide a different set of ops) for parsing complicated Type (by LL(1) Pratt logic)
// TODO: probably with a stack: stack.push(new_ops) and parse according to stack[last] then stack.pop() on signal/ctx_switch message
// Consideration: How much semantics is good in Type(s)?
func (p *Parser) nextType() Type {
	switch p.currTok.tt {
	case TT_IDENT:
		return IdentType{p.currTok}
	case TT_STAR:
		start := p.pos
		p.advance()
		inner := p.nextType()
		return PointerType{
			PositionVector{
				start,
				inner.end(),
			},
			inner,
		}
	default:
		return nil
	}
}

func (p *Parser) nextSpecification() *Specification {
	ident := Identifier{p.currTok}
	p.advance()
	if p.currTok.tt != TT_COLON {
		panic("ident ty must be separated by colon")
	}
	p.advance()
	ty := p.nextType()
	p.advance()
	return &Specification{
		ident,
		ty,
	}
}

func (p *Parser) nextExpr(minBindPower int) Expr {
	lhs := p.nextPrefix()
	if lhs == nil {
		return nil
	}
	for {
		if p.nextTok.tt == TT_EOF {
			break
		}
		if leftBindPower, ok := suffixBindPowers[p.nextTok.tt]; ok {
			if leftBindPower < minBindPower {
				break
			}
			p.advance()
			lhs = p.nextSuffix(lhs, p.currTok)
			continue
		}
		if leftBindPower, ok := leftBindPowers[p.nextTok.tt]; ok {
			if rightBindPower, ok := rightBindPowers[p.nextTok.tt]; ok {
				if leftBindPower < minBindPower {
					break
				}
				p.advance()
				lhs = p.nextInfix(lhs, rightBindPower, p.currTok)
				continue
			}
			panic("invalid state (no rightBP for leftBP)")
		}
		break
	}
	return lhs
}

func (p *Parser) nextPrefix() Expr {
	if _, ok := prefixOps[p.currTok.tt]; ok {
		currTok := p.currTok
		rightBindPower := prefixBindPowers[p.currTok.tt]
		p.advance()
		rhs := p.nextExpr(rightBindPower)
		return Prefix{currTok, rhs}
	}
	if _, ok := groupOps[p.currTok.tt]; ok {
		p.advance()
		inner := p.nextExpr(0)
		if inner == nil {
			inner = Unit{}
		} else {
			p.advance()
		}
		return inner
	}
	return p.nextLit()
}

func (p *Parser) nextInfix(lhs Expr, rightBindPower int, opTok Token) Infix {
	p.advance()
	rhs := p.nextExpr(rightBindPower)
	return Infix{PositionVector{lhs.start(), rhs.end()}, lhs, opTok, rhs}
}

func (p *Parser) nextSuffix(lhs Expr, opTok Token) Expr {
	if v, ok := suffixClosedOps[opTok.tt]; ok {
		p.advance()
		rhs := p.nextExpr(0)
		if rhs != nil {
			p.advance()
		}
		if p.currTok.tt != v {
			panic("improperly closed")
		}
		return Infix{PositionVector{lhs.start(), p.currTok.pos.end}, lhs, opTok, rhs}
	}
	if _, ok := suffixUnclosedOps[opTok.tt]; ok {
		return Suffix{lhs, opTok}
	}
	return nil
}

func (p *Parser) nextLit() Expr {
	switch p.currTok.tt {
	case TT_STRING:
		return String{p.currTok}
	case TT_BYTE:
		return Character{p.currTok}
	case TT_INT:
		return Integer{p.currTok}
	case TT_FLOAT:
		return Float{p.currTok}
	case TT_IDENT:
		return Identifier{p.currTok}
	case TT_LBRACE:
		return p.NextBlock()
	case TT_INVALID:
		return Err{p.currTok}
	default:
		return nil
	}
}

func (p *Parser) advance() {
	p.currTok = p.nextTok
	p.nextTok = p.nextToken()
}

func (p *Parser) nextToken() Token {
	for {
		if p.pos >= len(p.src) {
			break
		}
		switch p.currChar() {
		case '\t', ' ', '\b':
			p.pos++
			continue
		case '\n':
			p.pos++
			p.col = 1
			p.line++
			continue
		case '\r':
			p.pos++
			if p.currChar() == '\n' {
				p.pos++
			}
			p.col = 1
			p.line++
			continue
		case '"':
			return p.nextString()
		case '\'':
			return p.nextByte()
		}
		switch {
		case p.currChar() == '_' || (p.currChar() >= 'a' && p.currChar() <= 'z') || (p.currChar() >= 'A' && p.currChar() <= 'Z'):
			return p.nextIdent()
		case (p.currChar() == '.' && p.peekChar() >= '0' && p.peekChar() <= '9') || (p.currChar() >= '0' && p.currChar() <= '9'):
			return p.nextNumber()
		default:
			return p.nextOp()
		}
	}
	return Token{
		tt: TT_EOF,
	}
}

func (p *Parser) peekChar() uint8 {
	if p.pos+1 < len(p.src) {
		return p.src[p.pos+1]
	}
	return 0
}

func (p *Parser) currChar() uint8 {
	return p.src[p.pos]
}

func (p *Parser) nextString() Token {
	start := p.pos
	p.pos++ // skip initial "
	for {
		v := p.peekChar()
		if v != 0 && v != '"' {
			p.pos++
		} else {
			break
		}
	}
	var tt TokenType
	if p.peekChar() == 0 {
		p.pos++
		tt = TT_INVALID
	} else {
		p.pos += 2
		// p.pos += 2 // consume current and "
		tt = TT_STRING
	}
	return Token{
		tt,
		PositionVector{start, p.pos},
	}
}

func (p *Parser) nextByte() Token {
	start := p.pos
	tt := TT_INVALID
	if p.peekChar() != '\'' {
		tt = TT_BYTE
	}
	p.pos++
	if p.peekChar() != '\'' {
		tt = TT_INVALID
	}
	p.pos += 2
	return Token{tt, PositionVector{start, p.pos}}
}

func (p *Parser) nextIdent() Token {
	start := p.pos
	for {
		if v := p.peekChar(); v == '_' || (v >= 'a' && v <= 'z') || (v >= 'A' && v <= 'Z') || (v >= '0' && v <= '9') {
			p.pos++
		} else {
			break
		}
	}
	p.pos++ // ending
	var tt TokenType
	if tty, ok := keywords[p.src[start:p.pos]]; ok {
		tt = tty
	} else {
		tt = TT_IDENT
	}
	return Token{
		tt,
		PositionVector{start, p.pos},
	}
}

func (p *Parser) nextNumber() Token {
	start := p.pos
	var radix uint8 = 'd'
	if p.src[p.pos] == '0' {
		if r := p.peekChar(); r == 'b' || r == 'o' || r == 'x' {
			p.pos++
			radix = r
		}
	}
	float := false
	invalid := false
	for {
		switch {
		case p.peekChar() == 0:
			goto end
		case p.peekChar() == '.':
			if float {
				invalid = true
			} else {
				float = true
			}
			p.pos++
		case radix == 'd' && p.peekChar() >= '0' && p.peekChar() <= '9':
			p.pos++
		case radix == 'b' && p.peekChar() >= '0' && p.peekChar() <= '1':
			p.pos++
		case radix == 'o' && p.peekChar() >= '0' && p.peekChar() <= '7':
			p.pos++
		case radix == 'x' && (p.peekChar() >= '0' && p.peekChar() <= '9' || p.peekChar() >= 'a' && p.peekChar() <= 'f' || p.peekChar() >= 'A' && p.peekChar() <= 'F'):
			p.pos++
		default:
			goto end
		}
	}
end:
	var tt TokenType
	switch {
	case invalid:
		tt = TT_INVALID
	case float:
		tt = TT_FLOAT
	default:
		tt = TT_INT
	}
	p.pos++
	return Token{
		tt,
		PositionVector{start, p.pos},
	}
}

func (p *Parser) nextOp() Token {
	start := p.pos
	tt := TT_INVALID
	stack := ""
	for {
		if p.pos >= len(p.src) {
			break
		}
		// 2: max op len
		if len(stack) > 2-1 {
			break
		}
		char := p.src[p.pos]
		stack += string(char)
		found := false
		for k, v := range ops {
			if strings.HasPrefix(k, stack) {
				tt = v[len(stack)-1]
				found = true
				break
			}
		}
		if !found {
			break
		}
		p.pos++
	}
	return Token{
		tt,
		PositionVector{start, p.pos},
	}
}
