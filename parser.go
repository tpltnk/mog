package main

import (
	"fmt"
	"strings"
)

//go:generate stringer -type=TokenType
type TokenType int

const (
	TT_IDENT TokenType = iota
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
)

type Token struct {
	tt    TokenType
	start int
	end   int
}

type Parser struct {
	src string

	pos  int
	col  int
	line int

	currTok Token
	nextTok Token

	currBlk *Block
}

func NewParser(src string) Parser {
	p := Parser{
		src:  src,
		pos:  0,
		col:  1,
		line: 1,
	}
	p.advance()
	return p
}

func (p *Parser) nextBlock() *Block {
	p.currBlk = &Block{
		stmts:  make([]Stmt, 0),
		parent: p.currBlk,
	}
	for {
		stmt := p.nextStmt()
		if stmt == nil {
			break
		}
		p.currBlk.stmts = append(p.currBlk.stmts, stmt)
	}
	blk := p.currBlk
	p.currBlk = p.currBlk.parent
	return blk
}

func (p *Parser) nextStmt() Stmt {
	if p.nextTok.tt == TT_EOF {
		return nil
	}
	p.advance()
	switch p.currTok.tt {
	case TT_RBRACE:
		return nil
	case TT_KW_RETURN:
		return p.nextReturn()
	case TT_KW_BREAK:
		return p.nextBreak()
	case TT_KW_CONTINUE:
		return p.nextBreak()
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
		return p.nextFun()
	case TT_KW_CLASS:
		return p.nextClass()
	case TT_EOF:
		return nil
	default:
		return p.nextExpr(0)
	}
}

func (p *Parser) nextReturn() Stmt {
	p.advance()
	rhs := p.nextExpr(0)
	return Return{rhs}
}

func (p *Parser) nextBreak() Stmt {
	p.advance()
	rhs := p.nextExpr(0)
	return Break{rhs}
}

func (p *Parser) nextContinue() Stmt {
	p.advance()
	return Continue{}
}

func (p *Parser) nextYield() Stmt {
	p.advance()
	rhs := p.nextExpr(0)
	return Yield{rhs}
}

func (p *Parser) nextAssert() Stmt {
	p.advance()
	rhs := p.nextExpr(0)
	return Assert{rhs}
}

func (p *Parser) nextImport() Stmt {
	p.advance()
	rhs := p.nextExpr(0)
	return Import{rhs}
}

func (p *Parser) nextClass() Class {
	p.advance()
	name := Ident{}
	p.advance()
	p.advance()
	params := make([]Param, 0)
	for {
		if p.nextTok.tt == TT_EOF {
			break
		}
		switch p.nextTok.tt {
		case TT_COMMA:
			p.advance()
			continue
		case TT_RBRACE:
			p.advance()
			goto out
		case TT_IDENT:
			p.advance()
			params = append(params, p.nextParam())
			continue
		default:
			goto out
		}
	}
out:
	return Class{name, params}
}

func (p *Parser) nextIf() Expr {
	p.advance()
	cond := p.nextExpr(0)
	if cond == nil {
		panic("condition must be set")
	}
	p.advance()
	main := p.nextBlock()
	var alt *Block
	if p.nextTok.tt == TT_KW_ELSE {
		p.advance() // else
		p.advance() // {
		alt = p.nextBlock()
	}
	return If{cond, main, alt}
}

func (p *Parser) nextFor() Stmt {
	p.advance()
	var ident *Ident
	var ty *Ty
	if p.currTok.tt != TT_LBRACE {
		ident, ty = p.nextIdentTy()
	}
	var src Expr
	if ident != nil && ty != nil {
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
	body := p.nextBlock()
	return For{ident, ty, src, body}
}

func (p *Parser) nextFun() Fun {
	var ident Ident
	if p.nextTok.tt == TT_IDENT {
		p.advance()
		ident = Ident{}
	}
	p.advance()
	sig := p.nextSignature()
	p.advance()
	body := p.nextBlock()
	return Fun{ident, sig, body}
}

func (p *Parser) nextSignature() Signature {
	if p.currTok.tt != TT_LPAREN {
		panic("signature must start with (")
	}
	params := make([]Param, 0)
	for {
		if p.nextTok.tt == TT_EOF {
			break
		}
		switch p.nextTok.tt {
		case TT_COMMA:
			p.advance()
			continue
		case TT_RPAREN:
			p.advance()
			goto out
		case TT_IDENT:
			p.advance()
			params = append(params, p.nextParam())
			continue
		default:
			goto out
		}
	}
out:
	if p.currTok.tt != TT_RPAREN {
		panic("signature ended improperly")
	}
	var returnTy Ty
	if p.nextTok.tt == TT_COLON {
		p.advance()
		p.advance()
		returnTy = p.nextTy()
	}
	return Signature{params, returnTy}
}

func (p *Parser) nextParam() Param {
	ident, ty := p.nextIdentTy()
	var dfl Expr
	if p.nextTok.tt == TT_IS {
		p.advance()
		p.advance()
		dfl = p.nextExpr(0)
	}
	return Param{ident, ty, dfl}
}

func (p *Parser) nextTy() Ty {
	switch p.currTok.tt {
	case TT_IDENT:
		return Type{}
	case TT_STAR:
		p.advance()
		return Pointer{
			inner: p.nextTy(),
		}
	default:
		return nil
	}
}

func (p *Parser) nextIdentTy() (*Ident, *Ty) {
	ident := Ident{}
	p.advance()
	if p.currTok.tt != TT_COLON {
		panic("ident ty must be separated by colon")
	}
	p.advance()
	ty := p.nextTy()
	p.advance()
	return &ident, &ty
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
	for k := range prefixOps {
		if p.currTok.tt == k {
			currTok := p.currTok
			rightBindPower := prefixBindPowers[p.currTok.tt]
			p.advance()
			rhs := p.nextExpr(rightBindPower)
			return Prefix{currTok.tt, rhs}
		}
	}
	for k, _ := range groupOps {
		if p.currTok.tt == k {
			p.advance()
			inner := p.nextExpr(0)
			if inner == nil {
				return Null{}
			} else {
				p.advance()
			}
			return inner
		}
	}
	return p.nextLit()
}

func (p *Parser) nextInfix(lhs Expr, rightBindPower int, opTok Token) Infix {
	p.advance()
	rhs := p.nextExpr(rightBindPower)
	return Infix{lhs, opTok.tt, rhs}
}

func (p *Parser) nextSuffix(lhs Expr, opTok Token) Expr {
	for k, _ := range suffixClosedOps {
		if opTok.tt == k {
			p.advance()
			rhs := p.nextExpr(0)
			if rhs != nil {
				p.advance()
			}
			if p.currTok.tt != suffixClosedOps[opTok.tt] {
				panic("improperly closed")
			}
			return Infix{lhs, opTok.tt, rhs}
		}
	}
	for k, _ := range suffixUnclosedOps {
		if opTok.tt == k {
			return Suffix{lhs, opTok.tt}
		}
	}
	return nil
}

func (p *Parser) nextLit() Expr {
	switch p.currTok.tt {
	case TT_STRING:
		return String{}
	case TT_INT:
		return Int{}
	case TT_FLOAT:
		return Float{}
	case TT_IDENT:
		return Ident{}
	case TT_LBRACE:
		return p.nextBlock()
	case TT_INVALID:
		return Err{}
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
		char := p.src[p.pos]
		switch char {
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
			if p.peekChar() == '\n' {
				p.pos++
			}
			p.col = 1
			p.line++
			continue
		case '"':
			return p.nextString()
		}
		switch {
		case char == '_' || (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z'):
			return p.nextIdent()
		case char == '.' || (char >= '0' && char <= '9'):
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
		tt:    tt,
		start: start,
		end:   p.pos,
	}
}

func (p *Parser) nextIdent() Token {
	start := p.pos
	for {
		v := p.peekChar()
		if v == '_' || (v >= 'a' && v <= 'z') || (v >= '0' && v <= '9') {
			p.pos++
		} else {
			break
		}
	}
	p.pos++ // ending
	var tt TokenType
	if tokenType, ok := keywords[p.src[start:p.pos]]; ok {
		tt = tokenType
	} else {
		tt = TT_IDENT
	}
	return Token{
		tt:    tt,
		start: start,
		end:   p.pos,
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
		char := p.peekChar()
		if char == 0 {
			break
		}
		switch {
		case char == '.':
			if float {
				invalid = true
			} else {
				float = true
			}
			p.pos++
		case radix == 'd' && char >= '0' && char <= '9':
			p.pos++
		case radix == 'b' && char >= '0' && char <= '1':
			p.pos++
		case radix == 'o' && char >= '0' && char <= '7':
			p.pos++
		case radix == 'x' && (char >= '0' && char <= '9' || char >= 'a' && char <= 'f' || char >= 'A' && char <= 'F'):
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
		tt:    tt,
		start: start,
		end:   p.pos,
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
		// 1 = longest len(op) - 1
		if len(stack) > 1 {
			break
		}
		char := p.src[p.pos]
		stack = fmt.Sprintf("%s%c", stack, char)
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
		tt:    tt,
		start: start,
		end:   p.pos,
	}
}
