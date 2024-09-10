package main

import "fmt"

type CSTPrinter struct {
	root       *Block
	src        string
	blockDepth int
}

func NewCSTPrinter(root *Block, src string) CSTPrinter {
	return CSTPrinter{
		root,
		src,
		-1,
	}
}

func (ap *CSTPrinter) PrintRoot() {
	ap.PrintBlock(ap.root)
}

func (ap *CSTPrinter) PrintBlock(block *Block) {
	ap.blockDepth++
	for _, stmt := range block.stmts {
		fmt.Printf("%*s", ap.blockDepth*2, "")
		ap.printStatement(stmt)
	}
	ap.blockDepth--
}

func (ap *CSTPrinter) printStatement(stmt Statement) {
	switch stmt.(type) {
	case *Block:
		ap.PrintBlock(stmt.(*Block))
	case Function:
		ap.printFunction(stmt.(Function))
	case For:
		ap.printFor(stmt.(For))
	case If:
	default:
		fmt.Printf("%s\n", ap.src[stmt.start():stmt.end()])
	}
}

func (ap *CSTPrinter) printFunction(fun Function) {
	fmt.Printf("%s\n", ap.src[fun.pos.start:fun.sig.pos.end])
	ap.PrintBlock(fun.body)
}

func (ap *CSTPrinter) printFor(f For) {
	fmt.Printf("%s\n", ap.src[f.pos.start:f.src.end()])
	ap.PrintBlock(f.body)
}

type Position interface {
	start() int
	end() int
}

type PositionVector struct {
	start int
	end   int
}

type Block struct {
	pos    PositionVector
	stmts  []Statement
	parent *Block
}

func (b Block) start() int {
	return b.pos.start
}

func (b Block) end() int {
	return b.pos.end
}

type Statement interface {
	Position
}

type Expr interface {
	Position
}

type Err struct {
	Token
}

func (e Err) start() int {
	return e.Token.pos.start
}

func (e Err) end() int {
	return e.Token.pos.end
}

type Prefix struct {
	op  Token
	rhs Expr
}

func (p Prefix) start() int {
	return p.op.pos.start
}

func (p Prefix) end() int {
	return p.rhs.end()
}

type Infix struct {
	pos PositionVector
	lhs Expr
	op  Token
	rhs Expr
}

func (i Infix) start() int {
	return i.lhs.start()
}

func (i Infix) end() int {
	return i.pos.end
}

type Suffix struct {
	lhs Expr
	op  Token
}

func (s Suffix) start() int {
	return s.lhs.start()
}

func (s Suffix) end() int {
	return s.op.pos.end
}

type Unit struct {
}

func (n Unit) start() int {
	return 0
}

func (n Unit) end() int {
	return 0
}

type String struct {
	Token
}

func (s String) start() int {
	return s.Token.pos.start
}

func (s String) end() int {
	return s.Token.pos.end
}

type Character struct {
	Token
}

func (b Character) start() int {
	return b.Token.pos.start
}

func (b Character) end() int {
	return b.Token.pos.end
}

type Integer struct {
	Token
}

func (i Integer) start() int {
	return i.Token.pos.start
}

func (i Integer) end() int {
	return i.Token.pos.end
}

type Float struct {
	Token
}

func (f Float) start() int {
	return f.Token.pos.start
}

func (f Float) end() int {
	return f.Token.pos.end
}

type Identifier struct {
	Token
}

func (i Identifier) start() int {
	return i.Token.pos.start
}

func (i Identifier) end() int {
	return i.Token.pos.end
}

type Return struct {
	pos PositionVector
	rhs Expr
}

func (r Return) start() int {
	return r.pos.start
}

func (r Return) end() int {
	return r.pos.end
}

type Break struct {
	pos PositionVector
	rhs Expr
}

func (b Break) start() int {
	return b.pos.start
}

func (b Break) end() int {
	return b.pos.end
}

type Continue struct {
	pos PositionVector
}

func (c Continue) start() int {
	return c.pos.start
}

func (c Continue) end() int {
	return c.pos.end
}

type Yield struct {
	pos PositionVector
	rhs Expr
}

func (y Yield) start() int {
	return y.pos.start
}

func (y Yield) end() int {
	return y.pos.end
}

type Assert struct {
	pos PositionVector
	rhs Expr
}

func (a Assert) start() int {
	return a.pos.start
}

func (a Assert) end() int {
	return a.pos.end
}

type Import struct {
	pos PositionVector
	rhs Expr
}

func (i Import) start() int {
	return i.pos.start
}

func (i Import) end() int {
	return i.pos.end
}

type For struct {
	pos  PositionVector
	spec *Specification
	src  Expr
	body *Block
}

func (f For) start() int {
	return f.pos.start
}

func (f For) end() int {
	return f.pos.end
}

type If struct {
	pos  PositionVector
	cond Expr
	main *Block
	alt  *Block
}

func (i If) start() int {
	return i.pos.start
}

func (i If) end() int {
	return i.pos.end
}

type Parameter struct {
	spec Specification
	dfl  Expr
}

func (p Parameter) start() int {
	return p.spec.start()
}

func (p Parameter) end() int {
	if p.dfl != nil {
		return p.dfl.end()
	}
	return p.spec.end()
}

type Function struct {
	pos  PositionVector
	sig  Signature
	body *Block
}

func (f Function) start() int {
	return f.pos.start
}

func (f Function) end() int {
	return f.pos.end
}

type Signature struct {
	pos        PositionVector
	classIdent *Identifier
	ident      *Identifier
	params     []Parameter
	returnTy   Type
}

type Class struct {
	pos    PositionVector
	ident  Identifier
	fields []Parameter
}

func (c Class) start() int {
	return c.pos.start
}

func (c Class) end() int {
	return c.pos.end
}

type Specification struct {
	ident Identifier
	ty    Type
}

func (s Specification) start() int {
	return s.ident.pos.start
}

func (s Specification) end() int {
	return s.ty.end()
}

type Type interface {
	Position
}

type IdentType Identifier

func (t IdentType) start() int {
	return t.Token.pos.start
}

func (t IdentType) end() int {
	return t.Token.pos.end
}

type PointerType struct {
	pos     PositionVector
	pointed Type
}

func (p PointerType) start() int {
	return p.pos.start
}

func (p PointerType) end() int {
	return p.pos.end
}
