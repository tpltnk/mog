package main

import "fmt"

type Position interface {
	Start() int
	End() int
}

type Block struct {
	stmts  []Stmt
	parent *Block
}

func (b *Block) String() string {
	return fmt.Sprintf("%v", b.stmts)
}

type Stmt interface {
}

type Expr interface {
}

type Err struct {
}

type Prefix struct {
	op  TokenType
	rhs Expr
}

type Infix struct {
	lhs Expr
	op  TokenType
	rhs Expr
}

type Suffix struct {
	lhs Expr
	op  TokenType
}

type Null struct {
}

type String struct {
}

type Int struct {
}

type Float struct {
}

type Ident struct {
}

type Return struct {
	rhs Expr
}

type Break struct {
	rhs Expr
}

type Continue struct {
}

type Yield struct {
	rhs Expr
}

type Assert struct {
	rhs Expr
}

type Import struct {
	rhs Expr
}

type For struct {
	ident *Ident
	ty    *Ty
	src   Expr
	body  *Block
}

type If struct {
	cond Expr
	main *Block
	alt  *Block
}

type Param struct {
	ident *Ident
	ty    *Ty
	dfl   Expr
}

type Fun struct {
	ident Ident
	sig   Signature
	body  *Block
}

type Signature struct {
	params   []Param
	returnTy Ty
}

type Class struct {
	ident  Ident
	fields []Param
}

type Ty interface {
}

type Type struct {
}

type Pointer struct {
	inner Ty
}
