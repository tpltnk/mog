package main

type Encodable interface {
	Encode() []uint8
}

type Vec[T Encodable] struct {
	elements []T
}

func (ts Vec[T]) Encode() []uint8 {
	vec := UI32(len(ts.elements)).Encode()
	for _, element := range ts.elements {
		vec = append(vec, element.Encode()...)
	}
	return vec
}

type UI32 uint32

func (u UI32) Encode() []uint8 {
	return UI64LEB([]uint8{}, uint64(u))
}

type SI32 int32

func (s SI32) Encode() []uint8 {
	return SI64LEB([]uint8{}, int64(s))
}

type Byte uint8

func (b Byte) Encode() []uint8 {
	return []uint8{uint8(b)}
}

type Name Vec[Byte]

// Encode TODO: check for utf8 validity
func (n Name) Encode() []uint8 {
	return (Vec[Byte])(n).Encode()
}

type NumType uint8

func (nt NumType) Encode() []uint8 {
	return []uint8{uint8(nt)}
}

type VecType uint8

func (vt VecType) Encode() []uint8 {
	return []uint8{uint8(vt)}
}

type RefType uint8

func (rt RefType) Encode() []uint8 {
	return []uint8{uint8(rt)}
}

type ValType uint8

func (vt ValType) Encode() []uint8 {
	return []uint8{uint8(vt)}
}

type ResultType Vec[ValType]

func (rt ResultType) Encode() []uint8 {
	return (Vec[ValType])(rt).Encode()
}

type FunctionType struct {
	params  Vec[ResultType]
	results Vec[ResultType]
}

func (ft FunctionType) Encode() []uint8 {
	return append([]uint8{TY_FUN}, append(ft.params.Encode(), ft.results.Encode()...)...)
}

type Limits struct {
	lower UI32
	upper *UI32
}

func (l Limits) Encode() []uint8 {
	limStart := LIMIT_MINONLY
	var upper []uint8
	if l.upper != nil {
		limStart = LIMIT_MINMAX
		upper = l.upper.Encode()
	}
	return append([]uint8{limStart}, append(l.lower.Encode(), upper...)...)
}

type TableType struct {
	ref RefType
	lim Limits
}

func (tt TableType) Encode() []uint8 {
	return append(tt.ref.Encode(), tt.lim.Encode()...)
}

func (tt TableType) IDT() {}

type MemType struct {
	lim Limits
}

func (mt MemType) Encode() []uint8 {
	return mt.lim.Encode()
}

func (mt MemType) IDT() {}

type GlobalType struct {
	typ ValType
	mut uint8
}

func (gt GlobalType) Encode() []uint8 {
	return append(gt.typ.Encode(), gt.mut)
}

func (gt GlobalType) IDT() {}

type Instruction uint8

func (i Instruction) Encode() []uint8 {
	return []uint8{uint8(i)}
}

type Expression struct {
	instructions []Instruction
}

func (e Expression) Encode() []uint8 {
	var et []uint8
	for _, instr := range e.instructions {
		et = append(et, instr.Encode()...)
	}
	return append(et, OP_CTRL_END)
}

type Global struct {
	ty   GlobalType
	init Expression
}

func (g Global) Encode() []uint8 {
	return append(g.ty.Encode(), g.init.Encode()...)
}

type FunctionIdIndex UI32

func (fii FunctionIdIndex) Encode() []uint8 {
	return UI32(fii).Encode()
}

func (fii FunctionIdIndex) EDT() {}

type MemIdIndex UI32

func (mii MemIdIndex) Encode() []uint8 {
	return UI32(mii).Encode()
}

func (mii MemIdIndex) EDT() {}

type TableIdIndex UI32

func (tii TableIdIndex) Encode() []uint8 {
	return UI32(tii).Encode()
}

func (tii TableIdIndex) EDT() {}

type GlobalIdIndex UI32

func (gii GlobalIdIndex) Encode() []uint8 {
	return UI32(gii).Encode()
}

func (gii GlobalIdIndex) EDT() {}

type TypeIdIndex UI32

func (tii TypeIdIndex) Encode() []uint8 {
	return UI32(tii).Encode()
}

func (tii TypeIdIndex) IDT() {}

type ImportType struct {
	mod   Name
	alias Name
	desc  ImportDescription
}

func (it ImportType) Encode() []uint8 {
	return append(it.mod.Encode(), append(it.alias.Encode(), it.desc.Encode()...)...)
}

type ImportDescriptor uint8

func (id ImportDescriptor) Encode() []uint8 {
	return []uint8{uint8(id)}
}

type ImportDescriptorType interface {
	Encodable
	IDT()
}

type ImportDescription struct {
	descriptor     ImportDescriptor
	descriptorType ImportDescriptorType
}

func (id ImportDescription) Encode() []uint8 {
	return append(id.descriptor.Encode(), id.descriptorType.Encode()...)
}

type ExportType struct {
	alias Name
	desc  ExportDescription
}

func (et ExportType) Encode() []uint8 {
	return append(et.alias.Encode(), et.desc.Encode()...)
}

type ExportDescriptor uint8

func (ed ExportDescriptor) Encode() []uint8 {
	return []uint8{uint8(ed)}
}

type ExportDescriptorType interface {
	Encodable
	EDT()
}

type ExportDescription struct {
	descriptor     ExportDescriptor
	descriptorType ExportDescriptorType
}

func (ed ExportDescription) Encode() []uint8 {
	return append(ed.descriptor.Encode(), ed.descriptorType.Encode()...)
}

type Local struct {
	repetitions UI32
	value       ValType
}

func (l Local) Encode() []uint8 {
	return append(l.repetitions.Encode(), l.value.Encode()...)
}

// Func TODO: max size is 2^32 bytes
type Func struct {
	locals Vec[Local]
	expr   Expression
}

func (f Func) Encode() []uint8 {
	return append(f.locals.Encode(), f.expr.Encode()...)
}

type Code struct {
	size UI32
	code Func
}

func (c Code) Encode() []uint8 {
	return append(c.size.Encode(), c.code.Encode()...)
}

type DataDescriptor UI32

// bit 0 = passive
const (
	DataSegmentActive DataDescriptor = iota
	DataSegmentPassive
	DataSegmentActiveWithMemoryIdIndex
)

func (d DataDescriptor) Encode() []uint8 {
	return UI32(d).Encode()
}

type DataSegment interface {
	Encodable
	DS()
}

type PassiveDataSegment Vec[Byte]

func (pds PassiveDataSegment) DS() {}

func (pds PassiveDataSegment) Encode() []uint8 {
	return (Vec[Byte])(pds).Encode()
}

type ActiveDataSegment struct {
	memIdIndex *MemIdIndex
	offset     Expression
	bytes      Vec[Byte]
}

func (ads ActiveDataSegment) DS() {}

func (ads ActiveDataSegment) Encode() []uint8 {
	var memId []uint8
	if ads.memIdIndex != nil {
		memId = ads.memIdIndex.Encode()
	}
	return append(memId, append(ads.offset.Encode(), ads.bytes.Encode()...)...)
}

type Data struct {
	descriptor DataDescriptor
	segments   Vec[DataSegment]
}

func (d Data) Encode() []uint8 {
	return append(d.descriptor.Encode(), d.segments.Encode()...)
}

type ElementDescriptor UI32

func (ed ElementDescriptor) Encode() []uint8 {
	return UI32(ed).Encode()
}

// bit 0 = passive or declarative
// bit 1 = table index present
// bit 2 = element expression present
const (
	ElementActiveFunctionRef ElementDescriptor = iota
	ElementPassiveFunctionRef
	ElementActiveTableIdIndex
	ElementDeclarativeFunctionRef
	ElementActiveFunctionRefExpression
	ElementPassiveRefTypeExpression
	ElementActiveRefTypeTableExpression
	ElementDeclarativeRefTypeExpression
)

type ActiveFunctionRef struct {
	offset Expression
	init   Vec[FunctionIdIndex]
}

func (afr ActiveFunctionRef) Encode() []uint8 {
	afrb := ElementActiveFunctionRef.Encode()
	afrb = append(afrb, afr.offset.Encode()...)
	afrb = append(afrb, afr.init.Encode()...)
	return afrb
}

func (afr ActiveFunctionRef) ET() {}

type ElementSegmentPassiveFunctionRef struct {
	init Vec[FunctionIdIndex]
}

func (pfr ElementSegmentPassiveFunctionRef) Encode() []uint8 {
	pfrb := ElementPassiveFunctionRef.Encode()
	pfrb = append(pfrb, 0x00)
	pfrb = append(pfrb, pfr.init.Encode()...)
	return pfrb
}

func (pfr ElementSegmentPassiveFunctionRef) ET() {}

type ElementSegmentActiveTableIdIndex struct {
	tab    TableIdIndex
	offset Expression
	init   Vec[FunctionIdIndex]
}

func (atii ElementSegmentActiveTableIdIndex) Encode() []uint8 {
	attib := ElementActiveTableIdIndex.Encode()
	attib = append(attib, atii.tab.Encode()...)
	attib = append(attib, atii.offset.Encode()...)
	attib = append(attib, 0x00)
	attib = append(attib, atii.init.Encode()...)
	return attib
}

func (atti ElementSegmentActiveTableIdIndex) ET() {}

type ElementSegmentDeclarativeFunctionRef struct {
	init Vec[FunctionIdIndex]
}

func (dfr ElementSegmentDeclarativeFunctionRef) Encode() []uint8 {
	return append(ElementDeclarativeFunctionRef.Encode(), dfr.init.Encode()...)
}

func (dfr ElementSegmentDeclarativeFunctionRef) ET() {
}

type ElementSegmentActiveFunctionRefExpression struct {
	offset Expression
	init   Vec[Expression]
}

func (afre ElementSegmentActiveFunctionRefExpression) Encode() []uint8 {
	afreb := ElementActiveFunctionRefExpression.Encode()
	afreb = append(afreb, afre.offset.Encode()...)
	afreb = append(afreb, afre.init.Encode()...)
	return afreb
}

func (afre ElementSegmentActiveFunctionRefExpression) ET() {
}

type ElementSegmentPassiveRefTypeExpression struct {
	ref  RefType
	init Vec[Expression]
}

func (prte ElementSegmentPassiveRefTypeExpression) Encode() []uint8 {
	prteb := ElementPassiveRefTypeExpression.Encode()
	prteb = append(prteb, prte.ref.Encode()...)
	prteb = append(prteb, prte.init.Encode()...)
	return prteb
}

func (prte ElementSegmentPassiveRefTypeExpression) ET() {
}

type ElementSegmentActiveTableIdIndexExpression struct {
	tab    TableIdIndex
	offset Expression
	ref    RefType
	init   Vec[Expression]
}

func (atiie ElementSegmentActiveTableIdIndexExpression) Encode() []uint8 {
	atiieb := ElementActiveRefTypeTableExpression.Encode()
	atiieb = append(atiieb, atiie.tab.Encode()...)
	atiieb = append(atiieb, atiie.offset.Encode()...)
	atiieb = append(atiieb, atiie.ref.Encode()...)
	atiieb = append(atiieb, atiie.init.Encode()...)
	return atiieb
}

func (atiie ElementSegmentActiveTableIdIndexExpression) ET() {
}

type ElementSegmentRefTypeDeclarativeExpression struct {
	ref  RefType
	init Vec[Expression]
}

func (rtd ElementSegmentRefTypeDeclarativeExpression) Encode() []uint8 {
	rtdb := ElementDeclarativeRefTypeExpression.Encode()
	rtdb = append(rtdb, rtd.ref.Encode()...)
	rtdb = append(rtdb, rtd.init.Encode()...)
	return rtdb
}

func (rtd ElementSegmentRefTypeDeclarativeExpression) ET() {
}

type ElementType interface {
	Encodable
	ET()
}

type Element struct {
	descriptor  ElementDescriptor
	elementType ElementType
}

func (e Element) Encode() []uint8 {
	return append(e.descriptor.Encode(), e.elementType.Encode()...)
}
