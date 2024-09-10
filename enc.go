package main

type Module struct {
	sections []Section
}

func (m Module) Encode() []uint8 {
	mod := []uint8{
		MAGIC0,
		MAGIC1,
		MAGIC2,
		MAGIC3,
		VERSION0,
		VERSION1,
		VERSION2,
		VERSION3,
	}
	for _, section := range m.sections {
		secId := SEC_UNDEFINED
		var name []uint8
		switch section.(type) {
		case CustomSection:
			name = section.(CustomSection).Name()
			secId = SEC_CUSTOM
		case TypeSection:
			secId = SEC_TYPE
		case ImportSection:
			secId = SEC_IMPORT
		case FunctionSection:
			secId = SEC_FUNCTION
		case MemorySection:
			secId = SEC_MEMORY
		case TableSection:
			secId = SEC_TABLE
		case GlobalSection:
			secId = SEC_GLOBAL
		case ExportSection:
			secId = SEC_EXPORT
		case StartSection:
			secId = SEC_START
		case ElementSection:
			secId = SEC_ELEMENT
		case CodeSection:
			secId = SEC_CODE
		case DataSection:
			secId = SEC_DATA
		case DataCountSection:
			secId = SEC_DATACOUNT
		}
		if secId == SEC_UNDEFINED {
			panic("invalid section")
		}
		mod = append(mod, secId)
		mod = append(mod, name...)
		mod = append(mod, section.Encode()...)
	}
	return mod
}

type Section interface {
	Encodable
}

type CustomSection interface {
	Section
	Name() []uint8
}

type TypeSection = Vec[FunctionType]
type ImportSection = Vec[ImportType]
type FunctionSection = Vec[TypeIdIndex]
type TableSection = Vec[TableType]
type GlobalSection = Vec[Global]
type MemorySection = Vec[MemType]
type ExportSection = Vec[ExportType]
type StartSection = FunctionIdIndex
type ElementSection = Vec[Element]
type CodeSection = Vec[Code]
type DataSection = Vec[Data]
type DataCountSection = UI32
