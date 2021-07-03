module AST

type Primitive =
    | Unit // or void for C minded people
    | Byte // 8 bits
    | Integer // 32 bits
    | Float // 32 bits
    | Double // 64 bits
    | GUID // 128 bits
    | String

// TODO correctly constrained value type eg string50, only alphabetical or _ first char etc
type Identifier = string

type QualifiedIdentifier = string list

type UnresolvedTypeRef =
    | Identifier of Identifier
    | QualifiedIdentifier of QualifiedIdentifier


type Collection =
    | List of FIDLType
    | Map of key:Primitive * value:FIDLType

and Choice = {
    Identifier: Identifier
    Cases: Map<Identifier,FIDLType option>
}

and Record = {
    Identifier: Identifier
    Fields: Map<Identifier, FIDLType>
}

and FIDLType =
    | PrimitiveType of Primitive
    | CollectionType of Collection
    | UnresolvedTypeRef of UnresolvedTypeRef

and TypeDecl =
    | RecordType of Record
    | ChoiceType of Choice


type FIDLFunction = {
    Identifier: Identifier
    ReturnType: FIDLType
    Inputs: (Identifier* FIDLType) list
}

type FIDLNamespace = {
    Identifier : QualifiedIdentifier
    Children: FIDL list
}

and FIDL =
    | TypeDecl of TypeDecl
    | FIDLFunction of FIDLFunction
    | FIDLNamespace of FIDLNamespace