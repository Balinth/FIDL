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

module QualifiedIdentifier =
    let stringize (qualifiedIdentifier : QualifiedIdentifier) =
        System.String.Join(".", List.rev qualifiedIdentifier)
    let name (qualifiedIdentifier : QualifiedIdentifier) =
        List.last qualifiedIdentifier

type UnresolvedTypeRef =
    | Identifier of Identifier
    | QualifiedIdentifier of QualifiedIdentifier


type Collection =
    | List of FIDLType
    | Map of key:Primitive * value:FIDLType

and Choice = {
    Identifier: QualifiedIdentifier
    Cases: (Identifier * FIDLType option) list
}

and Record = {
    Identifier: QualifiedIdentifier
    Fields: (Identifier * FIDLType) list
}

and Function = {
    Identifier: QualifiedIdentifier
    ReturnType: FIDLType
    Parameters: (Identifier* FIDLType) list
}

and FIDLType =
    | PrimitiveType of Primitive
    | CollectionType of Collection
    | UnresolvedTypeRef of UnresolvedTypeRef

and TypeDecl =
    | RecordType of Record
    | ChoiceType of Choice
    | FunctionType of Function

type FIDLNamespace = {
    Identifier : QualifiedIdentifier
    Children: FIDL list
}

and FIDL =
    | TypeDecl of TypeDecl
    | FIDLNamespace of FIDLNamespace