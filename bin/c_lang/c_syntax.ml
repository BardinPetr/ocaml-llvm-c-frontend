type c_type =
  | TVoid
  | TChar
  | TShort
  | TInt
  | TLong
  | TFloat
  | TDouble
  | TPtr of c_type
  | TArray of c_type * int
  | TStruct of string
  | TOther of string
[@@deriving show]

type literal =
  | IntLit of int
  | FloatLit of float
  | CharLit of char
  | StringLit of string
[@@deriving show]

type unary_op = UOMinus | UOInv | UONeg | UOInc | UODec | UODeref | UORef
[@@deriving show]

type binary_op =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpAnd
  | OpXor
  | OpOr
  | OpLAnd
  | OpLOr
  | OpRight
  | OpLeft
  | OpCmpLe
  | OpCmpLt
  | OpCmpGe
  | OpCmpGt
  | OpCmpEq
  | OpCmpNe
[@@deriving show]

type asn_op =
  | AsnBase
  | AsnRight
  | AsnLeft
  | AsnAdd
  | AsnSub
  | AsnMul
  | AsnDiv
  | AsnMod
  | AsnAnd
  | AsnXor
  | AsnOr
[@@deriving show]

type expr =
  | ExLiteral of literal
  | ExId of string
  | ExUOpPre of unary_op * expr
  | ExUOpPost of unary_op * expr
  | ExBOp of binary_op * expr * expr
  | ExAOp of asn_op * expr * expr
  | ExCast of c_type * expr
  | ExSizeof of c_type
  | ExTernary of expr * expr * expr
  | ExArrIdx of expr * expr
  | ExAccess of expr * string
  | ExPAccess of expr * string
  | ExCall of string * expr list
[@@deriving show]

type decl =
  | DeclVar of c_type * string * expr option
  | FuncProto of c_type * string * (c_type * string) list
  | StructDecl of string * (c_type * string) list
[@@deriving show]

type stmt =
  | StExpr of expr
  | StIf of expr * stmt list * stmt list option
  | StFor of expr option * expr option * expr option * stmt list
  | StWhile of expr * stmt list
  | StBreak
  | StContinue
  | StReturn of expr option
  | StDecl of decl
[@@deriving show]

type func_decl =
  | FuncDecl of c_type * string * (c_type * string) list * stmt list
[@@deriving show]

type program = Prog of decl list * func_decl list [@@deriving show]

let rec wrap_ctype_array (dims : int list) (typ : c_type) : c_type =
  match dims with
  | [] -> typ
  | hd :: tl -> wrap_ctype_array tl (TArray (typ, hd))
