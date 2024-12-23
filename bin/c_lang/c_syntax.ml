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
  | TEllipsis
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

type var_decl = DeclVar of c_type * string * expr option [@@deriving show]

type stmt =
  | StExpr of expr
  | StIf of expr * stmt * stmt option
  | StFor of expr option * expr option * expr option * stmt
  | StWhile of expr * stmt
  | StBreak
  | StContinue
  | StReturn of expr option
  | StDecl of var_decl
  | StBlock of stmt list
[@@deriving show]

type global_decl =
  | FuncGDecl of c_type * string * (c_type * string) list * stmt option
  | VarGDecl of var_decl
  | StructGDecl of string * (c_type * string) list
[@@deriving show]

type program = Prog of global_decl list [@@deriving show]

let rec wrap_ctype_array (dims : int list) (typ : c_type) : c_type =
  match dims with
  | [] -> typ
  | hd :: tl -> wrap_ctype_array tl (TArray (typ, hd))
