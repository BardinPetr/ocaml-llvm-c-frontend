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

type unary_op =
  | UOPreMinus
  | UOPreInv
  | UOPreNeg
  | UOPreInc
  | UOPreDec
  | Deref
  | Addr
[@@deriving show]

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | And
  | Or
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
[@@deriving show]

type expr =
  | ExLiteral of literal
  | ExId of string
  | Var of string
  | UnaryOp of unary_op * expr
  | BinaryOp of binary_op * expr * expr
  | Cast of c_type * expr
  | Index of expr * expr
  | Member of expr * string
  | Call of string * expr list
  | Sizeof of c_type
[@@deriving show]

type stmt =
  | StExpr of expr
  | StIf of expr * stmt * stmt option
  | StFor of stmt option * expr option * stmt option * stmt
  | StWhile of expr * stmt
  | StDo of stmt * expr
  | StSwitch of expr * (int * stmt) list * stmt option
  | StBreak
  | StContinue
  | StReturn of expr option
[@@deriving show]

type func_decl =
  | FuncDecl of c_type * string * (c_type * string) list * stmt list
[@@deriving show]

type global_decl =
  | VarDecl of c_type * string
  | FuncProto of c_type * string * (c_type * string) list
  | StructDecl of string * (c_type * string) list
[@@deriving show]

type program = Prog of global_decl list * func_decl list [@@deriving show]
