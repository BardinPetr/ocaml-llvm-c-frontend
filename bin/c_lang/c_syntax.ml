type c_type =
  | Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Ptr of c_type
  | Array of c_type * int
  | Struct of string
[@@deriving show]

type literal =
  | IntLit of int
  | FloatLit of float
  | CharLit of char
  | StringLit of string
[@@deriving show]

type unary_op = Neg | Not | Deref | Addr [@@deriving show]

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
  | Literal of literal
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
  | ExprStmt of expr
  | If of expr * stmt * stmt option
  | For of stmt option * expr option * stmt option * stmt
  | While of expr * stmt
  | Do of stmt * expr
  | Switch of expr * (int * stmt) list * stmt option
  | Break
  | Continue
  | Return of expr option
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
