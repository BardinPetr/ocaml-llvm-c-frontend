%{
  open C_syntax
%}

%token <string> IDENTIFIER

%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT

%token ADD_OP SUB_OP MUL_OP DIV_OP 
// %token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN 
// %token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP 
// %token TYPEDEF SIZEOF EXTERN STATIC REGISTER TYPE_NAME
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
// %token STRUCT UNION ENUM ELLIPSIS
// %token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token EOF

%start prog
%type <C_syntax.expr> prog

%%


prog:
  | expr binop expr { BinaryOp ($2, $1, $3) }
  // | stmt { $1 }

// stmt:
  // | expr { ExprStmt $1 }
  // | typ ID { Decl ($1, $2) }

expr:
  | lit { Literal $1 }
  // | ID { Var $1 }
  // | expr binop expr { BinaryOp ($2, $1, $3) }
  // | MINUS expr %prec UMINUS { Unop ("-", $2) }

lit:
  | INT_LIT { IntLit $1 }
  | FLOAT_LIT { FloatLit $1 }
  | CHAR_LIT { CharLit $1 }
  | STRING_LIT { StringLit $1 }

binop:
  | ADD_OP { Add }
  | SUB_OP { Sub }
  | MUL_OP { Mul }
  | DIV_OP { Div }
