%{
  open C_syntax
%}

%token <string> IDENTIFIER

%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <char> LIT_CHAR
%token <string> LIT_STRING

%token BRC_L BRC_R BRK_L BRK_R PRH_L PRH_R
%token PU_SEMICOLON PU_COLON PU_QUEST PU_COMMA

%token OP_RIGHT OP_LEFT OP_ADD OP_SUB OP_STAR OP_DIV OP_MOD OP_AND OP_XOR OP_OR OP_LAND OP_LOR OP_ARROW OP_INC OP_DEC OP_DOT OP_QUEST
%token UOP_NEG UOP_INV
%token ASN_BASE ASN_RIGHT ASN_LEFT ASN_ADD ASN_SUB ASN_MUL ASN_DIV ASN_MOD ASN_AND ASN_XOR ASN_OR
%token CMP_LE CMP_LT CMP_GE CMP_GT CMP_EQ CMP_NE

%token TYP_CHAR TYP_SHORT TYP_INT TYP_LONG TYP_FLOAT TYP_DOUBLE TYP_VOID

%token CW_STRUCT CW_SIZEOF
%token CW_BREAK CW_CASE CW_CONTINUE CW_DEFAULT CW_DO CW_ELSE CW_FLOAT CW_FOR CW_GOTO CW_IF CW_RETURN CW_SWITCH CW_WHILE

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
  | LIT_INT { IntLit $1 }
  | LIT_FLOAT { FloatLit $1 }
  | LIT_CHAR { CharLit $1 }
  | LIT_STRING { StringLit $1 }

binop:
  | OP_ADD { Add }
  | OP_SUB { Sub }
  | OP_STAR { Mul }
  | OP_DIV { Div }
