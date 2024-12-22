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

%token OP_RIGHT OP_LEFT OP_PLUS OP_MINUS OP_STAR OP_DIV OP_MOD OP_AMPERSAND OP_XOR OP_OR OP_LAND OP_LOR OP_ARROW OP_DOT OP_QUEST
%token UOP_NEG UOP_INV UOP_INC UOP_DEC
%token ASN_BASE ASN_RIGHT ASN_LEFT ASN_ADD ASN_SUB ASN_MUL ASN_DIV ASN_MOD ASN_AND ASN_XOR ASN_OR
%token CMP_LE CMP_LT CMP_GE CMP_GT CMP_EQ CMP_NE

%token TYP_CHAR TYP_SHORT TYP_INT TYP_LONG TYP_FLOAT TYP_DOUBLE TYP_VOID

%token KW_STRUCT KW_SIZEOF
%token KW_BREAK KW_CASE KW_CONTINUE KW_DEFAULT KW_DO KW_ELSE KW_FLOAT KW_FOR KW_IF KW_RETURN KW_SWITCH KW_WHILE

%token EOF

%start prog
%type <C_syntax.stmt list> prog

%%

(* operators *)

op_unary:
	| OP_MINUS { UOPreMinus }
	| UOP_INV { UOPreInv }
	| UOP_NEG { UOPreNeg }
	| UOP_INC { UOPreInc }
	| UOP_DEC { UOPreDec } ;

// op_binary:
//   | OP_PLUS 
//   | OP_MINUS
//   | OP_STAR
//   | OP_DIV
//   | OP_MOD
//   | OP_AMPERSAND
//   | OP_XOR
//   | OP_OR
//   | OP_LAND
//   | OP_LOR
//   | OP_RIGHT
//   | OP_LEFT

// op_assign:
//   | ASN_BASE
//   | ASN_RIGHT
//   | ASN_LEFT
//   | ASN_ADD
//   | ASN_SUB
//   | ASN_MUL
//   | ASN_DIV
//   | ASN_MOD
//   | ASN_AND
//   | ASN_XOR
//   | ASN_OR

(* expressions *)

type_name:
  | v = IDENTIFIER { TOther v }
  | KW_STRUCT; v = IDENTIFIER { TStruct v } ;

literal:
  | LIT_INT { IntLit $1 }
  | LIT_FLOAT { FloatLit $1 }
  | LIT_CHAR { CharLit $1 }
  | LIT_STRING { StringLit $1 } ;

ex_primary:
	| PRH_L; e = expression; PRH_R { e }
  | v = IDENTIFIER { ExId v }
	| v = literal { ExLiteral v } ;

// ex_prefix:
//   | OP_AMPERSAND expression
//   | OP_STAR expression
//   | op_unary expression
//   | KW_SIZEOF expression
//   | KW_SIZEOF PRH_L type_name PRH_R
//   | PRH_L type_name PRH_R expression

// ex_postfix:
//   | BRK_L expression BRK_R (* arr *)
// 	| ex_postfix PRH_L PRH_R (* call *)
// 	| ex_postfix PRH_L ex_arg_list PRH_R
//   | expression UOP_INC
//   | expression UOP_DEC

// ex_arg_list: 
//   | expression
//   | ex_arg_list PU_COMMA expression 

// ex_binary:
//   | expression op_binary expression
//   | expression OP_DOT IDENTIFIER
//   | expression OP_ARROW IDENTIFIER

// ex_conditional:
// 	| expression PU_QUEST expression PU_COLON expression

// ex_assignment:
//   | expression op_assign expression

expression:
  | v = ex_primary { v } ;
  // | ex_prefix
  // | ex_postfix
  // | ex_binary
  // | ex_conditional
  // | ex_assignment

(* declarations *)

st_expr:
  | e = expression; PU_SEMICOLON { StExpr e }

func_body: list(st_expr) {$1};

prog:
  | e = func_body; EOF { e } 

// declaration:
//   | declaration_specifiers PU_SEMICOLON
// 	| declaration_specifiers init_declarator_list PU_SEMICOLON

// declaration_specifiers:
// 	| type_specifier
// 	| type_specifier declaration_specifiers

// init_declarator_list:
//   | init_declarator
// 	| init_declarator_list PU_COMMA init_declarator

// init_declarator:
//   | declarator
// 	| declarator ASN_BASE initializer

// type_specifier:
//   | TYP_VOID
// 	| TYP_CHAR
// 	| TYP_SHORT
// 	| TYP_INT
// 	| TYP_LONG
// 	| TYP_FLOAT
// 	| TYP_DOUBLE
// 	| struct_specifier
// 	| TYPE_NAME

// struct_specifier:
//   | KW_STRUCT IDENTIFIER BRC_L struct_declaration_list BRC_R
// 	| KW_STRUCT BRC_L struct_declaration_list BRC_R
// 	| KW_STRUCT IDENTIFIER

// struct_declaration_list:
//   | struct_declaration
// 	| struct_declaration_list struct_declaration

// struct_declaration:
//   | specifier_qualifier_list struct_declarator_list PU_SEMICOLON

// specifier_qualifier_list:
//   | type_specifier specifier_qualifier_list
// 	| type_specifier

// struct_declarator_list:
//   | struct_declarator
// 	| struct_declarator_list PU_COMMA struct_declarator

// struct_declarator:
//   | declarator
// 	| PU_COLON expression
// 	| declarator PU_COLON expression

// declarator:
//   | pointer direct_declarator
// 	| direct_declarator

// direct_declarator:
//   | IDENTIFIER
// 	| PRH_L declarator PRH_R
// 	| direct_declarator BRK_L expression BRK_R
// 	| direct_declarator BRK_L BRK_R
// 	| direct_declarator PRH_L parameter_type_list PRH_R
// 	| direct_declarator PRH_L identifier_list PRH_R
// 	| direct_declarator PRH_L PRH_R

// pointer:
//   | OP_STAR
// 	| OP_STAR pointer

// parameter_type_list:
//   | parameter_list
// 	| parameter_list PU_COMMA ELLIPSIS

// parameter_list:
//   | parameter_declaration
// 	| parameter_list PU_COMMA parameter_declaration

// parameter_declaration:
//   | declaration_specifiers declarator
// 	| declaration_specifiers abstract_declarator
// 	| declaration_specifiers

// identifier_list:
//   | IDENTIFIER
// 	| identifier_list PU_COMMA IDENTIFIER

// type_name:
//   | specifier_qualifier_list
// 	| specifier_qualifier_list abstract_declarator

// abstract_declarator:
//   | pointer
// 	| direct_abstract_declarator
// 	| pointer direct_abstract_declarator

// direct_abstract_declarator:
//   | PRH_L abstract_declarator PRH_R
// 	| BRK_L BRK_R
// 	| BRK_L expression BRK_R
// 	| direct_abstract_declarator BRK_L BRK_R
// 	| direct_abstract_declarator BRK_L expression BRK_R
// 	| PRH_L PRH_R
// 	| PRH_L parameter_type_list PRH_R
// 	| direct_abstract_declarator PRH_L PRH_R
// 	| direct_abstract_declarator PRH_L parameter_type_list PRH_R

// initializer:
//   | ex_assignment
// 	| BRC_L initializer_list BRC_R
// 	| BRC_L initializer_list PU_COMMA BRC_R

// initializer_list:
//   | initializer
// 	| initializer_list PU_COMMA initializer

// statement:
//   | st_labeled
// 	| st_compound
// 	| st_expression
// 	| st_selection
// 	| st_iteration
// 	| st_jump

// st_labeled:
// 	| CASE expression PU_COLON statement
// 	| DEFAULT PU_COLON statement

// st_compound:
//   | BRC_L BRC_R
// 	| BRC_L statement_list BRC_R
// 	| BRC_L declaration_list BRC_R
// 	| BRC_L declaration_list statement_list BRC_R

// declaration_list:
//   | declaration
// 	| declaration_list declaration

// statement_list:
//   | statement
// 	| statement_list statement

// st_expression:
//   | PU_SEMICOLON
// 	| expression PU_SEMICOLON

// st_selection:
//   | IF PRH_L expression PRH_R statement
// 	| IF PRH_L expression PRH_R statement ELSE statement
// 	| SWITCH PRH_L expression PRH_R statement

// st_iteration:
//   | WHILE PRH_L expression PRH_R statement
// 	| DO statement WHILE PRH_L expression PRH_R PU_SEMICOLON
// 	| FOR PRH_L st_expression st_expression PRH_R statement
// 	| FOR PRH_L st_expression st_expression expression PRH_R statement

// st_jump:
// 	| CONTINUE PU_SEMICOLON
// 	| BREAK PU_SEMICOLON
// 	| RETURN PU_SEMICOLON
// 	| RETURN expression PU_SEMICOLON

// translation_unit:
//   | external_declaration
// 	| translation_unit external_declaration

// external_declaration:
//   | function_definition
// 	| declaration

// function_definition:
//   | declaration_specifiers declarator declaration_list st_compound
// 	| declaration_specifiers declarator st_compound
// 	| declarator declaration_list st_compound
// 	| declarator st_compound

