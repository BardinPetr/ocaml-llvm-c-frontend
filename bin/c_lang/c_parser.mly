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
%token KW_BREAK KW_CASE KW_CONTINUE KW_DEFAULT KW_DO KW_ELSE KW_FOR KW_IF KW_RETURN KW_SWITCH KW_WHILE

%token EOF

%left PU_COMMA
%right PU_QUEST PU_COLON
%right ASN_BASE ASN_RIGHT ASN_LEFT ASN_ADD ASN_SUB ASN_MUL ASN_DIV ASN_MOD ASN_AND ASN_XOR ASN_OR
%left OP_OR
%left OP_LOR
%left OP_XOR
%left OP_AMPERSAND
%left OP_LAND
%left CMP_EQ CMP_NE
%left CMP_LT CMP_GT CMP_LE CMP_GE
%left OP_LEFT OP_RIGHT
%left OP_STAR OP_DIV OP_MOD
%right UOP_INV UOP_NEG UOP_INC UOP_DEC KW_SIZEOF
%left OP_DOT
%left BRK_L BRK_R PRH_L PRH_R


%start prog
%type <C_syntax.stmt list> prog

// %start block
// %type <C_syntax.stmt list> block

%%

(* operators *)

%inline op_unary_prefix:
  | OP_MINUS { UOMinus }
  | UOP_INV { UOInv }
  | UOP_NEG { UONeg }
  | UOP_INC { UOInc }
  | UOP_DEC { UODec } 
  | OP_AMPERSAND { UORef }
  | OP_STAR { UODeref }

%inline op_binary:
  | OP_PLUS  { OpAdd }
  | OP_MINUS { OpSub }
  | OP_STAR { OpMul }
  | OP_DIV { OpDiv }
  | OP_MOD { OpMod }
  | OP_AMPERSAND { OpAnd }
  | OP_XOR { OpXor }
  | OP_OR { OpOr }
  | OP_LAND { OpLAnd }
  | OP_LOR { OpLOr }
  | OP_RIGHT { OpRight }
  | OP_LEFT { OpLeft }
  | CMP_LE { OpCmpLe }
  | CMP_LT { OpCmpLt }
  | CMP_GE { OpCmpGe }
  | CMP_GT { OpCmpGt }
  | CMP_EQ { OpCmpEq }
  | CMP_NE { OpCmpNe }

%inline op_assign:
  | ASN_BASE { AsnBase }
  | ASN_RIGHT { AsnRight }
  | ASN_LEFT { AsnLeft }
  | ASN_ADD { AsnAdd }
  | ASN_SUB { AsnSub }
  | ASN_MUL { AsnMul }
  | ASN_DIV { AsnDiv }
  | ASN_MOD { AsnMod }
  | ASN_AND { AsnAnd }
  | ASN_XOR { AsnXor }
  | ASN_OR { AsnOr }

(* expressions *)

ex_type:
  | TYP_CHAR {TChar}
  | TYP_SHORT {TShort}
  | TYP_INT {TInt}
  | TYP_LONG {TLong}
  | TYP_FLOAT {TFloat}
  | TYP_DOUBLE {TDouble}
  | TYP_VOID {TVoid}
  | v = IDENTIFIER { TOther v }
  | KW_STRUCT; v = IDENTIFIER { TStruct v } 
  | t=ex_type; OP_STAR { TPtr t }

literal:
  | LIT_INT { IntLit $1 }
  | LIT_FLOAT { FloatLit $1 }
  | LIT_CHAR { CharLit $1 }
  | LIT_STRING { StringLit $1 } 

ex_primary:
  | PRH_L; e = expression; PRH_R { e }
  | v = IDENTIFIER { ExId v }
  | v = literal { ExLiteral v } 

ex_prefix:
  | o = op_unary_prefix; e = expression { ExUOpPre (o, e) }
  | KW_SIZEOF; PRH_L; t = ex_type; PRH_R { ExSizeof t }
  | PRH_L; t = ex_type; PRH_R; e = expression { ExCast (t, e) }

ex_postfix:
  | arr=expression; BRK_L; idx=expression; BRK_R { ExArrIdx (arr, idx) }
  | f=IDENTIFIER; PRH_L; ps=separated_list(PU_COMMA, expression) ; PRH_R { ExCall (f, ps) }
  | e=expression; UOP_INC { ExUOpPost (UOInc, e) }
  | e=expression; UOP_DEC { ExUOpPost (UODec, e) }

ex_binary:
  | l=expression; o=op_binary; r=expression { ExBOp (o, l, r) }
  | l=expression; o=op_assign; r=expression { ExAOp (o, l, r) }
  | l=expression; OP_DOT; n=IDENTIFIER { ExAccess (l, n) }
  | l=expression; OP_ARROW; n=IDENTIFIER { ExPAccess (l, n) }
  
ex_conditional:
  | q = expression; PU_QUEST; t = expression; PU_COLON; f = expression { ExTernary (q, t, f) }

expression:
  | ex_primary {$1} 
  | ex_prefix {$1}
  | ex_postfix {$1}
  | ex_binary {$1}
  | ex_conditional {$1}

ex_opt: e = ioption(expression) { e }

(* blocks (stmt list) *)

block_cont: l = list(statement) { l }

block: BRC_L; b=block_cont; BRC_R { b } 

block_or_line:
  | b=block { b }
  | s=statement { [ s ] }

(* statements *)

st_if_hd: KW_IF; PRH_L; chk=expression; PRH_R { chk }
st_if:
  | chk=st_if_hd; t=block_or_line { StIf (chk, t, None) }
  | chk=st_if_hd; t=block_or_line; KW_ELSE; f=block_or_line { StIf (chk, t, Some f) }

st_for: KW_FOR; PRH_L; s1=ex_opt; PU_SEMICOLON; s2=ex_opt; PU_SEMICOLON; s3=ex_opt; PRH_R; b=block_or_line
  { StFor (s1, s2, s3, b) }

st_while: KW_WHILE; PRH_L; e=expression; PRH_R; f=block_or_line { StWhile (e, f) }

st_cmd: 
  | KW_BREAK { StBreak }
  | KW_CONTINUE { StContinue }
  | KW_RETURN; e = ioption(expression) { StReturn e }

statement:
  | e = expression; PU_SEMICOLON { StExpr e }
  | s = st_cmd; PU_SEMICOLON { s }
  | st_if { $1 }
  | st_for { $1 }
  | st_while { $1 }

(* declarations *)

prog:
  | e = block; EOF { e } 

// declaration:
//   | declaration_specifiers PU_SEMICOLON
//   | declaration_specifiers init_declarator_list PU_SEMICOLON

// declaration_specifiers:
//   | type_specifier
//   | type_specifier declaration_specifiers

// init_declarator_list:
//   | init_declarator
//   | init_declarator_list PU_COMMA init_declarator

// init_declarator:
//   | declarator
//   | declarator ASN_BASE initializer

// struct_specifier:
//   | KW_STRUCT IDENTIFIER BRC_L struct_declaration_list BRC_R
//   | KW_STRUCT BRC_L struct_declaration_list BRC_R
//   | KW_STRUCT IDENTIFIER

// struct_declaration_list:
//   | struct_declaration
//   | struct_declaration_list struct_declaration

// struct_declaration:
//   | specifier_qualifier_list struct_declarator_list PU_SEMICOLON

// specifier_qualifier_list:
//   | type_specifier specifier_qualifier_list
//   | type_specifier

// struct_declarator_list:
//   | struct_declarator
//   | struct_declarator_list PU_COMMA struct_declarator

// struct_declarator:
//   | declarator
//   | PU_COLON expression
//   | declarator PU_COLON expression

// declarator:
//   | pointer direct_declarator
//   | direct_declarator

// direct_declarator:
//   | IDENTIFIER
//   | PRH_L declarator PRH_R
//   | direct_declarator BRK_L expression BRK_R
//   | direct_declarator BRK_L BRK_R
//   | direct_declarator PRH_L parameter_type_list PRH_R
//   | direct_declarator PRH_L identifier_list PRH_R
//   | direct_declarator PRH_L PRH_R

// pointer:
//   | OP_STAR
//   | OP_STAR pointer

// parameter_type_list:
//   | parameter_list
//   | parameter_list PU_COMMA ELLIPSIS

// parameter_list:
//   | parameter_declaration
//   | parameter_list PU_COMMA parameter_declaration

// parameter_declaration:
//   | declaration_specifiers declarator
//   | declaration_specifiers abstract_declarator
//   | declaration_specifiers

// identifier_list:
//   | IDENTIFIER
//   | identifier_list PU_COMMA IDENTIFIER

// type_name:
//   | specifier_qualifier_list
//   | specifier_qualifier_list abstract_declarator

// abstract_declarator:
//   | pointer
//   | direct_abstract_declarator
//   | pointer direct_abstract_declarator

// direct_abstract_declarator:
//   | PRH_L abstract_declarator PRH_R
//   | BRK_L BRK_R
//   | BRK_L expression BRK_R
//   | direct_abstract_declarator BRK_L BRK_R
//   | direct_abstract_declarator BRK_L expression BRK_R
//   | PRH_L PRH_R
//   | PRH_L parameter_type_list PRH_R
//   | direct_abstract_declarator PRH_L PRH_R
//   | direct_abstract_declarator PRH_L parameter_type_list PRH_R

// initializer:
//   | ex_assignment
//   | BRC_L initializer_list BRC_R
//   | BRC_L initializer_list PU_COMMA BRC_R

// initializer_list:
//   | initializer
//   | initializer_list PU_COMMA initializer

// statement:
//   | st_labeled
//   | st_compound
//   | st_expression
//   | st_selection
//   | st_iteration
//   | st_jump

// st_labeled:
//   | CASE expression PU_COLON statement
//   | DEFAULT PU_COLON statement

// st_compound:
//   | BRC_L BRC_R
//   | BRC_L statement_list BRC_R
//   | BRC_L declaration_list BRC_R
//   | BRC_L declaration_list statement_list BRC_R

// declaration_list:
//   | declaration
//   | declaration_list declaration

// statement_list:
//   | statement
//   | statement_list statement

// st_expression:
//   | PU_SEMICOLON
//   | expression PU_SEMICOLON

// st_selection:
//   | IF PRH_L expression PRH_R statement
//   | IF PRH_L expression PRH_R statement ELSE statement
//   | SWITCH PRH_L expression PRH_R statement

// st_iteration:
//   | WHILE PRH_L expression PRH_R statement
//   | DO statement WHILE PRH_L expression PRH_R PU_SEMICOLON
//   | FOR PRH_L st_expression st_expression PRH_R statement
//   | FOR PRH_L st_expression st_expression expression PRH_R statement

// st_jump:
//   | CONTINUE PU_SEMICOLON
//   | BREAK PU_SEMICOLON
//   | RETURN PU_SEMICOLON
//   | RETURN expression PU_SEMICOLON

// translation_unit:
//   | external_declaration
//   | translation_unit external_declaration

// external_declaration:
//   | function_definition
//   | declaration

// function_definition:
//   | declaration_specifiers declarator declaration_list st_compound
//   | declaration_specifiers declarator st_compound
//   | declarator declaration_list st_compound
//   | declarator st_compound

