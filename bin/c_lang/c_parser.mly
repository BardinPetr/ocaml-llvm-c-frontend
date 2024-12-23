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
%type <C_syntax.program> prog

%start block
%type <C_syntax.stmt list> block

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
  | d = decl_var; PU_SEMICOLON { StDecl d }
  | st_if { $1 }
  | st_for { $1 }
  | st_while { $1 }

(* declarations *)

decl_var_init: 
  | ASN_BASE; e=expression { e }

decl_typed_var:
  | typ=ex_type; name=IDENTIFIER; arr_dims=list(delimited(BRK_L, LIT_INT, BRK_R))
    { ((wrap_ctype_array (List.rev arr_dims) typ), name) }

decl_var:
  | v=decl_typed_var; init=option(decl_var_init) 
    { match v with (typ, name) -> DeclVar (typ, name, init) }

decl_fun_sig:
  | ret=ex_type; name=IDENTIFIER; params=delimited(PRH_L, separated_list(PU_COMMA, decl_typed_var), PRH_R)
    { (ret, name, params) }

decl_struct:
  | KW_STRUCT; name=IDENTIFIER; fields=delimited(BRC_L, list(terminated(decl_typed_var, PU_SEMICOLON)), BRC_R)
    { (name, fields) }

(* global *)

decl_global:
  | d = decl_fun_sig; PU_SEMICOLON 
    { match d with (ret, name, params) -> FuncGDecl (ret, name, params, None) }
  | d = decl_fun_sig; body=block 
    { match d with (ret, name, params) -> FuncGDecl (ret, name, params, Some body) }
  | d = decl_var; PU_SEMICOLON { VarGDecl d }
  | d = decl_struct; PU_SEMICOLON
    { match d with (name, fields) -> StructGDecl (name, fields) }

prog:
  | e = list(decl_global); EOF { Prog e } 
