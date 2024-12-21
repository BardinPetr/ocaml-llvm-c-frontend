{
open Lexing
open C_parser

exception SyntaxError of string

let keyword_table = Hashtbl.create 30
let _ =
  List.iter (fun (keyword, token) ->
    Hashtbl.add keyword_table keyword token
  ) [
    (* types *)
    ("void", TYP_VOID);
    ("char", TYP_CHAR);
    ("short", TYP_SHORT);
    ("int", TYP_INT);
    ("long", TYP_LONG);
    ("float", TYP_FLOAT);
    ("double", TYP_DOUBLE);
    ("struct", CW_STRUCT);

    (* control *)
    ("break", CW_BREAK);
    ("case", CW_CASE);
    ("continue", CW_CONTINUE);
    ("default", CW_DEFAULT);
    ("do", CW_DO);
    ("else", CW_ELSE);
    ("float", CW_FLOAT);
    ("for", CW_FOR);
    ("goto", CW_GOTO);
    ("if", CW_IF);
    ("return", CW_RETURN);
    ("switch", CW_SWITCH);
    ("while", CW_WHILE); 
    ("sizeof", CW_SIZEOF);

    (* NOT IMPLEMENTED *)
    (* ("const", CONST); ??? *)
    (* ("extern", CW_EXTERN); ??? *)
    (* ("static", CW_STATIC); ??? *)
    (* ("enum", CW_ENUM); ???*) 
    (* ("volatile", CW_VOLATILE); *)
    (* ("union", CW_UNION); *)
    (* ("typedef", CW_TYPEDEF); *)
    (* ("signed", CW_SIGNED); *)
    (* ("unsigned", CW_UNSIGNED); *)
  ]

let id_or_keyword lexbuf =
  let id = Lexing.lexeme lexbuf in
  try Hashtbl.find keyword_table id
  with Not_found -> IDENTIFIER id
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let id = letter (letter | digit | '_')*
let integer = digit+
let float = digit+ '.' digit+
let char = '\'' _ '\''
let string = '"' (('\\' _) | [^'"'])* '"'


rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | "//" { comment lexbuf }
  | "/*" { block_comment lexbuf }

  (* literals *)
  | integer as i { LIT_INT (int_of_string i) }
  | float as i { LIT_FLOAT (float_of_string i) }
  | char as i { LIT_CHAR (String.get i 1) }
  | string as i { LIT_STRING (Scanf.unescaped (String.sub i 1 (String.length i - 2))) }
  
  (* operators *)
  | ">>" { OP_RIGHT }
  | "<<" { OP_LEFT }
  | "+" { OP_ADD }
  | "-" { OP_SUB }
  | "*" { OP_STAR }
  | "/" { OP_DIV }
  | "%" { OP_MOD }
  | "&" { OP_AND }
  | "^" { OP_XOR }
  | "|" { OP_OR }
  | "&&" { OP_LAND }
  | "||" { OP_LOR }
  | "->" { OP_ARROW }
  | "." { OP_DOT }
  | "++" { OP_INC }
  | "--" { OP_DEC }

  (* unary operators *)
  | '!' { UOP_NEG }
  | '~' { UOP_INV }

  (* assignment *)
  | "=" { ASN_BASE }
  | ">>=" { ASN_RIGHT }
  | "<<=" { ASN_LEFT }
  | "+=" { ASN_ADD }
  | "-=" { ASN_SUB }
  | "*=" { ASN_MUL }
  | "/=" { ASN_DIV }
  | "%=" { ASN_MOD }
  | "&=" { ASN_AND }
  | "^=" { ASN_XOR }
  | "|=" { ASN_OR }

  (* comparison *)
  | "<=" { CMP_LE }
  | "<" { CMP_LT }
  | ">=" { CMP_GE }
  | ">" { CMP_GT }
  | "==" { CMP_EQ }
  | "!=" { CMP_NE }

  (* punctuation *)
  | '(' { PRH_L }
  | ')' { PRH_R }
  | '{' { BRC_L }
  | '}' { BRC_R }
  | '[' { BRK_L }
  | ']' { BRK_R }
  | "?" { PU_QUEST }
  | ":" { PU_COLON }
  | ";" { PU_SEMICOLON }
  | "," { PU_COMMA }

  (* final *)
  | id { id_or_keyword lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
  | '\n' { read lexbuf }
  | _ { comment lexbuf }

and block_comment = parse
  | "*/" { read lexbuf }
  | _ { block_comment lexbuf }
