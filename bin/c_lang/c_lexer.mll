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
    ("struct", KW_STRUCT);

    (* control *)
    ("break", KW_BREAK);
    ("case", KW_CASE);
    ("continue", KW_CONTINUE);
    ("else", KW_ELSE);
    ("for", KW_FOR);
    ("if", KW_IF);
    ("return", KW_RETURN);
    ("while", KW_WHILE); 
    ("sizeof", KW_SIZEOF);

    (* NOT IMPLEMENTED *)
    (* ("switch", KW_SWITCH); *)
    (* ("default", KW_DEFAULT); *)
    (* ("do", KW_DO); *)
    (* ("const", CONST); *)
    (* ("extern", KW_EXTERN); *)
    (* ("static", KW_STATIC); *)
    (* ("enum", KW_ENUM); *) 
    (* ("volatile", KW_VOLATILE); *)
    (* ("union", KW_UNION); *)
    (* ("typedef", KW_TYPEDEF); *)
    (* ("signed", KW_SIGNED); *)
    (* ("unsigned", KW_UNSIGNED); *)
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
  | "+" { OP_PLUS }
  | "-" { OP_MINUS }
  | "*" { OP_STAR }
  | "/" { OP_DIV }
  | "%" { OP_MOD }
  | "&" { OP_AMPERSAND }
  | "^" { OP_XOR }
  | "|" { OP_OR }
  | "&&" { OP_LAND }
  | "||" { OP_LOR }
  | ">>" { OP_RIGHT }
  | "<<" { OP_LEFT }
  | "->" { OP_ARROW }
  | "." { OP_DOT }
  (* unary operators *)
  | '!' { UOP_NEG }
  | '~' { UOP_INV }
  | "++" { UOP_INC }
  | "--" { UOP_DEC }

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
  | eof { EOF }
  | id { id_or_keyword lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
  | '\n' { read lexbuf }
  | _ { comment lexbuf }

and block_comment = parse
  | "*/" { read lexbuf }
  | _ { block_comment lexbuf }
