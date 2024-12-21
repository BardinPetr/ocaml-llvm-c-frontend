{
open Lexing
open C_parser

exception SyntaxError of string

let keyword_table = Hashtbl.create 10
let _ =
  List.iter (fun (keyword, token) ->
    Hashtbl.add keyword_table keyword token
  ) [
    ("void", VOID);
    ("char", CHAR);
    ("short", SHORT);
    ("int", INT);
    ("long", LONG);
    ("float", FLOAT);
    ("double", DOUBLE);
  ]

let id_or_keyword lexbuf =
  let id = Lexing.lexeme lexbuf in
  try Hashtbl.find keyword_table id
  with Not_found -> IDENTIFIER id

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
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
  | newline { newline lexbuf; read lexbuf }
  | integer as i { INT_LIT (int_of_string i) }
  | float { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) }
  | char { CHAR_LIT (String.get (Lexing.lexeme lexbuf) 1) }
  | string { STRING_LIT (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 2)) }
  | '+' { ADD_OP }
  | '-' { SUB_OP }
  | '*' { MUL_OP }
  | '/' { DIV_OP }
  | id { id_or_keyword lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }


  (* | ['0'-'9']+ as i { INT_LITERAL (int_of_string i) } *)
  (* | white    { read lexbuf } *)
  (* | newline  { next_line lexbuf; read lexbuf } *)
  (* | int      { INT_LITERAL (int_of_string (Lexing.lexeme lexbuf)) } *)
  (* | float    { FLOAT_LITERAL (float_of_string (Lexing.lexeme lexbuf)) } *)
(*| '"'      { read_string (Buffer.create 17) lexbuf }
  | ','      { COMMA }
  | '...' { ELLIPSIS }
  | '>>=' { RIGHT_ASSIGN }
  | '<<=' { LEFT_ASSIGN }
  | '+=' { ADD_ASSIGN }
  | '-=' { SUB_ASSIGN }
  | '*=' { MUL_ASSIGN }
  | '/=' { DIV_ASSIGN }
  | '%=' { MOD_ASSIGN }
  | '&=' { AND_ASSIGN }
  | '^=' { XOR_ASSIGN }
  | '|=' { OR_ASSIGN }
  | '>>' { RIGHT_OP }
  | '<<' { LEFT_OP }
  | '++' { INC_OP }
  | '--' { DEC_OP }
  | '->' { PTR_OP }
  | '&&' { AND_OP }
  | '||' { OR_OP }
  | '<=' { LE_OP }
  | '>=' { GE_OP }
  | '==' { EQ_OP }
  | '!=' { NE_OP }
  | ';' { ';' }
  | '{' { '{' }
  | '}' { '}' }
  | ',' { ',' }
  | ':' { ':' }
  | '=' { '=' }
  | '(' { '(' }
  | ')' { ')' }
  | '[' { '[' }
  | ']' { ']' }
  | '.' { '.' }
  | '&' { '&' }
  | '!' { '!' }
  | '~' { '~' }
  | '-' { '-' }
  | '+' { '+' }
  | '*' { '*' }
  | '/' { '/' }
  | '%' { '%' }
  | '<' { '<' }
  | '>' { '>' }
  | '^' { '^' }
  | '|' { '|' }
  | '?' { '?' }
  | 'auto' { AUTO }
  | 'break' { BREAK }
  | 'case' { CASE }
  | 'char' { CHAR }
  | 'const' { CONST }
  | 'continue' { CONTINUE }
  | 'default' { DEFAULT }
  | 'do' { DO }
  | 'double' { DOUBLE }
  | 'else' { ELSE }
  | 'enum' { ENUM }
  | 'extern' { EXTERN }
  | 'float' { FLOAT }
  | 'for' { FOR }
  | 'goto' { GOTO }
  | 'if' { IF }
  | 'int' { INT }
  | 'long' { LONG }
  | 'register' { REGISTER }
  | 'return' { RETURN }
  | 'short' { SHORT }
  | 'signed' { SIGNED }
  | 'sizeof' { SIZEOF }
  | 'static' { STATIC }
  | 'struct' { STRUCT }
  | 'switch' { SWITCH }
  | 'typedef' { TYPEDEF }
  | 'union' { UNION }
  | 'unsigned' { UNSIGNED }
  | 'void' { VOID }
  | 'volatile' { VOLATILE }
  | 'while' { WHILE } 
  | eof      { EOF }*)

(* 
and comment = parse
  | '\n' { token lexbuf }
  | _ { comment lexbuf }

and block_comment = parse
  | "*/" { token lexbuf }
  | _ { block_comment lexbuf } *)