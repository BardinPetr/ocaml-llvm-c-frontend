open Lexing
open Printf

type tok = [%import: C_parser.token] [@@deriving show]

let print_tokens x = List.map show_tok x |> String.concat ", " |> printf "%s\n"

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse str =
  let lexbuf = Lexing.from_string str in
  try C_parser.prog C_lexer.read lexbuf with
  | C_lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      exit (-1)
  | C_parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let lex s =
  let lb = Lexing.from_string s in
  let rec helper l =
    try
      let t = C_lexer.read lb in
      if t = C_parser.EOF then
        List.rev l
      else
        helper (t :: l)
    with _ -> List.rev l
  in
  helper []

let translate f_in f_out =
  f_in
  |> Core.(In_channel.read_all)
  |> parse
  |> C_fe.translate
  |> Llvm.print_module f_out
