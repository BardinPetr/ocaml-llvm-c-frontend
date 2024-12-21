open Printf

let parse_default x = x |> Main.parse |> C_syntax.show_expr |> printf "%s\n"
let lex_default x = x |> Main.lex |> Main.print_tokens

(* lexer tests *)
let%expect_test "math" =
  lex_default "1 + 1 * 23 / ( 123 + 13 / 2) * (2 * (2 * 1 / (1)))";
  [%expect
    {| (C_parser.LIT_INT 1), C_parser.OP_ADD, (C_parser.LIT_INT 1), C_parser.OP_STAR, (C_parser.LIT_INT 23), C_parser.OP_DIV, C_parser.PRH_L, (C_parser.LIT_INT 123), C_parser.OP_ADD, (C_parser.LIT_INT 13), C_parser.OP_DIV, (C_parser.LIT_INT 2), C_parser.PRH_R, C_parser.OP_STAR, C_parser.PRH_L, (C_parser.LIT_INT 2), C_parser.OP_STAR, C_parser.PRH_L, (C_parser.LIT_INT 2), C_parser.OP_STAR, (C_parser.LIT_INT 1), C_parser.OP_DIV, C_parser.PRH_L, (C_parser.LIT_INT 1), C_parser.PRH_R, C_parser.PRH_R, C_parser.PRH_R |}]

let%expect_test "literals" =
  lex_default "'a' \"asdad\njh\thkasd\" \"\" 231 12.21";
  [%expect
    {| (C_parser.LIT_CHAR 'a'), (C_parser.LIT_STRING "asdad\njh\thkasd"), (C_parser.LIT_STRING ""), (C_parser.LIT_INT 231), (C_parser.LIT_FLOAT 12.21) |}]

let%expect_test "comment" =
  lex_default
    {| 
  1 
  2 /* -1 */ 
  3 /* -2
  -3
  */ 4
  5 
  6 // -4 
  7 |};
  [%expect
    {| (C_parser.LIT_INT 1), (C_parser.LIT_INT 2), (C_parser.LIT_INT 3), (C_parser.LIT_INT 4), (C_parser.LIT_INT 5), (C_parser.LIT_INT 6), (C_parser.LIT_INT 7) |}]
