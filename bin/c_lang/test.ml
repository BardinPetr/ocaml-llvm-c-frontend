open Printf

let%expect_test "minimal" =
  let code = "1 + 1" in
  (* code |> Main.lex |> Main.print_tokens *)
  code |> Main.parse |> C_syntax.show_expr |> printf "%s\n"
