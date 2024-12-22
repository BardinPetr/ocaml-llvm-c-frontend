open Printf

let parse_default x =
  x
  |> Main.parse
  |> List.map C_syntax.show_stmt
  |> String.concat ";\n"
  |> printf "%s\n"

let lex_default x = x |> Main.lex |> Main.print_tokens

(* lexer tests *)
let%expect_test "math" =
  lex_default "1 + 1 * 23 / ( 123 + 13 / 2) * (2 * (2 * 1 / (1)))";
  [%expect
    {| (C_parser.LIT_INT 1), C_parser.OP_PLUS, (C_parser.LIT_INT 1), C_parser.OP_STAR, (C_parser.LIT_INT 23), C_parser.OP_DIV, C_parser.PRH_L, (C_parser.LIT_INT 123), C_parser.OP_PLUS, (C_parser.LIT_INT 13), C_parser.OP_DIV, (C_parser.LIT_INT 2), C_parser.PRH_R, C_parser.OP_STAR, C_parser.PRH_L, (C_parser.LIT_INT 2), C_parser.OP_STAR, C_parser.PRH_L, (C_parser.LIT_INT 2), C_parser.OP_STAR, (C_parser.LIT_INT 1), C_parser.OP_DIV, C_parser.PRH_L, (C_parser.LIT_INT 1), C_parser.PRH_R, C_parser.PRH_R, C_parser.PRH_R |}]

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

(* parser tests *)

let%expect_test "expression" =
  parse_default
    {| 
  1;
  (2);
  asd;
  "asd";
  ++a;
  &*(*a);
  sizeof(int);
  sizeof(struct asd);
  asb < ++asd ? 123 : ++bca;
  func1();
  func2(1, 'a', "222", ++asd, asd--, &(p++[324]));
  |};
  [%expect {|
    (C_syntax.StExpr (C_syntax.ExLiteral (C_syntax.IntLit 1)));
    (C_syntax.StExpr (C_syntax.ExLiteral (C_syntax.IntLit 2)));
    (C_syntax.StExpr (C_syntax.ExId "asd"));
    (C_syntax.StExpr (C_syntax.ExLiteral (C_syntax.StringLit "asd")));
    (C_syntax.StExpr (C_syntax.ExUOpPre (C_syntax.UOInc, (C_syntax.ExId "a"))));
    (C_syntax.StExpr
       (C_syntax.ExUOpPre (C_syntax.UORef,
          (C_syntax.ExUOpPre (C_syntax.UODeref,
             (C_syntax.ExUOpPre (C_syntax.UODeref, (C_syntax.ExId "a")))))
          )));
    (C_syntax.StExpr (C_syntax.ExSizeof C_syntax.TInt));
    (C_syntax.StExpr (C_syntax.ExSizeof (C_syntax.TStruct "asd")));
    (C_syntax.StExpr
       (C_syntax.ExTernary (
          (C_syntax.ExBOp (C_syntax.OpCmpLt, (C_syntax.ExId "asb"),
             (C_syntax.ExUOpPre (C_syntax.UOInc, (C_syntax.ExId "asd"))))),
          (C_syntax.ExLiteral (C_syntax.IntLit 123)),
          (C_syntax.ExUOpPre (C_syntax.UOInc, (C_syntax.ExId "bca"))))));
    (C_syntax.StExpr (C_syntax.ExCall ("func1", [])));
    (C_syntax.StExpr
       (C_syntax.ExCall ("func2",
          [(C_syntax.ExLiteral (C_syntax.IntLit 1));
            (C_syntax.ExLiteral (C_syntax.CharLit 'a'));
            (C_syntax.ExLiteral (C_syntax.StringLit "222"));
            (C_syntax.ExUOpPre (C_syntax.UOInc, (C_syntax.ExId "asd")));
            (C_syntax.ExUOpPost (C_syntax.UODec, (C_syntax.ExId "asd")));
            (C_syntax.ExUOpPre (C_syntax.UORef,
               (C_syntax.ExArrIdx (
                  (C_syntax.ExUOpPost (C_syntax.UOInc, (C_syntax.ExId "p"))),
                  (C_syntax.ExLiteral (C_syntax.IntLit 324))))
               ))
            ]
          )))
    |}]

let%expect_test "binary_expr" =
  parse_default
    {| 
    1 * 2 + 3 / 4 - 4 % 2 + 2 ;
    test += (a + 3)->f_name;
    a = func(1 * 1 - 13 + asd.dsa * 4 + 1 / 8);
  |};
  [%expect {|
    (C_syntax.StExpr
       (C_syntax.ExBOp (C_syntax.OpMul, (C_syntax.ExLiteral (C_syntax.IntLit 1)),
          (C_syntax.ExBOp (C_syntax.OpAdd,
             (C_syntax.ExLiteral (C_syntax.IntLit 2)),
             (C_syntax.ExBOp (C_syntax.OpDiv,
                (C_syntax.ExLiteral (C_syntax.IntLit 3)),
                (C_syntax.ExBOp (C_syntax.OpSub,
                   (C_syntax.ExLiteral (C_syntax.IntLit 4)),
                   (C_syntax.ExBOp (C_syntax.OpMod,
                      (C_syntax.ExLiteral (C_syntax.IntLit 4)),
                      (C_syntax.ExBOp (C_syntax.OpAdd,
                         (C_syntax.ExLiteral (C_syntax.IntLit 2)),
                         (C_syntax.ExLiteral (C_syntax.IntLit 2))))
                      ))
                   ))
                ))
             ))
          )));
    (C_syntax.StExpr
       (C_syntax.ExAOp (C_syntax.AsnAdd, (C_syntax.ExId "test"),
          (C_syntax.ExPAccess (
             (C_syntax.ExBOp (C_syntax.OpAdd, (C_syntax.ExId "a"),
                (C_syntax.ExLiteral (C_syntax.IntLit 3)))),
             "f_name"))
          )));
    (C_syntax.StExpr
       (C_syntax.ExAOp (C_syntax.AsnBase, (C_syntax.ExId "a"),
          (C_syntax.ExCall ("func",
             [(C_syntax.ExBOp (C_syntax.OpMul,
                 (C_syntax.ExLiteral (C_syntax.IntLit 1)),
                 (C_syntax.ExBOp (C_syntax.OpSub,
                    (C_syntax.ExLiteral (C_syntax.IntLit 1)),
                    (C_syntax.ExBOp (C_syntax.OpAdd,
                       (C_syntax.ExLiteral (C_syntax.IntLit 13)),
                       (C_syntax.ExBOp (C_syntax.OpMul,
                          (C_syntax.ExAccess ((C_syntax.ExId "asd"), "dsa")),
                          (C_syntax.ExBOp (C_syntax.OpAdd,
                             (C_syntax.ExLiteral (C_syntax.IntLit 4)),
                             (C_syntax.ExBOp (C_syntax.OpDiv,
                                (C_syntax.ExLiteral (C_syntax.IntLit 1)),
                                (C_syntax.ExLiteral (C_syntax.IntLit 8))))
                             ))
                          ))
                       ))
                    ))
                 ))
               ]
             ))
          )))
    |}]
