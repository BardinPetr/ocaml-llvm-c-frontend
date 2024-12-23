let tr x = x |> Main.parse |> C_fe.translate

let%expect_test "fun" =
  tr
    {|  
      void printf(char* fmt, ...);

      void test() {
        //return 1 + 'a';
        return;
      }

      int main(int argv, char** argc) {
        //return test();
        return;
      }
    |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"

    declare void @printf(ptr %0, ...)

    define void @test() {
    entry:
    }

    define i32 @main(i32 %0, ptr %1) {
    entry:
    }
    |}]
