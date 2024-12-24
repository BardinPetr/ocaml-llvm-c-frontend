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
    |}
