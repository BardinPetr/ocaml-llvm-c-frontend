let tr x = x |> Main.parse |> C_fe.translate

let%expect_test "fun" =
  tr
    {|  
      void printf(char* fmt, ...);

      int test() {
        int a = 42;
        int b = a % 10;
        int c = (b + a * 2);
        return c / 4;
      }

      int main(int argv, char** argc) {
        return test();
      }
    |}
