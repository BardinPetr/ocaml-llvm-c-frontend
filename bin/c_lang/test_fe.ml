let exec_mod llm =
  Llvm_executionengine.(
    initialize () |> ignore;
    let engine = Llvm_executionengine.create llm in
    let main_func =
      Llvm_executionengine.get_function_address "main"
        (Foreign.funptr Ctypes.(void @-> returning int))
        engine
    in
    Printf.printf "Return code: %d" (main_func ()))

let tr x = x |> Main.parse |> C_fe.translate
let tr_print x = Llvm.dump_module (tr x)

let tr_print_exec x =
  let m = tr x in
  Llvm.dump_module m;
  exec_mod m

let%expect_test "calc" =
  tr_print_exec
    {|  
  int main(int argv, char** argc) {
    int a = 42;
    int b = a % 10; // 2
    b -= 1; // 1
    int c = (b + a * 2); // 85
    c += 2; // 87
    return c + ++b; // 87 + 2 = 89 
  }
|};
  [%expect
    {|
    ; ModuleID = 'main'
    source_filename = "main"

    define i32 @main(i32 %0, ptr %1) {
    entry:
      %a = alloca i32, align 4
      store i32 42, ptr %a, align 4
      %b = alloca i32, align 4
      %tmp = load i32, ptr %a, align 4
      %tmp1 = srem i32 %tmp, 10
      store i32 %tmp1, ptr %b, align 4
      %tmp2 = load i32, ptr %b, align 4
      %tmp3 = sub i32 %tmp2, 1
      store i32 %tmp3, ptr %b, align 4
      %tmp4 = load i32, ptr %b, align 4
      %c = alloca i32, align 4
      %tmp5 = load i32, ptr %a, align 4
      %tmp6 = mul i32 %tmp5, 2
      %tmp7 = load i32, ptr %b, align 4
      %tmp8 = add i32 %tmp7, %tmp6
      store i32 %tmp8, ptr %c, align 4
      %tmp9 = load i32, ptr %c, align 4
      %tmp10 = add i32 %tmp9, 2
      store i32 %tmp10, ptr %c, align 4
      %tmp11 = load i32, ptr %c, align 4
      %tmp12 = load i32, ptr %b, align 4
      %tmp13 = load i32, ptr %b, align 4
      %tmp14 = add i32 %tmp13, 1
      store i32 %tmp14, ptr %b, align 4
      %tmp15 = load i32, ptr %b, align 4
      %tmp16 = load i32, ptr %c, align 4
      %tmp17 = add i32 %tmp16, %tmp15
      ret i32 %tmp17
    }
    Return code: 89
    |}]
