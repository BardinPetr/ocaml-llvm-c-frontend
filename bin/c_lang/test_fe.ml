let exec_mod llm =
  Llvm_executionengine.(
    initialize () |> ignore;
    let engine = Llvm_executionengine.create llm in
    let main_func =
      Llvm_executionengine.get_function_address "main"
        (Foreign.funptr Ctypes.(void @-> returning int))
        engine
    in
    Printf.printf "----RET -> %d----\n" (main_func ()))

let tr x = x |> Main.parse |> C_fe.translate
let tr_print x = Llvm.dump_module (tr x)

let tr_print_exec x =
  let m = tr x in
  print_endline "\n----IR----";
  Llvm.dump_module m;
  exec_mod m

let tr_compile_run text =
  let m = tr text in
  print_endline "\n----IR----";
  Llvm.dump_module m;
  let file = "tmp.ll" in
  Llvm.print_module file m;
  print_endline "----COMPILE----";
  Sys.command ("llc-17 " ^ file) |> ignore;
  Sys.command "clang-17 -w tmp.s" |> ignore;
  print_endline "----OUTPUT----";
  Printf.printf "----RET -> %d----\n" (Sys.command "./a.out")

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
    ----IR----
    ; ModuleID = 'main'
    source_filename = "main"

    ; Function Attrs: noinline optnone
    define i32 @main(i32 %0, ptr %1) #0 {
    entry:
      %argv = alloca i32, align 4
      store i32 %0, ptr %argv, align 4
      %argc = alloca ptr, align 8
      store ptr %1, ptr %argc, align 8
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

    attributes #0 = { noinline optnone }
    ----RET -> 89----
    |}]

let%expect_test "func" =
  tr_print_exec
    {|  
    int func(int a, int b) {
      return a + 2 * b;
    }

    int main(int argv, char** argc) {
      int p1 = 10;
      int p2 = 20;
      int r = func(p1, p2);
      return r;
    }
  |};
  [%expect
    {|
    ----IR----
    ; ModuleID = 'main'
    source_filename = "main"

    ; Function Attrs: noinline optnone
    define i32 @func(i32 %0, i32 %1) #0 {
    entry:
      %a = alloca i32, align 4
      store i32 %0, ptr %a, align 4
      %b = alloca i32, align 4
      store i32 %1, ptr %b, align 4
      %tmp = load i32, ptr %b, align 4
      %tmp1 = mul i32 2, %tmp
      %tmp2 = load i32, ptr %a, align 4
      %tmp3 = add i32 %tmp2, %tmp1
      ret i32 %tmp3
    }

    ; Function Attrs: noinline optnone
    define i32 @main(i32 %0, ptr %1) #0 {
    entry:
      %argv = alloca i32, align 4
      store i32 %0, ptr %argv, align 4
      %argc = alloca ptr, align 8
      store ptr %1, ptr %argc, align 8
      %p1 = alloca i32, align 4
      store i32 10, ptr %p1, align 4
      %p2 = alloca i32, align 4
      store i32 20, ptr %p2, align 4
      %r = alloca i32, align 4
      %tmp = load i32, ptr %p1, align 4
      %tmp1 = load i32, ptr %p2, align 4
      %funcres = call i32 @func(i32 %tmp, i32 %tmp1)
      store i32 %funcres, ptr %r, align 4
      %tmp2 = load i32, ptr %r, align 4
      ret i32 %tmp2
    }

    attributes #0 = { noinline optnone }
    ----RET -> 50----
    |}]

let%expect_test "printf" =
  tr_compile_run
    {|  
    int printf(char* fmt, ...);

    int main(int argv, char** argc) {
      char* a = "\n\n\t\thehe\n\n";
      int abc = 123123; 
      printf("Hello world! %d %s", abc, a);
      return 0;
    }
  |};
  [%expect
    {|
    ----IR----
    ; ModuleID = 'main'
    source_filename = "main"

    @0 = private unnamed_addr constant [11 x i8] c"\0A\0A\09\09hehe\0A\0A\00", align 1
    @1 = private unnamed_addr constant [19 x i8] c"Hello world! %d %s\00", align 1

    declare i32 @printf(ptr %0, ...)

    ; Function Attrs: noinline optnone
    define i32 @main(i32 %0, ptr %1) #0 {
    entry:
      %argv = alloca i32, align 4
      store i32 %0, ptr %argv, align 4
      %argc = alloca ptr, align 8
      store ptr %1, ptr %argc, align 8
      %a = alloca ptr, align 8
      store ptr @0, ptr %a, align 8
      %abc = alloca i32, align 4
      store i32 123123, ptr %abc, align 4
      %tmp = load i32, ptr %abc, align 4
      %tmp1 = load ptr, ptr %a, align 8
      %printfres = call i32 (ptr, ...) @printf(ptr @1, i32 %tmp, ptr %tmp1)
      ret i32 0
    }

    attributes #0 = { noinline optnone }
    ----COMPILE----
    /usr/bin/ld: /tmp/build_8ad482_dune/tmp-46dca4.o: warning: relocation in read-only section `.text'
    /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
    ----OUTPUT----
    Hello world! 123123

    hehe

    ----RET -> 0----
    |}]
