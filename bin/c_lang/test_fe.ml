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
  Sys.command "clang-17 -w tmp.s > /dev/null 2>&1" |> ignore;
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
    ----OUTPUT----
    Hello world! 123123

    hehe

    ----RET -> 0----
    |}]

let%expect_test "if" =
  tr_compile_run
    {|  
    int printf(char* fmt, ...);
    
    int main(int argv, char** argc) {
      int a = 1;
      int b = 2; 
      if(a > b) {
        printf("1 > 2 (INVALID)\n");
      } else {
        printf("1 < 2 (VALID)\n");
      }
      if(0) {
        printf("1! (INVALID)\n");
      } else {
        printf("0! (VALID)\n");
      }
      if(1) {
        printf("1! (VALID)\n");
      } else {
        printf("0! (INVALID)\n");
      }
      if(10 > (2 * b)) {
        if(1) {
          printf("nested 11! (VALID)\n");
        } else {
          printf("nested 10! (INVALID)\n");
        }
      }
      if(10 < (2 * b)) {
      } else {
        if(0) {
          printf("nested 01! (INVALID)\n");
        } else {
          printf("nested 00! (VALID)\n");
        }
      }
      return 0;
    }
|};
  [%expect
    {|
    ----IR----
    ; ModuleID = 'main'
    source_filename = "main"

    @0 = private unnamed_addr constant [17 x i8] c"1 > 2 (INVALID)\0A\00", align 1
    @1 = private unnamed_addr constant [15 x i8] c"1 < 2 (VALID)\0A\00", align 1
    @2 = private unnamed_addr constant [14 x i8] c"1! (INVALID)\0A\00", align 1
    @3 = private unnamed_addr constant [12 x i8] c"0! (VALID)\0A\00", align 1
    @4 = private unnamed_addr constant [12 x i8] c"1! (VALID)\0A\00", align 1
    @5 = private unnamed_addr constant [14 x i8] c"0! (INVALID)\0A\00", align 1
    @6 = private unnamed_addr constant [20 x i8] c"nested 11! (VALID)\0A\00", align 1
    @7 = private unnamed_addr constant [22 x i8] c"nested 10! (INVALID)\0A\00", align 1
    @8 = private unnamed_addr constant [22 x i8] c"nested 01! (INVALID)\0A\00", align 1
    @9 = private unnamed_addr constant [20 x i8] c"nested 00! (VALID)\0A\00", align 1

    declare i32 @printf(ptr %0, ...)

    ; Function Attrs: noinline optnone
    define i32 @main(i32 %0, ptr %1) #0 {
    entry:
      %argv = alloca i32, align 4
      store i32 %0, ptr %argv, align 4
      %argc = alloca ptr, align 8
      store ptr %1, ptr %argc, align 8
      %a = alloca i32, align 4
      store i32 1, ptr %a, align 4
      %b = alloca i32, align 4
      store i32 2, ptr %b, align 4
      %tmp = load i32, ptr %b, align 4
      %tmp1 = load i32, ptr %a, align 4
      %tmp2 = icmp sgt i32 %tmp1, %tmp
      %ifvalcast = sext i1 %tmp2 to i32
      %ifcond = icmp ne i32 %ifvalcast, 0
      br i1 %ifcond, label %then, label %else

    then:                                             ; preds = %entry
      %printfres = call i32 (ptr, ...) @printf(ptr @0)
      br label %merge

    else:                                             ; preds = %entry
      %printfres3 = call i32 (ptr, ...) @printf(ptr @1)
      br label %merge

    merge:                                            ; preds = %else, %then
      br i1 false, label %then4, label %else6

    then4:                                            ; preds = %merge
      %printfres5 = call i32 (ptr, ...) @printf(ptr @2)
      br label %merge8

    else6:                                            ; preds = %merge
      %printfres7 = call i32 (ptr, ...) @printf(ptr @3)
      br label %merge8

    merge8:                                           ; preds = %else6, %then4
      br i1 true, label %then9, label %else11

    then9:                                            ; preds = %merge8
      %printfres10 = call i32 (ptr, ...) @printf(ptr @4)
      br label %merge13

    else11:                                           ; preds = %merge8
      %printfres12 = call i32 (ptr, ...) @printf(ptr @5)
      br label %merge13

    merge13:                                          ; preds = %else11, %then9
      %tmp14 = load i32, ptr %b, align 4
      %tmp15 = mul i32 2, %tmp14
      %tmp16 = icmp sgt i32 10, %tmp15
      %ifvalcast17 = sext i1 %tmp16 to i32
      %ifcond18 = icmp ne i32 %ifvalcast17, 0
      br i1 %ifcond18, label %then19, label %else25

    then19:                                           ; preds = %merge13
      br i1 true, label %then20, label %else22

    then20:                                           ; preds = %then19
      %printfres21 = call i32 (ptr, ...) @printf(ptr @6)
      br label %merge24

    else22:                                           ; preds = %then19
      %printfres23 = call i32 (ptr, ...) @printf(ptr @7)
      br label %merge24

    merge24:                                          ; preds = %else22, %then20
      br label %merge26

    else25:                                           ; preds = %merge13
      br label %merge26

    merge26:                                          ; preds = %else25, %merge24
      %tmp27 = load i32, ptr %b, align 4
      %tmp28 = mul i32 2, %tmp27
      %tmp29 = icmp slt i32 10, %tmp28
      %ifvalcast30 = sext i1 %tmp29 to i32
      %ifcond31 = icmp ne i32 %ifvalcast30, 0
      br i1 %ifcond31, label %then32, label %else33

    then32:                                           ; preds = %merge26
      br label %merge39

    else33:                                           ; preds = %merge26
      br i1 false, label %then34, label %else36

    then34:                                           ; preds = %else33
      %printfres35 = call i32 (ptr, ...) @printf(ptr @8)
      br label %merge38

    else36:                                           ; preds = %else33
      %printfres37 = call i32 (ptr, ...) @printf(ptr @9)
      br label %merge38

    merge38:                                          ; preds = %else36, %then34
      br label %merge39

    merge39:                                          ; preds = %merge38, %then32
      ret i32 0
    }

    attributes #0 = { noinline optnone }
    ----COMPILE----
    ----OUTPUT----
    1 < 2 (VALID)
    0! (VALID)
    1! (VALID)
    nested 11! (VALID)
    nested 00! (VALID)
    ----RET -> 0----
    |}]
