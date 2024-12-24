; ModuleID = 'main'
source_filename = "main"

@0 = private unnamed_addr constant [11 x i8] c"\0A\0A\09\09hehe\0A\0A\00", align 1
@1 = private unnamed_addr constant [19 x i8] c"Hello world! %d %s\00", align 1

declare i32 @printf(ptr, ...)

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
