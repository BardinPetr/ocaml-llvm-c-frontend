@.str = private constant [3 x i8] c"H2\00"

declare i32 @printf(ptr noundef, ...) #1

define i32 @main(i32 %0, ptr %1) noinline optnone {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  store ptr %1, ptr %5, align 8
  %6 = call i32 @printf(ptr @.str)
  ret i32 -1
}

