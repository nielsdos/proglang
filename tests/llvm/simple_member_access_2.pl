# RUN: ../compile %s |& FileCheck %s

class Foo:
    int x
    int y

fn test(&Foo f) -> int:
    return f.x + f.y

# CHECK: define i64 @test(ptr %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   %ptr_load = load i64, ptr %0, align 4
# CHECK-NEXT:   %gep3 = getelementptr inbounds %Foo, ptr %0, i64 0, i32 1
# CHECK-NEXT:   %ptr_load4 = load i64, ptr %gep3, align 4
# CHECK-NEXT:   %add = add i64 %ptr_load4, %ptr_load
# CHECK-NEXT:   ret i64 %add
# CHECK-NEXT: }
