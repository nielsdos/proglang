# RUN: ../compile %s |& FileCheck %s

class Foo:
    int x
    int y
    &Foo f

class X:
    &Foo foo1
    Foo foo2

fn test(&X x) -> int:
    return x.foo1.f.f.x + x.foo2.y

# CHECK: define i64 @test(ptr %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   %ptr_load = load ptr, ptr %0, align 8
# CHECK-NEXT:   %gep2 = getelementptr inbounds %Foo, ptr %ptr_load, i64 0, i32 2
# CHECK-NEXT:   %ptr_load3 = load ptr, ptr %gep2, align 8
# CHECK-NEXT:   %gep4 = getelementptr inbounds %Foo, ptr %ptr_load3, i64 0, i32 2
# CHECK-NEXT:   %ptr_load5 = load ptr, ptr %gep4, align 8
# CHECK-NEXT:   %ptr_load7 = load i64, ptr %ptr_load5, align 4
# CHECK-NEXT:   %gep10 = getelementptr inbounds %X, ptr %0, i64 0, i32 1, i32 1
# CHECK-NEXT:   %ptr_load11 = load i64, ptr %gep10, align 4
# CHECK-NEXT:   %add = add i64 %ptr_load11, %ptr_load7
# CHECK-NEXT:   ret i64 %add
# CHECK-NEXT: }
