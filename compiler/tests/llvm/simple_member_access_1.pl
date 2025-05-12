# RUN: ../compile %s |& FileCheck %s

class Foo:
    int x
    int y

fn test(Foo f) -> int:
    return f.x + f.y

# CHECK: define i64 @test(%Foo %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   %.elt = extractvalue %Foo %0, 0
# CHECK-NEXT:   %.elt4 = extractvalue %Foo %0, 1
# CHECK-NEXT:   %add = add i64 %.elt, %.elt4
# CHECK-NEXT:   ret i64 %add
