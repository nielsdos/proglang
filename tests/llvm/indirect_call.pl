# RUN: ../compile %s |& filecheck %s

fn add_one(int x) -> int:
  return x + 1

# CHECK: define i64 @add_one(i64 %0) local_unnamed_addr #0 {
# CHECK-NEXT: entry:
# CHECK-NEXT:   %add = add i64 %0, 1
# CHECK-NEXT:   ret i64 %add
# CHECK-NEXT: }

fn twice(fn (int) -> int callee, int x) -> int:
  return callee(x) + callee(x)

# CHECK: define i64 @twice(ptr %0, i64 %1) local_unnamed_addr {
# CHECK-NEXT: entry:
# CHECK-NEXT:   %indirect_call = tail call i64 %0(i64 %1)
# CHECK-NEXT:   %indirect_call4 = tail call i64 %0(i64 %1)
# CHECK-NEXT:   %add = add i64 %indirect_call4, %indirect_call
# CHECK-NEXT:   ret i64 %add
# CHECK-NEXT: }
