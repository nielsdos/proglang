# RUN: ../compile %s |& FileCheck %s

fn add_one(int x) -> int:
  return x + 1

# CHECK-DAG: define i64 @add_one(i64 %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   %add = add i64 %0, 1
# CHECK-DAG:   ret i64 %add
# CHECK-DAG: }

fn twice(fn (int) -> int callee, int x) -> int:
  return callee(x) + callee(x)

# CHECK-DAG: define i64 @twice(ptr %0, i64 %1) local_unnamed_addr {
# CHECK-DAG: entry:
# CHECK-DAG:   %indirect_call = tail call i64 %0(i64 %1)
# CHECK-DAG:   %indirect_call6 = tail call i64 %0(i64 %1)
# CHECK-DAG:   %add = add i64 %indirect_call6, %indirect_call
# CHECK-DAG:   ret i64 %add
# CHECK-DAG: }
