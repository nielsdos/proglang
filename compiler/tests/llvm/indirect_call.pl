# RUN: ../compile %s |& FileCheck %s

fn add_one(int x): int
  return x + 1
end

# CHECK-DAG: define i64 @add_one(ptr nocapture readnone %0, i64 %1) local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   %add = add i64 %1, 1
# CHECK-DAG:   ret i64 %add
# CHECK-DAG: }

fn twice(fn (int): int callee, int x): int
  return callee(x) + callee(x)
end

# CHECK-DAG: define i64 @twice(ptr %0, ptr %1, i64 %2) local_unnamed_addr {
# CHECK-DAG: entry:
# CHECK-DAG:   %indirect_call = tail call i64 %1(ptr %0, i64 %2)
# CHECK-DAG:   %indirect_call6 = tail call i64 %1(ptr %0, i64 %2)
# CHECK-DAG:   %add = add i64 %indirect_call6, %indirect_call
# CHECK-DAG:   ret i64 %add
# CHECK-DAG: }
