# RUN: ../compile --optimization-level=0 %s |& FileCheck %s

fn foo(): int
  return bar()
end

# CHECK-DAG: define i64 @foo(ptr %0) {
# CHECK-DAG: entry:
# CHECK-DAG:   %direct_call = call i64 @bar(ptr %0)
# CHECK-DAG:   ret i64 %direct_call
# CHECK-DAG: }

fn bar(): int
  return 1
end

# CHECK-DAG: define i64 @bar(ptr %0) {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }
