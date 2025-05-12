# RUN: ../compile --optimization-level=0 %s |& FileCheck %s

fn foo() -> int:
  return bar()

# CHECK-DAG: define i64 @foo() {
# CHECK-DAG: entry:
# CHECK-DAG:   %direct_call = call i64 @bar()
# CHECK-DAG:   ret i64 %direct_call
# CHECK-DAG: }

fn bar() -> int:
  return 1

# CHECK-DAG: define i64 @bar() {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }
