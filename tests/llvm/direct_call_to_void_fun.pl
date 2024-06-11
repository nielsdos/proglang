# RUN: ../compile --optimization-level=0 %s |& FileCheck %s

fn voidfun():
    return

# CHECK-DAG: define void @voidfun() {
# CHECK-DAG: entry:
# CHECK-DAG:   ret void
# CHECK-DAG: }

fn nonvoidfun() -> int:
    voidfun()
    return 1

# CHECK-DAG: define i64 @nonvoidfun() {
# CHECK-DAG: entry:
# CHECK-DAG:   call void @voidfun()
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }
