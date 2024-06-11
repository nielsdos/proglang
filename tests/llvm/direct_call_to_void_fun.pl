# RUN: ../compile --optimization-level=0 %s |& FileCheck %s

fn voidfun():
    return

# CHECK-DAG: define void @voidfun() local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret void
# CHECK-DAG: }

fn nonvoidfun() -> int:
    voidfun()
    return 1

# CHECK-DAG: define i64 @nonvoidfun() local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }
