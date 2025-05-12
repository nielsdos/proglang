# RUN: ../compile %s |& FileCheck %s

fn voidfun():
    return

# CHECK-DAG: define void @voidfun() local_unnamed_addr #{{[0-9]+}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret void
# CHECK-DAG: }

fn nonvoidfun(fn () x) -> int:
    x()
    return 1

# CHECK-DAG: define i64 @nonvoidfun(ptr %0) local_unnamed_addr {
# CHECK-DAG: entry:
# CHECK-DAG:   tail call void %0()
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }
