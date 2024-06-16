# RUN: ../compile %s |& FileCheck %s

fn loop_idiom() -> int:
    mut sum = 0
    mut i = 0
    while i <= 10:
        sum = sum + i
        i = i + 1
    return sum

# CHECK-DAG: define i64 @loop_idiom() local_unnamed_addr #{{[0-9]+}} {
# CHECK-DAG: after_loop:
# CHECK-DAG:   ret i64 55
# CHECK-DAG: }

fn loop_return() -> int:
    while true:
        return 1
    return 0

# CHECK-DAG: define i64 @loop_return() local_unnamed_addr #{{[0-9]+}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }
