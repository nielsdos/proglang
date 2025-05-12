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

fn do_while_loop_return() -> int:
    do:
        return 1
    while true
    return 0

# CHECK-DAG: define i64 @do_while_loop_return() local_unnamed_addr #{{[0-9]+}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }

fn do_while_loop(mut int i) -> int:
    mut sum = 0
    do:
        sum = sum + i
        i = i + 1
    while i <= 10
    return sum

# CHECK-DAG: define i64 @do_while_loop(i64 %0) local_unnamed_addr #{{[0-9]+}} {
# CHECK-DAG: entry:
# CHECK-DAG:   br label %body
# CHECK-DAG: body:                                             ; preds = %body, %entry
# CHECK-DAG:   %var1.0 = phi i64 [ 0, %entry ], [ %add, %body ]
# CHECK-DAG:   %var.0 = phi i64 [ %0, %entry ], [ %add6, %body ]
# CHECK-DAG:   %add = add i64 %var.0, %var1.0
# CHECK-DAG:   %add6 = add i64 %var.0, 1
# CHECK-DAG:   %eq = icmp slt i64 %add6, 11
# CHECK-DAG:   br i1 %eq, label %body, label %after_loop
# CHECK-DAG: after_loop:                                       ; preds = %body
# CHECK-DAG:   ret i64 %add
# CHECK-DAG: }
