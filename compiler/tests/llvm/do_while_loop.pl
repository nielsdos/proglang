# RUN: ../compile %s |& FileCheck %s

fn do_while_loop(mut int i): int
    mut sum = 0
    do
        sum = sum + i
        i = i + 1
    while i <= 10
    return sum
end

# CHECK: define i64 @do_while_loop(ptr nocapture readnone %0, i64 %1) local_unnamed_addr #{{[0-9]+}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   br label %body
# CHECK-EMPTY:
# CHECK-NEXT: body:                                             ; preds = %body, %entry
# CHECK-DAG:   %{{.+}} = phi i64 [ 0, %entry ], [ %add, %body ]
# CHECK-DAG:   %{{.+}} = phi i64 [ %1, %entry ], [ %add6, %body ]
# CHECK-NEXT:   %add = add i64 %{{.+}}, %{{.+}}
# CHECK-NEXT:   %add6 = add i64 %{{.+}}, 1
# CHECK-NEXT:   %eq = icmp slt i64 %add6, 11
# CHECK-NEXT:   br i1 %eq, label %body, label %after_loop
# CHECK-EMPTY:
# CHECK-NEXT: after_loop:                                       ; preds = %body
# CHECK-NEXT:   ret i64 %add
# CHECK-NEXT: }
