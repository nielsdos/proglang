# RUN: ../compile %s |& FileCheck %s

fn infinite_loop(mut int i): int
    loop
        if i > 10 then
            return i
        end
        i = i + 1
    end
    return 0
end

# CHECK-DAG: define i64 @infinite_loop(ptr nocapture readnone %0, i64 %1) local_unnamed_addr #{{[0-9]+}} {
# CHECK-DAG: entry:
# CHECK-DAG:   br label %condition
# CHECK-DAG: condition:                                        ; preds = %condition, %entry
# CHECK-DAG:   %var.0 = phi i64 [ %1, %entry ], [ %add, %condition ]
# CHECK-DAG:   %eq = icmp sgt i64 %var.0, 10
# CHECK-DAG:   %add = add i64 %var.0, 1
# CHECK-DAG:   br i1 %eq, label %then, label %condition
# CHECK-DAG: then:                                             ; preds = %condition
# CHECK-DAG:   ret i64 %var.0
# CHECK-DAG: }
