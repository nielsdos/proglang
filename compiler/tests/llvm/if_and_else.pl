# RUN: ../compile %s |& FileCheck %s

fn rec(int x): int
    if x <= 1 then return 1 end
    return x * rec(x-1)
end

fn if_and_else(int a, int b): int
    if a > 0 && rec(b) > 16 then
        return rec(a)
    else
        return rec(b)
    end
end

# CHECK: define i64 @if_and_else(ptr nocapture readnone %0, i64 %1, i64 %2) local_unnamed_addr #{{.+}} {
# CHECK-DAG: entry:
# CHECK-DAG:   %eq = icmp sgt i64 %1, 0
# CHECK-DAG:   br i1 %eq, label %merge, label %else
# CHECK-DAG: merge:                                            ; preds = %entry
# CHECK-DAG:   %direct_call = tail call i64 @rec(ptr %0, i64 %2)
# CHECK-DAG:   %eq4 = icmp sgt i64 %direct_call, 16
# CHECK-DAG:   br i1 %eq4, label %then, label %else
# CHECK-DAG: common.ret:                                       ; preds = %else, %then
# CHECK-DAG:   %common.ret.op = phi i64 [ %direct_call6, %then ], [ %direct_call8, %else ]
# CHECK-DAG:   ret i64 %common.ret.op
# CHECK-DAG: then:                                             ; preds = %merge
# CHECK-DAG:   %direct_call6 = tail call i64 @rec(ptr %0, i64 %1)
# CHECK-DAG:   br label %common.ret
# CHECK-DAG: else:                                             ; preds = %entry, %merge
# CHECK-DAG:   %direct_call8 = tail call i64 @rec(ptr %0, i64 %2)
# CHECK-DAG:   br label %common.ret
# CHECK-DAG: }
