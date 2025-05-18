# RUN: ../compile %s |& FileCheck %s

fn return_or(int a, int b): bool
    return a <= 0 || b <= 0
end

# CHECK: define i1 @return_or(ptr nocapture readnone %0, i64 %1, i64 %2) local_unnamed_addr #0 {
# CHECK-NEXT: entry:
# CHECK-NEXT:   %eq = icmp slt i64 %1, 1
# CHECK-NEXT:   %eq4 = icmp slt i64 %2, 1
# CHECK-NEXT:   %spec.select = select i1 %eq, i1 true, i1 %eq4
# CHECK-NEXT:   ret i1 %spec.select
# CHECK-NEXT: }

