# RUN: ../compile %s |& FileCheck %s

fn test(int a, int b, int c): int
    return a - b * c
end

fn caller(): int
    return test(b = 2, c = 10, a = 1)
end

# CHECK: define i64 @caller(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 -19
# CHECK-NEXT: }
