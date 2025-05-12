# RUN: ../compile %s |& FileCheck %s

fn test(int a, int b, int c) -> int:
    return a - b * c

fn caller() -> int:
    return test(b = 2, c = 10, a = 1)

# CHECK: define i64 @caller() local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 -19
# CHECK-NEXT: }
