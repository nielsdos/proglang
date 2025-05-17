# RUN: ../compile %s |& FileCheck %s

fn binary(): int
  return 0b1110
end

# CHECK: define i64 @binary(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 14
# CHECK-NEXT: }
