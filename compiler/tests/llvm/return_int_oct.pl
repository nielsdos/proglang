# RUN: ../compile %s |& FileCheck %s

fn octal(): int
  return 0o777
end

# CHECK: define i64 @octal(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 511
# CHECK-NEXT: }
