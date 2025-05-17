# RUN: ../compile %s |& FileCheck %s

fn decimal(): int
  return 1
end

# CHECK: define i64 @decimal(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 1
# CHECK-NEXT: }
