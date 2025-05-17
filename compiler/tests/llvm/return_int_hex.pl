# RUN: ../compile %s |& FileCheck %s

fn hex(): int
  return 0xdeadbeef
end

# CHECK: define i64 @hex(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 3735928559
# CHECK-NEXT: }
