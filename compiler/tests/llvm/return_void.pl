# RUN: ../compile %s |& FileCheck %s

fn foo()
  return
end

# CHECK: define void @foo(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret
# CHECK-NEXT: }
