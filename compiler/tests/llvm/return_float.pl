# RUN: ../compile %s |& FileCheck %s

fn foo(): float
  return 1.0
end

# CHECK: define double @foo(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret double 1.000000e+00
# CHECK-NEXT: }
