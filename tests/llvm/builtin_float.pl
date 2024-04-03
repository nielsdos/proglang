# RUN: ../compile %s |& FileCheck %s

fn test() -> float:
  return float(123)

# CHECK: define double @test() local_unnamed_addr #{{[0-9]}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret double 1.230000e+02
# CHECK-NEXT: }
