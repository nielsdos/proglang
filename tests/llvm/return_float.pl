# RUN: ../compile %s |& filecheck %s

fn foo() -> float:
  return 1.0

# CHECK: define double @foo() local_unnamed_addr #0 {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret double 1.000000e+00
# CHECK-NEXT: }
