# RUN: ../compile %s |& filecheck %s

fn foo() -> int:
  return 1

# CHECK: define i64 @foo() local_unnamed_addr #1 {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 1
# CHECK-NEXT: }
