# RUN: ../compile %s |& filecheck %s

fn foo():
  return

# CHECK: define void @foo() local_unnamed_addr #0 {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret
# CHECK-NEXT: }
