# RUN: ../compile --optimization-level=0 %s |& filecheck %s

fn bar() -> int:
  return 1

# CHECK: define i64 @bar() {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 1
# CHECK-NEXT: }

fn foo() -> int:
  return bar()

# CHECK: define i64 @foo() {
# CHECK-NEXT: entry:
# CHECK-NEXT:   %direct_call = call i64 @bar()
# CHECK-NEXT:   ret i64 %direct_call
# CHECK-NEXT: }
