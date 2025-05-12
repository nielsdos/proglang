# RUN: ../compile %s |& FileCheck %s

fn foo() -> int:
  return

# CHECK: {{.+}}: function must return a value of type 'int', but this returns nothing
