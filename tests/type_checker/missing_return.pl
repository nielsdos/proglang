# RUN: ../compile %s |& FileCheck %s

fn foo() -> int:
  foo()

# CHECK: {{.+}}: function 'foo' does not always return a value
