# RUN: ../compile %s |& filecheck %s

fn foo() -> int:
  foo()

# CHECK: {{.+}}: function 'foo' does not always return a value
