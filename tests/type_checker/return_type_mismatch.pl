# RUN: ../compile %s |& filecheck %s

fn foo() -> int:
  return 1.0

# CHECK: {{.+}}: function must return a value of type 'int', but this returns a value of type 'float'
