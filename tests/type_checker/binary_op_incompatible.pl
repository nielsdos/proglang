# RUN: ../compile %s |& filecheck %s

fn foo() -> int:
  return 1 + 1.0

# CHECK: {{.+}}: expected both operands to have the same type, but the left-hand side has type 'int' and the right-hand side has type 'float'
