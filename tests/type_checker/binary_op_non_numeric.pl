# RUN: ../compile %s |& filecheck %s

fn foo() -> int:
  return 1 + true

# CHECK: {{.+}}: expected both operands to be numeric, but the left-hand side has type 'int' and the right-hand side has type 'bool'
