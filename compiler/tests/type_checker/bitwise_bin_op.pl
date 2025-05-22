# RUN: ../compile %s |& FileCheck %s

fn foo1(float x): int
  return 1 | x
end

fn foo2(float x): int
  return 1 ^ x
end

fn foo3(float x): int
  return 1 & x
end

# CHECK: {{.+}}: expected both operands to be of type 'int', but the left-hand side has type 'int' and the right-hand side has type 'float'
# CHECK: {{.+}}: expected both operands to be of type 'int', but the left-hand side has type 'int' and the right-hand side has type 'float'
# CHECK: {{.+}}: expected both operands to be of type 'int', but the left-hand side has type 'int' and the right-hand side has type 'float'
