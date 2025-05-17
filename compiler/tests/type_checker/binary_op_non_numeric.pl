# RUN: ../compile %s |& FileCheck %s

fn foo(): int
  return 1 + true
end

# CHECK: {{.+}}: expected both operands to be numeric, but the left-hand side has type 'int' and the right-hand side has type 'bool'
