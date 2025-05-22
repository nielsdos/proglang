# RUN: ../compile %s |& FileCheck %s

fn foo1(float x): int
  return ~x
end

# CHECK: {{.+}}: unary operator '~' expects a value of type 'int', found a value of type 'float' instead
