# RUN: ../compile %s |& FileCheck %s

fn foo(): int
  return 1.0
end

# CHECK: {{.+}}: function must return a value of type 'int', but this returns a value of type 'float'
