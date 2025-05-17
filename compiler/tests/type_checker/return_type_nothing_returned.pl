# RUN: ../compile %s |& FileCheck %s

fn foo(): int
  return
end

# CHECK: {{.+}}: function must return a value of type 'int', but this returns nothing
