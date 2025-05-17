# RUN: ../compile %s |& FileCheck %s

fn foo(): int
  foo()
end

# CHECK: {{.+}}: function 'foo' does not always return a value
