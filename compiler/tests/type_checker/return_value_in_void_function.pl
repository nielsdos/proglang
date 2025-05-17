# RUN: ../compile %s |& FileCheck %s

fn foo()
  return 1.0
end

# CHECK: {{.+}}: function must not return a value because its return type is void
