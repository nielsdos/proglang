# RUN: ../compile %s |& filecheck %s

fn foo():
  return 1.0

# CHECK: {{.+}}: function must not return a value because its return type is void
