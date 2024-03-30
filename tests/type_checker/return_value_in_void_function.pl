# RUN: ../compile %s |& filecheck %s

fn foo():
  return 1.0

# CHECK: {{.+}}: function must return a value of type 'void', but this returns a value of type 'double'
