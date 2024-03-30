# RUN: ../compile %s |& filecheck %s

fn foo():
  if false:
    mut x = 1
  x = 2

# CHECK: {{.+}}: identifier 'x' was not found in the current scope
