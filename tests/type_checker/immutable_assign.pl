# RUN: ../compile %s |& filecheck %s

fn foo():
  let a = 1
  a = 0

# CHECK: {{.+}}: the variable 'a' is immutable
