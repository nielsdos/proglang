# RUN: ../compile %s |& FileCheck %s

fn foo()
  let a = 1
  a = 0
end

# CHECK: {{.+}}: the variable 'a' is immutable
