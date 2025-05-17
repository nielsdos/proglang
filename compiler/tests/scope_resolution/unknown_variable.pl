# RUN: ../compile %s |& FileCheck %s

fn foo()
  if false then
    mut x = 1
  end
  x = 2
end

# CHECK: {{.+}}: identifier 'x' was not found in the current scope
