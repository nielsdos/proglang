# RUN: ../compile %s |& FileCheck %s

fn foo()
  return
  let a = 1
end

# CHECK: Note: {{.+}}: this is the last non-dead statement in this block
# CHECK-NEXT: {{.+}}: therefore, this statement and any following statements in this block are unreachable

