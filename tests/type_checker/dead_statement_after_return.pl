# RUN: ../compile %s |& filecheck %s

fn foo():
  return
  let a = 1

# CHECK: Note: {{.+}}: this is the last non-dead statement in this block
# CHECK-NEXT: {{.+}}: therefore, this statement and any following statements in this block are unreachable

