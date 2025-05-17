# RUN: ../compile %s |& FileCheck %s

fn oct(): int
    return 0o19
end

# CHECK: {{.+}}: this is the last non-dead statement in this block
# CHECK: {{.+}}: therefore, this statement and any following statements in this block are unreachable
