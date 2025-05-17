# RUN: ../compile %s |& FileCheck %s

fn hex(): int
    return 0xaz
end

# CHECK: {{.+}}: identifier 'z' was not found in the current scope
