# RUN: ../compile %s |& FileCheck %s

fn if_check(): int
    if 0 then
        return 1
    end
    return 0
end

# CHECK: {{.+}}: expected a condition of type 'bool', but this condition has type 'int'

fn while_loop(): int
    while 0 do
        return 1
    end
    return 0
end

# CHECK: {{.+}}: expected a condition of type 'bool', but this condition has type 'int'

fn do_while_loop(): int
    do
        return 1
    while 0
    return 0
end

# CHECK: {{.+}}: expected a condition of type 'bool', but this condition has type 'int'
