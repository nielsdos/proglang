# RUN: ../compile %s |& FileCheck %s

fn do_while_loop_return(): int
    do
        return 1
    while true
    return 0
end

# CHECK: define i64 @do_while_loop_return(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]+}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 1
# CHECK-NEXT: }
