# RUN: ../compile %s |& FileCheck %s

fn loop_return(): int
    while true do
        return 2
    end
    return 0
end

# CHECK: define i64 @loop_return(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]+}} {
# CHECK-NEXT: entry:
# CHECK-NEXT:   ret i64 2
# CHECK-NEXT: }
