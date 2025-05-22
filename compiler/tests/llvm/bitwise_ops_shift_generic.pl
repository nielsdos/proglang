# RUN: ../compile %s |& FileCheck %s

fn lshift(int x, int y): int
    return x << y
end

fn rshift(int x, int y): int
    return x >> y
end

fn rashift(int x, int y): int
    return x >>> y
end

#CHECK-DAG: define i64 @lshift(ptr nocapture readnone %0, i64 %1, i64 %2) local_unnamed_addr #{{.+}} {
#CHECK-DAG: entry:
#CHECK-DAG:   %shift_cmp = icmp ugt i64 %2, 63
#CHECK-DAG:   %lshift = shl i64 %1, %2
#CHECK-DAG:   %shift_select = select i1 %shift_cmp, i64 0, i64 %lshift
#CHECK-DAG:   ret i64 %shift_select
#CHECK-DAG: }

#CHECK-DAG: define i64 @rshift(ptr nocapture readnone %0, i64 %1, i64 %2) local_unnamed_addr #{{.+}} {
#CHECK-DAG: entry:
#CHECK-DAG:   %shift_cmp = icmp ugt i64 %2, 63
#CHECK-DAG:   %rshift = lshr i64 %1, %2
#CHECK-DAG:   %shift_select = select i1 %shift_cmp, i64 0, i64 %rshift
#CHECK-DAG:   ret i64 %shift_select
#CHECK-DAG: }

#CHECK-DAG: define i64 @rashift(ptr nocapture readnone %0, i64 %1, i64 %2) local_unnamed_addr #{{.+}} {
#CHECK-DAG: entry:
#CHECK-DAG:   %shift_cmp = icmp ugt i64 %2, 63
#CHECK-DAG:   %rshift = ashr i64 %1, %2
#CHECK-DAG:   %shift_select = select i1 %shift_cmp, i64 0, i64 %rshift
#CHECK-DAG:   ret i64 %shift_select
#CHECK-DAG: }
