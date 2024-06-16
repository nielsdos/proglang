# RUN: ../compile %s |& FileCheck %s

fn decimal() -> int:
  return 1

# CHECK-DAG: define i64 @decimal() local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }

fn hex() -> int:
  return 0xdeadbeef

# CHECK-DAG: define i64 @hex() local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 3735928559
# CHECK-DAG: }

fn octal() -> int:
  return 0o777

# CHECK-DAG: define i64 @octal() local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 511
# CHECK-DAG: }

fn binary() -> int:
  return 0b1110

# CHECK-DAG: define i64 @binary() local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 14
# CHECK-DAG: }
