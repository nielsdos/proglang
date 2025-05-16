use gc_arena::lock::RefLock;
use gc_arena::{Arena, Collect, Gc, Mutation, Rootable};
use std::collections::HashMap;
use std::slice;

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub enum DynamicValue<'gc> {
    Int(i64),
    Table(TablePtr<'gc>),
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct Table<'gc> {
    map: HashMap<String, DynamicValue<'gc>>,
}

type TablePtr<'gc> = Gc<'gc, RefLock<Table<'gc>>>;

type GcArena = Arena<Rootable![VirtualCallFrame<'_>]>;

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct VirtualCallFrame<'gc> {
    slots: Vec<TablePtr<'gc>>,
}

fn new_table<'gc>(mc: &Mutation<'gc>, capacity: usize) -> TablePtr<'gc> {
    Gc::new(
        mc,
        RefLock::new(Table {
            map: HashMap::with_capacity(capacity),
        }),
    )
}

#[no_mangle]
pub extern "C" fn rt_create_table(arena: &mut GcArena, capacity: i64) -> i64 {
    arena.mutate_root(|mc, root| {
        let table = new_table(mc, capacity as usize);
        let idx = root.slots.len();
        root.slots.push(table);
        idx as i64
    })
}

#[no_mangle]
pub unsafe extern "C" fn rt_get_from_table(arena: &GcArena, handle: i64, bytes: *const u8, len: usize) -> i64 {
    // TODO: avoid creating copies
    let bytes = slice::from_raw_parts(bytes, len);
    let str = String::from_utf8(bytes.into()).unwrap(); // TODO

    arena.mutate(|_mc, root| {
        let table = root.slots[handle as usize];
        match table.borrow().map.get(&str) {
            Some(DynamicValue::Int(i)) => *i,
            _ => panic!(),
        }
    })
}

#[no_mangle]
pub unsafe extern "C" fn rt_set_in_table(arena: &GcArena, handle: i64, bytes: *const u8, len: usize, value: i64) {
    let bytes = slice::from_raw_parts(bytes, len);
    let str = String::from_utf8(bytes.into()).unwrap(); // TODO

    rt_set_in_table_ex(arena, handle, str, value)
}

fn rt_set_in_table_ex(arena: &GcArena, handle: i64, str: String, value: i64) {
    arena.mutate(|mc, root| {
        let table = root.slots[handle as usize];
        table.borrow_mut(mc).map.insert(str, DynamicValue::Int(value));
    });
}

extern "C" {
    pub fn test_table(_: &mut GcArena) -> i64;
}

#[no_mangle]
pub unsafe extern "C" fn rt_main() {
    let mut arena = GcArena::new(|_mc| VirtualCallFrame { slots: vec![] });
    let res = test_table(&mut arena);
    println!("res {}", res);
}
