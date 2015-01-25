#![allow(dead_code)]
#![allow(unstable)]

#![feature(box_syntax)]
#![feature(unsafe_destructor)]

extern crate core;

#[macro_use]
extern crate log;

mod builder;
mod context;
mod imm_str;
mod ilist;
mod ir;
#[macro_use]
mod pattern_match;
mod ty;

mod util {
    pub mod constant_fold;
}

fn main() {
    use builder::IRBuilder;

    let cx = context::Context::new();

    let args = box [cx.types.u8, cx.types.bool];
    let ret = cx.types.usize;

    let s = ty::Ty::struct_(&*cx, "TestTy", box [cx.types.u8, cx.types.bool]);

    let builder = IRBuilder::new(cx);

    let fn_builder = builder.start_function("test", args, ret);
    fn_builder.set_arg_name(0, "a");
    fn_builder.set_arg_name(1, "b");

    {
        let cx = fn_builder.cx();
        let entry = fn_builder.new_block("entry");

        let a0 = fn_builder.arg(0);
        let a1 = fn_builder.arg(1);

        let zero = fn_builder.const_u8(0);
        let one = fn_builder.const_u8(1);

        let add = fn_builder.add(entry, Some("add"), one.clone(), zero.clone());

        let eq = fn_builder.cmp(entry, Some("eq"), ir::Cmp::Eq, add, one.clone());
        fn_builder.and(entry, Some("and"), eq, a1);

        let sub = fn_builder.sub(entry, Some("sub"), zero, a0.clone());
        let ret = fn_builder.add(entry, Some("ret"), a0, sub);

        fn_builder.ret(entry, ret);
    }

    let ccx = fn_builder.finish().unwrap();

    println!("{:?}", ccx);
}
