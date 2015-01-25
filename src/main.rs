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
    let ret = cx.types.u8;

    let s = ty::Ty::struct_(&*cx, "TestTy", box [cx.types.u8, cx.types.bool]);

    let builder = IRBuilder::new(cx);

    let f = builder.start_function("test", args, ret);
    f.set_arg_name(0, "a");
    f.set_arg_name(1, "b");

    {
        let cx = f.cx();

        let entry = f.new_block("entry");
        let then = f.new_block("then");
        let els = f.new_block("els");
        let join = f.new_block("join");

        let one = f.const_u8(1);

        let retval = f.alloca(entry, Some("retval"), cx.types.u8);

        f.condbr(entry, f.arg(1), then, els);

        let sub = f.sub(then, Some("sub"), f.arg(0), one.clone());
        f.store(then, retval.clone(), sub);
        f.br(then, join);

        let add = f.add(els, Some("add"), f.arg(0), one);
        f.store(els, retval.clone(), add);
        f.br(els, join);

        let ret = f.load(join, Some("ret"), retval);
        f.ret(join, ret);
    }

    let ccx = f.finish().unwrap();

    println!("{:?}", ccx);
}
