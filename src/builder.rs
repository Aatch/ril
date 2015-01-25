
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::marker;
use std::borrow::BorrowFrom;
use context::Context;
use imm_str::{ImmString,IntoImmString};
use ir;
use ty;

pub struct IRBuilder<B> {
    cx: Box<Context>,
    b: B
}

impl<T> IRBuilder<T> {
    pub fn cx<'a>(&'a self) -> &'a Context {
        &*self.cx
    }

    pub fn get_function(&self, name: &str) -> Option<ValueHandle> {
        self.cx.get_function(name).map(|f| {
            ValueHandle {
                val: ir::Value::func(f),
                lt: marker::ContravariantLifetime
            }
        })
    }

    pub fn decl_function<'a, S>(&'a self, name: S,
                            args: Box<[ty::TyRef]>, ret: ty::TyRef) -> ValueHandle<'a>
    where S : IntoImmString, str: BorrowFrom<S> {
        let fty = ty::Ty::fn_(&*self.cx, args, ty::FnOutput::Converging(ret));

        let fp = self.decl_fn_inner(name, fty);

        ValueHandle {
            val: ir::Value::func(fp),
            lt: marker::ContravariantLifetime
        }
    }

    fn decl_fn_inner<S>(&self, name: S, ty: ty::TyRef) -> *mut ir::Function
    where S : IntoImmString, str: BorrowFrom<S> {
        {
            let name_s : &str = BorrowFrom::borrow_from(&name);
            if let Some(f) = self.cx.get_function(name_s) {
                let exist_fty = unsafe { (*f).get_type() };
                assert!(exist_fty == ty, "Function exists with same name, but different type");
                return f;
            }
        }

        let mut f = ir::Function::new(name, ty);
        let fp = &mut *f as *mut _;

        self.cx.insert_function(f);

        fp
    }

    fn con<'a>(&'a self, c: *mut ir::Constant) -> ValueHandle<'a> {
        ValueHandle {
            val: ir::Value::constant(c),
            lt: marker::ContravariantLifetime,
        }
    }

    pub fn const_nil<'a>(&'a self) -> ValueHandle<'a> {
        self.con(ir::Constant::nil(&*self.cx))
    }

    pub fn const_i8<'a>(&'a self, val: i8) -> ValueHandle<'a> {
        self.con(ir::Constant::i8(&*self.cx, val))
    }
    pub fn const_i16<'a>(&'a self, val: i16) -> ValueHandle<'a> {
        self.con(ir::Constant::i16(&*self.cx, val))
    }
    pub fn const_i32<'a>(&'a self, val: i32) -> ValueHandle<'a> {
        self.con(ir::Constant::i32(&*self.cx, val))
    }
    pub fn const_i64<'a>(&'a self, val: i64) -> ValueHandle<'a> {
        self.con(ir::Constant::i64(&*self.cx, val))
    }
    pub fn const_isize<'a>(&'a self, val: isize) -> ValueHandle<'a> {
        self.con(ir::Constant::isize(&*self.cx, val))
    }

    pub fn const_u8<'a>(&'a self, val: u8) -> ValueHandle<'a> {
        self.con(ir::Constant::u8(&*self.cx, val))
    }
    pub fn const_u16<'a>(&'a self, val: u16) -> ValueHandle<'a> {
        self.con(ir::Constant::u16(&*self.cx, val))
    }
    pub fn const_u32<'a>(&'a self, val: u32) -> ValueHandle<'a> {
        self.con(ir::Constant::u32(&*self.cx, val))
    }
    pub fn const_u64<'a>(&'a self, val: u64) -> ValueHandle<'a> {
        self.con(ir::Constant::u64(&*self.cx, val))
    }
    pub fn const_usize<'a>(&'a self, val: usize) -> ValueHandle<'a> {
        self.con(ir::Constant::usize(&*self.cx, val))
    }
}

impl IRBuilder<()> {
    pub fn new(cx: Box<Context>) -> IRBuilder<()> {
        IRBuilder {
            cx: cx,
            b: ()
        }
    }

    pub fn unwrap(self) -> Box<Context> {
        self.cx
    }

    pub fn start_function<S>(self, name: S,
                          args: Box<[ty::TyRef]>,
                          ret: ty::TyRef) -> IRBuilder<Function>
    where S : IntoImmString, str: BorrowFrom<S> {
        let fty = ty::Ty::fn_(&*self.cx, args, ty::FnOutput::Converging(ret));
        let f = self.decl_fn_inner(name, fty);

        unsafe {
            assert!(!(*f).has_body(), "Function already created");
        }

        IRBuilder {
            cx: self.cx,
            b: Function {
                f: f,
                inst_names: RefCell::new(HashMap::new()),
                block_names: RefCell::new(HashMap::new()),
            },
        }
    }
}

pub struct Function {
    f: *mut ir::Function,
    inst_names: RefCell<HashMap<ImmString, usize>>,
    block_names: RefCell<HashMap<ImmString, usize>>,
}

#[derive(Copy)]
pub struct BlockHandle<'a> {
    blk: *mut ir::BasicBlock,
    lt: marker::ContravariantLifetime<'a>
}

#[derive(Clone)]
pub struct ValueHandle<'a> {
    pub val: ir::Value,
    lt: marker::ContravariantLifetime<'a>
}

macro_rules! binops {
    ($($(#[$attr:meta])* op $mname:ident: $opname:ident,)+) => {
        impl IRBuilder<Function> {
            $(
            $(#[$attr])*
            pub fn $mname<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>,
                                 l: ValueHandle<'a>, r: ValueHandle<'a>) -> ValueHandle<'a>
                where S: IntoImmString {
                    let mut l = l.val;
                    let mut r = r.val;

                    let lty = l.get_type();
                    let rty = r.get_type();

                    assert!(lty == rty, "Operands are different types");

                    let l_use = l.add_use();
                    let r_use = r.add_use();

                    let op = ir::Op::$opname(l_use, r_use);

                    self.add_instruction(blk, name, op, lty)
            }
            )+
        }
    }
}

binops! {
    op add: Add,
    op sub: Sub,
    op mul: Mul,
    op div: Div,
    op rem: Rem,
    op and: And,
    op or:  Or,
    op xor: Xor,
}

impl IRBuilder<Function> {
    pub fn not<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>,
                      val: ValueHandle<'a>) -> ValueHandle<'a> where S: IntoImmString {
        let mut val = val.val;
        let u = val.add_use();
        let op = ir::Op::Not(u);

        let ty = val.get_type();

        self.add_instruction(blk, name, op, ty)
    }
    pub fn cmp<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>, cmp: ir::Cmp,
                         l: ValueHandle<'a>, r: ValueHandle<'a>) -> ValueHandle<'a>
    where S: IntoImmString {
        let mut l = l.val;
        let mut r = r.val;

        let lty = l.get_type();
        let rty = r.get_type();

        assert!(lty == rty, "Operands are different types");

        let l_use = l.add_use();
        let r_use = r.add_use();

        let op = ir::Op::Cmp(cmp, l_use, r_use);

        self.add_instruction(blk, name, op, self.cx.types.bool)
    }

    pub fn br<'a>(&'a self, blk: BlockHandle<'a>, goto: BlockHandle<'a>) {
        let goto = goto.blk;
        let op = ir::Op::Br(goto);

        self.add_instruction(blk, None::<&'static str>, op, self.cx.types.nil);
    }

    pub fn condbr<'a>(&'a self, blk: BlockHandle<'a>,
                         val: ValueHandle<'a>,
                         then: BlockHandle<'a>, els: BlockHandle<'a>) {
        let then = then.blk;
        let els = els.blk;
        let mut val = val.val;

        let ty = val.get_type();
        assert!(ty == self.cx.types.bool, "Branch condition not a boolean value");

        let u = val.add_use();

        let op = ir::Op::CondBr(u, then, els);

        self.add_instruction(blk, None::<&'static str>, op, self.cx.types.nil);
    }

    pub fn ret<'a>(&'a self, blk: BlockHandle<'a>, val: ValueHandle<'a>) {
        let mut val = val.val;

        let u = val.add_use();
        let op = ir::Op::Return(u);
        self.add_instruction(blk, None::<&'static str>, op, self.cx.types.nil);
    }

    pub fn call<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>,
                       f: ValueHandle<'a>,
                       args: &[ValueHandle<'a>]) -> ValueHandle<'a>
    where S: IntoImmString {

        let mut f = f.val;

        let retty = match &*f.get_type() {
            &ty::Ty::Fn(_, ref out) => match out {
                &ty::FnOutput::Diverging => self.cx.types.nil,
                &ty::FnOutput::Converging(ty) => ty
            },
            _ => panic!("Call function is not a function type")
        };

        let fu = f.add_use();
        let arg_uses = args.iter().map(|a| {
            let mut a = a.clone();
            a.val.add_use()
        }).collect::<Vec<_>>().into_boxed_slice();

        let op = ir::Op::Call(fu, arg_uses);

        self.add_instruction(blk, name, op, retty)
    }

    pub fn alloca<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>,
                        ty: ty::TyRef) -> ValueHandle<'a>
    where S: IntoImmString {
        let op = ir::Op::Alloca(ty);
        let ity = ty::Ty::rptr(&*self.cx, true, ty);
        self.add_instruction(blk, name, op, ity)
    }

    pub fn store<'a>(&self, blk: BlockHandle<'a>,
                     dst: ValueHandle<'a>, val: ValueHandle<'a>) {
        let mut dst = dst.val;
        let mut val = val.val;

        let dst_ty = &*dst.get_type();
        let val_ty = val.get_type();

        assert!(dst_ty.is_pointer(), "Destination ({:?}) is not a pointer", dst_ty);
        assert!(dst_ty.pointee() == val_ty, "Type Mismatch: {:?} != {:?}",
                dst_ty.pointee(), val_ty);
        let op = ir::Op::Store(dst.add_use(), val.add_use());

        self.add_instruction(blk, None::<&'static str>, op, val_ty);
    }

    pub fn load<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>,
                       src: ValueHandle<'a>) -> ValueHandle<'a>
    where S: IntoImmString {
        let mut src = src.val;
        let src_ty = src.get_type();

        assert!(src_ty.is_pointer(), "Source is not a pointer");

        let ty = src_ty.pointee();

        let op = ir::Op::Load(src.add_use());
        self.add_instruction(blk, name, op, ty)
    }

    pub fn getfieldptr<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>, mutbl: bool,
                              val: ValueHandle<'a>, fld: u32) -> ValueHandle<'a>
    where S: IntoImmString {
        let mut val = val.val;
        let val_ty = val.get_type();

        assert!(val_ty.is_pointer(), "Value is not a pointer ({:?} != pointer type)",
                val_ty);
        let pointee = val_ty.pointee();

        let ty = match &*pointee {
            &ty::Ty::Struct(ref sty) => {
                if sty.num_fields() > fld {
                    sty.get_field(fld)
                } else {
                    panic!("Field '{}' not in range", fld)
                }
            }
            &ty::Ty::Tuple(ref tys) => {
                if tys.len() > fld as usize {
                    tys[fld as usize]
                } else {
                    panic!("Field '{}' not in range", fld)
                }
            }
            _ => panic!("Value is not a pointer to a valid type")
        };

        let ty = ty::Ty::rptr(&*self.cx, mutbl, ty);

        let op = ir::Op::GetFieldPtr(val.add_use(), fld);
        self.add_instruction(blk, name, op, ty)
    }

    pub fn index<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>, mutbl: bool,
                        val: ValueHandle<'a>, idx: ValueHandle<'a>) -> ValueHandle<'a>
    where S: IntoImmString {
        let mut val = val.val;
        let mut idx = idx.val;

        let idx_ty = idx.get_type();
        let val_ty = val.get_type();

        assert!(idx_ty.is_uint(), "Index is not a valid type ({:?} != unsigned integer)",
                idx_ty);
        assert!(val_ty.is_pointer(), "Value is not a pointer ({:?} != pointer type)",
                val_ty);
        let seq_ty = val_ty.pointee();

        let el_ty = match &*seq_ty {
            &ty::Ty::Array(ty, _) |
            &ty::Ty::Slice(ty) => ty,
            ty => panic!("Value is not a pointer to a valid type ({:?})", ty)
        };

        let ty = ty::Ty::rptr(&*self.cx, mutbl, el_ty);
        let op = ir::Op::Index(val.add_use(), idx.add_use());

        self.add_instruction(blk, name, op, ty)
    }
}

impl IRBuilder<Function> {
    #[inline]
    pub fn get_fn<'a>(&'a self) -> &'a ir::Function {
        unsafe { &*self.b.f }
    }

    pub fn arg<'a>(&'a self, idx: u32) -> ValueHandle<'a> {
        unsafe {
            let arg = (*self.b.f).get_arg(idx).expect("Argument index out of range");
            ValueHandle {
                val: ir::Value::arg(arg),
                lt: marker::ContravariantLifetime
            }
        }
    }

    pub fn set_arg_name<S:IntoImmString>(&self, idx: u32, name: S) {
        unsafe {
            let arg = (*self.b.f).get_arg(idx).expect("Argument index out of range");
            let name = self.inst_name(Some(name)).unwrap();
            (*arg).set_name(name);
        }
    }

    pub fn new_block<'a, S:IntoImmString>(&'a self, name: S) -> BlockHandle<'a> {
        let name = self.block_name(Some(name)).unwrap();
        let mut blk = ir::BasicBlock::new(name);
        let blkp = &mut *blk as *mut _;

        unsafe {
            (*self.b.f).push_block(blk);
        }

        BlockHandle {
            blk: blkp,
            lt: marker::ContravariantLifetime
        }
    }

    fn add_instruction<'a, S>(&'a self, blk: BlockHandle<'a>, name: Option<S>,
                              op: ir::Op, ty: ty::TyRef) -> ValueHandle<'a>
    where S:IntoImmString {
        let blk = unsafe { &mut *blk.blk };
        let name = self.inst_name(name);
        let mut inst = ir::Instruction::new(name, ty, op);

        let val = ir::Value::inst(&mut *inst);

        for u in inst.operands_mut() {
            if let Some(u) = u.get_use_mut() {
                u.set_consumer(val.clone());
            }
        }

        blk.push_instruction(inst);

        ValueHandle {
            val: val,
            lt: marker::ContravariantLifetime
        }
    }

    fn inst_name<S>(&self, name: Option<S>) -> Option<ImmString>
    where S:IntoImmString {
        name.map(|n| {
            let n = n.into_imm_string();
            let mut names = self.b.inst_names.borrow_mut();
            match names.entry(n.clone()) {
                Entry::Occupied(mut e) => {
                    let count = e.get_mut();
                    let new_name = format!("{}.{}", n, *count).into_imm_string();
                    *count += 1;

                    new_name
                }
                Entry::Vacant(e) => {
                    e.insert(0);
                    n
                }
            }
        })

    }

    fn block_name<S>(&self, name: Option<S>) -> Option<ImmString>
    where S:IntoImmString {
        name.map(|n| {
            let n = n.into_imm_string();
            let mut names = self.b.block_names.borrow_mut();
            match names.entry(n.clone()) {
                Entry::Occupied(mut e) => {
                    let count = e.get_mut();
                    let new_name = format!("{}.{}", n, *count).into_imm_string();
                    *count += 1;

                    new_name
                }
                Entry::Vacant(e) => {
                    e.insert(0);
                    n
                }
            }
        })

    }

    #[inline]
    pub fn finish(self) -> IRBuilder<()> {
        IRBuilder {
            cx: self.cx,
            b: (),
        }
    }
}
