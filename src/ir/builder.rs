
use dlist::Cursor;
use ir;
use ir::insts::*;
use ir::ty::{self};

pub struct Builder<'a, 'tcx:'a> {
    tcx: &'tcx ty::Context<'tcx>,
    f: &'a mut ir::Function<'tcx>,
}

impl<'a, 'tcx:'a> Builder<'a, 'tcx> {
    pub fn new(tcx: &'tcx ty::Context<'tcx>, f: &'a mut ir::Function<'tcx>) -> Builder<'a, 'tcx> {
        Builder {
            tcx: tcx,
            f: f,
        }
    }

    pub fn new_block(&mut self) -> ir::BlockName<'tcx> {
        self.f.new_block()
    }

    pub fn const_bool(&self, val: bool) -> ir::Value<'tcx> {
        ir::Value::Const(ir::Constant::Bool(val))
    }

    pub fn arg(&self, idx: usize) -> ir::Value<'tcx> {
        ir::Value::Arg(idx)
    }

    pub fn add(&mut self, blk: ir::BlockName<'tcx>, l: ir::Value<'tcx>, r: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let ty = self.val_ty(l);
        let op = Op::Add(l, r);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn sub(&mut self, blk: ir::BlockName<'tcx>, l: ir::Value<'tcx>, r: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let ty = self.val_ty(l);
        let op = Op::Sub(l, r);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn mul(&mut self, blk: ir::BlockName<'tcx>, l: ir::Value<'tcx>, r: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let ty = self.val_ty(l);
        let op = Op::Mul(l, r);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn div(&mut self, blk: ir::BlockName<'tcx>, l: ir::Value<'tcx>, r: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let ty = self.val_ty(l);
        let op = Op::Div(l, r);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn rem(&mut self, blk: ir::BlockName<'tcx>, l: ir::Value<'tcx>, r: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let ty = self.val_ty(l);
        let op = Op::Rem(l, r);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn cmp(&mut self, blk: ir::BlockName<'tcx>, cmp: Cmp, l: ir::Value<'tcx>, r: ir::Value<'tcx>) -> ir::Value<'tcx> {
        match cmp {
            Cmp::Eq => {
                if l == r { return self.const_bool(true) }
            }
            Cmp::Ne => {
                if l == r { return self.const_bool(false) }
            }
            _ => ()
        }

        let op = Op::Cmp(cmp, l, r);
        self.add_instruction(blk, Instruction::new(op, self.tcx.types.bool))
    }

    pub fn local(&mut self, blk: ir::BlockName<'tcx>, ty: ty::Ty<'tcx>,
                 mutable: bool, init: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let op = Op::Local(init);
        let ty = if mutable {
            self.tcx.mk_mut_ptr(ty)
        } else {
            self.tcx.mk_imm_ptr(ty)
        };
        self.add_instruction(blk, Instruction::new(op,ty))
    }

    pub fn load(&mut self, blk: ir::BlockName<'tcx>, ptr: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let ptr_ty = self.val_ty(ptr);
        let ty = self.tcx.inner_ty(ptr_ty).expect("Load from a non-pointer");
        let op = Op::Load(ptr);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn store(&mut self, blk: ir::BlockName<'tcx>, ptr: ir::Value<'tcx>, val: ir::Value<'tcx>) {
        let op = Op::Store(ptr, val);
        self.add_instruction(blk, Instruction::new(op, self.tcx.types.nil));
    }

    pub fn index(&mut self, blk: ir::BlockName<'tcx>, base: ir::Value<'tcx>, offset: ir::Value<'tcx>) -> ir::Value<'tcx> {
        let base_ty = self.tcx.inner_ty(self.val_ty(base)).unwrap();
        let ty = self.tcx.mk_mut_ptr(base_ty);
        let op = Op::Index(base, offset);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn phi(&mut self, blk: ir::BlockName<'tcx>, set: Vec<(ir::Value<'tcx>, ir::BlockName<'tcx>)>) -> ir::Value<'tcx> {
        assert!(set.len() > 0);

        let ty;
        {
            let mut iter = set.iter();
            ty = self.val_ty(iter.next().unwrap().0);
            for &(v, _) in iter {
                let vty = self.val_ty(v);
                assert!(ty == vty, "Not all incoming values are same type")
            }
        }

        let op = Op::Phi(set);
        self.add_instruction(blk, Instruction::new(op, ty))
    }

    pub fn br(&mut self, blk: ir::BlockName<'tcx>, goto: ir::BlockName<'tcx>) {
        let op = Op::Br(goto);
        self.add_instruction(blk, Instruction::new(op, self.tcx.types.nil));
    }

    pub fn condbr(&mut self, blk: ir::BlockName<'tcx>, val: ir::Value<'tcx>,
                  if_true: ir::BlockName<'tcx>, if_false: ir::BlockName<'tcx>) {
        let op = Op::CondBr(val, if_true, if_false);
        self.add_instruction(blk, Instruction::new(op, self.tcx.types.nil));
    }

    pub fn ret(&mut self, blk: ir::BlockName<'tcx>, val: ir::Value<'tcx>) {
        let op = Op::Return(val);
        self.add_instruction(blk, Instruction::new(op, self.tcx.types.nil));
    }
}

impl<'a, 'tcx> Builder<'a, 'tcx> {

    fn add_instruction(&mut self, blk: ir::BlockName<'tcx>, inst: Instruction<'tcx>) -> ir::Value<'tcx> {
        let name = self.f.unique_inst(inst);
        self.f.push_inst(blk, name);
        ir::Value::Inst(name)
    }

    fn val_ty(&self, val: ir::Value<'tcx>) -> ty::Ty<'tcx> {
        match val {
            ir::Value::Inst(i) => {
                let i = self.f.inst_map.get(i).expect("Invalid Instruction");
                i.ty
            }
            ir::Value::Const(c) => {
                match c {
                    ir::Constant::Zero(ty) |
                    ir::Constant::Undef(ty) |
                    ir::Constant::Int(ty, _) |
                    ir::Constant::Uint(ty, _) => ty,
                    ir::Constant::Bool(_) => self.tcx.types.bool,
                    ir::Constant::Nil => self.tcx.types.nil
                }
            }
            ir::Value::Arg(idx) => {
                self.f.arg_tys[idx]
            }
        }
    }
}
