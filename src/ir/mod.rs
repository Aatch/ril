use std::fmt;
use uniq_map::{Name, UniqueMap, Named};

use dlist::DList;

use ir::insts::Instruction;

pub mod insts;
pub mod builder;
pub mod ty;
pub mod print;

pub type InstName<'tcx> = Name<Instruction<'tcx>>;
pub type BlockName<'tcx> = Name<BasicBlock<'tcx>>;
pub type FunctionName<'tcx> = Name<Function<'tcx>>;

pub struct Module<'tcx> {
    pub function_map: UniqueMap<Function<'tcx>>
}

pub struct Function<'tcx> {
    pub name: FunctionName<'tcx>,
    pub arg_tys: Vec<ty::Ty<'tcx>>,
    pub ret_ty: ty::Ty<'tcx>,
    pub inst_map: UniqueMap<Instruction<'tcx>>,
    pub block_map: UniqueMap<BasicBlock<'tcx>>,
    pub blocks: DList<BlockName<'tcx>>,
}

pub struct BasicBlock<'tcx> {
    name: BlockName<'tcx>,
    pub instructions: DList<InstName<'tcx>>,
}

impl<'tcx> Named for BasicBlock<'tcx> {
    fn set_name(&mut self, name: BlockName<'tcx>) {
        assert!(!self.name.is_valid());
        self.name = name;
    }
}

#[derive(Copy,Clone,Show,PartialEq,Eq)]
pub enum Value<'tcx> {
    Const(Constant<'tcx>),
    Inst(InstName<'tcx>),
    Arg(usize),
}

#[derive(Copy,Clone,Show,PartialEq,Eq)]
pub enum Constant<'tcx> {
    Nil,
    Zero(ty::Ty<'tcx>),
    Undef(ty::Ty<'tcx>),
    Bool(bool),
    Int(ty::Ty<'tcx>, i64),
    Uint(ty::Ty<'tcx>, u64),
}

pub struct Use<'tcx> {
    consumer: InstName<'tcx>,
    idx: usize,
}

impl<'tcx> Function<'tcx> {
    pub fn new(arg_tys: Vec<ty::Ty<'tcx>>, ret_ty: ty::Ty<'tcx>) -> Function<'tcx> {
        Function {
            name: Name::invalid(),
            arg_tys: arg_tys,
            ret_ty: ret_ty,
            inst_map: UniqueMap::new(),
            block_map: UniqueMap::new(),
            blocks: DList::new(),
        }
    }

    pub fn new_block(&mut self) -> BlockName<'tcx> {
        let blk = BasicBlock {
            name: Name::invalid(),
            instructions: DList::new()
        };

        let name = self.block_map.unique(blk);
        self.push_block(name);
        name
    }

    pub fn unique_inst(&mut self, inst: Instruction<'tcx>) -> InstName<'tcx> {
        self.inst_map.unique(inst)
    }

    pub fn push_inst(&mut self, blk: BlockName<'tcx>, inst: InstName<'tcx>) {
        use ir::insts::Op;
        let op;
        {
            let inst = self.inst_map.get_mut(inst)
                .expect("Instruction doesn't doesn't exist in this function");
            assert!(!inst.blk.is_valid());

            inst.blk = blk;
            op = inst.op.clone();
        }

        match op {
            Op::Add(l, r) |
            Op::Sub(l, r) |
            Op::Mul(l, r) |
            Op::Div(l, r) |
            Op::Rem(l, r) |
            Op::Shl(l, r) |
            Op::Shr(l, r) |
            Op::And(l, r) |
            Op::Or(l, r)  |
            Op::Xor(l, r) |
            Op::Cmp(_, l, r) |
            Op::Store(l, r) |
            Op::Index(l, r) => {
                self.add_use(l, inst, 0);
                self.add_use(r, inst, 1);
            }
            Op::Local(v) |
            Op::Load(v) |
            Op::Not(v) |
            Op::CondBr(v, _, _) |
            Op::Return(v) => {
                self.add_use(v, inst, 0);
            }
            Op::Phi(ref set) => {
                for (i, &(v, _)) in set.iter().enumerate() {
                    self.add_use(v, inst, i);
                }
            }
            Op::Br(_) => ()
        }

        let blk = self.block_map.get_mut(blk).expect("Block doesn't exist in this fuction");
        blk.push_instruction(inst);
    }

    pub fn replace_uses_with(&mut self, old: InstName<'tcx>, new: Value<'tcx>) {
        match new {
            // If the new and old are the same, do nothing.
            Value::Inst(new_inst) if new_inst == old => return,
            // Otherwise, replace uses of 'old' with 'new'
            _ => {
                if let Some(old_inst) = self.inst_map.remove(old) {
                    for u in old_inst.users.iter() {
                        if let Some(user) = self.inst_map.get_mut(u.consumer) {
                            user.replace_operand(u.idx, new);
                        }
                    }

                    if let Some(blk) = self.block_map.get_mut(old_inst.blk) {
                        let mut cursor = blk.instructions.cursor();
                        while let Some(&mut inst) = cursor.peek_next() {
                            if inst == old {
                                cursor.remove();
                                break;
                            }
                            cursor.next();
                        }
                    }
                }
            }
        }
    }

    fn add_use(&mut self, val: Value<'tcx>, inst: InstName<'tcx>, idx: usize) {
        match val {
            Value::Inst(used) => {
                if let Some (used) = self.inst_map.get_mut(used) {
                    used.users.push(Use::new(inst, idx));
                }
            }
            _ => ()
        }
    }

    fn push_block(&mut self, blk: BlockName<'tcx>) {
        self.blocks.push_back(blk);
    }
}

impl<'tcx> BasicBlock<'tcx> {
    fn push_instruction(&mut self, inst: InstName<'tcx>) {
        self.instructions.push_back(inst);
    }
}

impl<'tcx> Use<'tcx> {
    pub fn new(consumer: InstName<'tcx>, idx: usize) -> Use<'tcx> {
        Use {
            consumer: consumer,
            idx: idx
        }
    }
}

impl<'tcx> Constant<'tcx> {
    #[inline]
    pub fn zero(ty: ty::Ty<'tcx>) -> Value<'tcx> {
        match ty.data {
            ty::Int(_) => Constant::int(ty, 0),
            ty::Uint(_) => Constant::uint(ty, 0),
            _ => Value::Const(Constant::Zero(ty)),
        }
    }

    #[inline]
    pub fn undef(ty: ty::Ty<'tcx>) -> Value<'tcx> {
        Value::Const(Constant::Undef(ty))
    }

    #[inline]
    pub fn i8(tcx: &ty::Context<'tcx>, val: i8) -> Value<'tcx> {
        Constant::int(tcx.types.i8, val as i64)
    }

    #[inline]
    pub fn i16(tcx: &ty::Context<'tcx>, val: i16) -> Value<'tcx> {
        Constant::int(tcx.types.i16, val as i64)
    }

    #[inline]
    pub fn i32(tcx: &ty::Context<'tcx>, val: i32) -> Value<'tcx> {
        Constant::int(tcx.types.i32, val as i64)
    }

    #[inline]
    pub fn i64(tcx: &ty::Context<'tcx>, val: i64) -> Value<'tcx> {
        Constant::int(tcx.types.i64, val as i64)
    }

    #[inline]
    pub fn isize(tcx: &ty::Context<'tcx>, val: isize) -> Value<'tcx> {
        Constant::int(tcx.types.isize, val as i64)
    }

    #[inline]
    pub fn int(ty: ty::Ty<'tcx>, val: i64) -> Value<'tcx> {
        Value::Const(Constant::Int(ty, val))
    }

    #[inline]
    pub fn u8(tcx: &ty::Context<'tcx>, val: u8) -> Value<'tcx> {
        Constant::uint(tcx.types.u8, val as u64)
    }

    #[inline]
    pub fn u16(tcx: &ty::Context<'tcx>, val: u16) -> Value<'tcx> {
        Constant::uint(tcx.types.u16, val as u64)
    }

    #[inline]
    pub fn u32(tcx: &ty::Context<'tcx>, val: u32) -> Value<'tcx> {
        Constant::uint(tcx.types.u32, val as u64)
    }

    #[inline]
    pub fn u64(tcx: &ty::Context<'tcx>, val: u64) -> Value<'tcx> {
        Constant::uint(tcx.types.u64, val as u64)
    }

    #[inline]
    pub fn usize(tcx: &ty::Context<'tcx>, val: usize) -> Value<'tcx> {
        Constant::uint(tcx.types.usize, val as u64)
    }

    #[inline]
    pub fn uint(ty: ty::Ty<'tcx>, val: u64) -> Value<'tcx> {
        Value::Const(Constant::Uint(ty, val))
    }
}

impl<'tcx> fmt::Show for Function<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(f.write_str("Function ("));
        let mut comma = false;
        for arg_ty in self.arg_tys.iter() {
            if comma {
                try!(f.write_str(", "));
            }
            comma = true;
            try!(write!(f, "{:?}", arg_ty));
        }
        try!(write!(f, ") -> {:?} {{", &self.ret_ty));

        try!(write!(f, "  # Blocks: {}\n", self.blocks.len()));
        for &bn in self.blocks.iter() {
            let blk = self.block_map.get(bn).expect("Basic Block doesn't exist?!");
            try!(write!(f, "  Block {:?} {{\n    # Instructions: {}\n",
                        bn, blk.instructions.len()));

            for &i in blk.instructions.iter() {
                let inst = self.inst_map.get(i).expect("Instruction doesn't exist?!");

                try!(write!(f, "    Instruction {:?} - {:?} ({:?})\n", i, inst.op, inst.ty));
                if inst.users.len() > 0 {
                    try!(write!(f, "      Users: {:?}\n", inst.users));
                }
            }
            try!(f.write_str("  }\n"));;
        }
        f.write_str("}")
    }
}

impl<'tcx> fmt::Show for Use<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Use({:?}, {})", self.consumer, self.idx)
    }
}
