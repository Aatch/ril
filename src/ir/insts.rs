use uniq_map::{Name,Named};
use ir::{Use, Value, InstName, BlockName};
use ir::ty;

pub struct Instruction<'tcx> {
    pub name: InstName<'tcx>,
    pub blk: BlockName<'tcx>,
    pub ty: ty::Ty<'tcx>,
    pub op: Op<'tcx>,
    pub users: Vec<Use<'tcx>>
}

impl<'tcx> Named for Instruction<'tcx> {
    fn set_name(&mut self, name: InstName<'tcx>) {
        assert!(!self.name.is_valid());
        self.name = name;
    }
}

impl<'tcx> Instruction<'tcx> {
    pub fn new(op: Op<'tcx>, ty: ty::Ty<'tcx>) -> Instruction<'tcx> {
        Instruction {
            name: Name::invalid(),
            blk: Name::invalid(),
            ty: ty,
            op: op,
            users: Vec::new()
        }
    }

    pub fn replace_operand(&mut self, idx: usize, val: Value<'tcx>) {
        match self.op {
            Op::Add(ref mut l, ref mut r) |
            Op::Sub(ref mut l, ref mut r) |
            Op::Mul(ref mut l, ref mut r) |
            Op::Div(ref mut l, ref mut r) |
            Op::Rem(ref mut l, ref mut r) |
            Op::Shl(ref mut l, ref mut r) |
            Op::Shr(ref mut l, ref mut r) |
            Op::And(ref mut l, ref mut r) |
            Op::Or(ref mut l, ref mut r)  |
            Op::Xor(ref mut l, ref mut r) |
            Op::Cmp(_, ref mut l, ref mut r) |
            Op::Index(ref mut l, ref mut r) |
            Op::Store(ref mut l, ref mut r) if idx < 2 => {
                if idx == 0 {
                    *l = val;
                } else if idx == 1 {
                    *r = val;
                }
            }
            Op::Not(ref mut v) |
            Op::Local(ref mut v) |
            Op::Load(ref mut v) |
            Op::CondBr(ref mut v, _, _) |
            Op::Return(ref mut v) if idx == 0 => { *v = val }
            Op::Phi(ref mut set) if idx < set.len() => {
                set[idx].0 = val
            }
            _ => panic!("Invalid operand index {}", idx)
        }
    }
}

#[derive(Clone,Show)]
pub enum Op<'tcx> {
    Add(Value<'tcx>, Value<'tcx>),
    Sub(Value<'tcx>, Value<'tcx>),
    Mul(Value<'tcx>, Value<'tcx>),
    Div(Value<'tcx>, Value<'tcx>),
    Rem(Value<'tcx>, Value<'tcx>),
    Shl(Value<'tcx>, Value<'tcx>),
    Shr(Value<'tcx>, Value<'tcx>),
    And(Value<'tcx>, Value<'tcx>),
    Or(Value<'tcx>, Value<'tcx>),
    Xor(Value<'tcx>, Value<'tcx>),
    Not(Value<'tcx>),
    Cmp(Cmp, Value<'tcx>, Value<'tcx>),

    Local(Value<'tcx>),
    Load(Value<'tcx>),
    Store(Value<'tcx>, Value<'tcx>),
    Index(Value<'tcx>, Value<'tcx>),

    Phi(Vec<(Value<'tcx>, BlockName<'tcx>)>),

    Br(BlockName<'tcx>),
    CondBr(Value<'tcx>, BlockName<'tcx>, BlockName<'tcx>),
    Return(Value<'tcx>),
}

#[derive(Copy,Clone,Show)]
pub enum Cmp {
    Eq, // Equal
    Ne, // Not Equal
    Lt, // Less Than
    Le, // Less Than or Equal
    Gt, // Greater Than
    Ge, // Greater Than or Equal
}

impl<'tcx> Op<'tcx> {
    pub fn is_pure(&self) -> bool {
        match *self {
            Op::Add(..) |
            Op::Sub(..) |
            Op::Mul(..) |
            Op::Div(..) |
            Op::Rem(..) |
            Op::Shl(..) |
            Op::Shr(..) |
            Op::And(..) |
            Op::Or(..)  |
            Op::Xor(..) |
            Op::Not(..) |
            Op::Cmp(..) |
            Op::Local(..)  |
            Op::Index(..) |
            Op::Load(..) => true,
            _ => false
        }
    }

    pub fn is_terminator(&self) -> bool {
        match *self {
            Op::Br(..) |
            Op::CondBr(..) |
            Op::Return(..) => true,
            _ => false
        }
    }
}
