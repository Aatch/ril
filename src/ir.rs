#![allow(raw_pointer_derive)]

use std::borrow::BorrowFrom;
use std::fmt;
use std::hash::{Hash, Hasher, Writer};
use std::mem;
use std::ptr;

use context::Context;
use imm_str::{ImmString, IntoImmString};
use ilist::{self, IList, IListNode, LinkPair};
use ty;

pub struct Function {
    pub name: ImmString,
    blocks: IList<BasicBlock>,
    ty: ty::TyRef,

    pub cx: *const Context,

    args: IList<Arg>,
    uses: IList<Use>,
}

pub struct BasicBlock {
    pub name: ImmString,
    instructions: IList<Instruction>,
    parent: *mut Function,
    preds: Vec<*mut BasicBlock>,

    links: LinkPair<BasicBlock>
}

pub struct Arg {
    pub name: Option<ImmString>,
    parent: *mut Function,
    ty: ty::TyRef,
    idx: u32,

    uses: IList<Use>,

    links: LinkPair<Arg>
}

pub struct Instruction {
    pub name: Option<ImmString>,
    pub op: Op,
    ty: ty::TyRef,

    bb: *mut BasicBlock,
    uses: IList<Use>,
    links: LinkPair<Instruction>
}

pub struct Use {
    producer: Definition,
    consumer: Value,
    links: LinkPair<Use>
}

type Definition = Value;
#[derive(PartialEq,Eq,Clone)]
pub struct Value {
    kind: ValueKind
}

#[derive(PartialEq,Eq,Clone)]
enum ValueKind {
    None,
    Arg(*mut Arg),
    Inst(*mut Instruction),
    Function(*mut Function),
    Const(*mut Constant),
}

pub struct Constant {
    cx: *const Context,
    pub data: ConstData,
    uses: IList<Use>,
}

#[derive(Hash,Eq,PartialEq)]
pub enum ConstData {
    Undef(ty::TyRef),
    Nil,
    Bool(bool),
    Int(i64, u16),
    Uint(u64, u16),
    String(ImmString)
}

struct UseRef {
    p: *mut Use
}

impl UseRef {
    fn new(u: *mut Use) -> UseRef {
        UseRef { p: u }
    }

    pub fn clear(&mut self) {
        self.p = ptr::null_mut();
    }

    pub fn get_use<'a>(&'a self) -> Option<&'a Use> {
        if self.p.is_null() {
            None
        } else {
            unsafe { Some(&*self.p) }
        }
    }

    pub fn get_use_mut<'a>(&'a mut self) -> Option<&'a mut Use> {
        if self.p.is_null() {
            None
        } else {
            unsafe { Some(&mut *self.p) }
        }
    }
}

/// Specific Operations inside instructions
pub enum Op {
    /// Add(lhs, rhs) -> lhs + rhs
    Add(UseRef, UseRef),
    /// Sub(lhs, rhs) -> lhs - rhs
    Sub(UseRef, UseRef),
    /// Mul(lhs, rhs) -> lhs * rhs
    Mul(UseRef, UseRef),
    /// Div(lhs, rhs) -> lhs / rhs
    Div(UseRef, UseRef),
    /// Rem(lhs, rhs) -> lhs % rhs
    Rem(UseRef, UseRef),
    /// And(lhs, rhs) -> lhs & rhs
    And(UseRef, UseRef),
    /// Or(lhs, rhs) -> lhs | rhs
    /// This is also used for logical or
    Or(UseRef, UseRef),
    /// Xor(lhs, rhs) -> lhs ^ rhs
    Xor(UseRef, UseRef),
    /// Not(val) -> !val
    /// For bools this is logical not, for integers, bitwise inversion
    Not(UseRef),
    /// Cmp(op, lhs, rhs) -> `lhs` `op` `rhs`
    Cmp(Cmp, UseRef, UseRef),

    /// Call(fn, args) - Call function `fn` with `args`
    Call(UseRef, Box<[UseRef]>),

    /// Store(dst, val) - Store `val` into the location at `dst`
    /// The type pointed to by `dst` must be the same as the type of `val`
    Store(UseRef, UseRef),
    /// Load(src) - Load the value at `src`. `src` must be a pointer type
    Load(UseRef),
    /// ExtractField(val, fld) - Get field `fld` from the `val`.
    ExtractField(UseRef, u32),
    /// Index(val, idx) - Get the `idx`-th element of `val`
    Index(UseRef, UseRef),

    /// Br(target) - Unconditional branch to `target`
    Br(*mut BasicBlock),
    /// CondBr(cond, then, else) - Conditional branch based on `cond`, `cond` must be boolean.
    /// If `cond` is true, branch to `then` otherwise branch to `else`
    CondBr(UseRef, *mut BasicBlock, *mut BasicBlock),
    /// Return(val) - Return the value `val` from the function
    Return(UseRef),

    /// PHI Node
    Phi(Vec<(UseRef, *mut BasicBlock)>)
}

#[derive(Copy)]
pub enum Cmp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge
}

impl Function {
    pub fn new<S:IntoImmString>(name: S, ty: ty::TyRef) -> Box<Function> {
        assert!(ty.is_fn_ty(), "`ty` is not a function type");

        let mut f = box Function {
            name: name.into_imm_string(),
            ty: ty,
            args: IList::new(),

            cx: ptr::null(),

            uses: IList::new(),
            blocks: IList::new(),
        };

        let fp = &mut *f as *mut _;

        if let ty::Ty::Fn(ref ins, _) = *ty {
            for (i, &ty) in ins.iter().enumerate() {
                f.args.push_back(box Arg {
                    name: None,
                    parent: fp,
                    ty: ty,
                    idx: i as u32,

                    uses: IList::new(),
                    links: LinkPair::new()
                });
            }
        } else {
            unreachable!()
        }

        f
    }

    pub fn blocks<'a>(&'a self) -> &'a IList<BasicBlock> {
        &self.blocks
    }

    pub fn blocks_mut<'a>(&'a mut self) -> &'a mut IList<BasicBlock> {
        &mut self.blocks
    }

    pub fn push_block(&mut self, mut blk: Box<BasicBlock>) {
        blk.parent = self;
        self.blocks.push_back(blk);
    }

    pub fn insert_block(&mut self, before: *mut BasicBlock, mut blk: Box<BasicBlock>) {
        assert!(before != (&mut *blk as *mut _), "Inserted a block before itself");
        unsafe {
            if before.is_null() {
                self.push_block(blk);
            } else {
                assert!((*before).parent == (self as *mut _));
                blk.parent = self;
                self.blocks.insert_before(&mut *before, blk);
            }
        }
    }

    pub fn get_type(&self) -> ty::TyRef {
        self.ty
    }

    pub fn get_return_type(&self) -> ty::FnOutput {
        if let &ty::Ty::Fn(_, ref out) = &*self.ty {
            out.clone()
        } else {
            panic!("Function does not have a function type!")
        }
    }

    pub fn has_body(&self) -> bool {
        !self.blocks.is_empty()
    }

    pub fn get_arg(&mut self, idx: u32) -> Option<*mut Arg> {
        self.args.get_mut(idx).map(|x| x as *mut _)
    }

    pub fn add_use(&mut self) -> UseRef {
        let mut u = box Use::new();
        u.set_producer(Value::func(self));

        let up = &mut *u as *mut _;
        self.uses.push_back(u);

        UseRef::new(up)
    }

    pub fn remove_use(&mut self, u: &mut Use) -> Box<Use> {
        unsafe {
            if let ValueKind::Function(a) = u.producer.kind {
                assert!(a == (self as *mut _), "Use not for this argument");
                self.uses.remove_node(u)
            } else {
                panic!("Invalid use");
            }
        }
    }

    pub fn set_context(&mut self, cx: *const Context) {
        assert!(self.cx.is_null());
        assert!(!cx.is_null());

        self.cx = cx;
    }
}

impl Arg {
    pub fn set_name<S:IntoImmString>(&mut self, name: S) {
        let name = name.into_imm_string();
        if name.len() == 0 {
            self.name = None;
        } else {
            self.name = Some(name);
        }
    }

    pub fn add_use(&mut self) -> UseRef {
        let mut u = box Use::new();
        u.set_producer(Value::arg(self));

        let up = &mut *u as *mut _;
        self.uses.push_back(u);

        UseRef::new(up)
    }

    pub fn remove_use(&mut self, u: &mut Use) -> Box<Use> {
        unsafe {
            if let ValueKind::Arg(a) = u.producer.kind {
                assert!(a == (self as *mut _), "Use not for this argument");
                debug!("Removing Use for Arg");
                self.uses.remove_node(u)
            } else {
                panic!("Invalid use");
            }
        }
    }

}

impl Drop for Function {
    fn drop(&mut self) {
        self.uses.clear();
        // We have to be careful about dropping block here, control flow insturctions update the
        // predecessor set when they are dropped, so we need to make sure those blocks are still
        // valid. We do this by clearing all the instructions from each block before dropping the
        // list of blocks itself.
        {
            let mut cursor = self.blocks.cursor();
            while let Some(blk) = cursor.next() {
                blk.instructions.clear();
            }
        }
        self.blocks.clear();
        self.args.clear();
    }
}

impl BasicBlock {
    pub fn new<S:IntoImmString>(name: S) -> Box<BasicBlock> {
        box BasicBlock {
            name: name.into_imm_string(),
            instructions: IList::new(),
            parent: ptr::null_mut(),
            preds: Vec::new(),

            links: LinkPair::new()
        }
    }

    pub fn instructions<'a>(&'a self) -> &'a IList<Instruction> {
        &self.instructions
    }

    pub fn instructions_mut<'a>(&'a mut self) -> &'a mut IList<Instruction> {
        &mut self.instructions
    }

    pub fn push_instruction(&mut self, mut inst: Box<Instruction>) {
        inst.bb = self;
        self.update_target_preds(&*inst);
        self.instructions.push_back(inst);
    }

    pub fn insert_instruction(&mut self, before: *mut Instruction, mut inst: Box<Instruction>) {
        assert!(before != (&mut *inst as *mut _), "Inserted an instruction before itself");
        unsafe {
            if before.is_null() {
                self.push_instruction(inst);
            } else {
                assert!((*before).bb == (self as *mut _));
                inst.bb = self;
                self.update_target_preds(&*inst);
                self.instructions.insert_before(&mut *before, inst);
            }
        }
    }

    fn update_target_preds(&mut self, i: &Instruction) {
        match i.op {
            Op::Br(target) => unsafe {
                (*target).add_pred(self);
            },
            Op::CondBr(_, then, els) => unsafe {
                (*then).add_pred(self);
                (*els).add_pred(self);
            },
            _ => ()
        }
    }

    pub fn add_pred(&mut self, pred: *mut BasicBlock) {
        for &p in self.preds.iter() {
            if pred == p { return; }
        }

        self.preds.push(pred);
    }

    pub fn remove_pred(&mut self, pred: *mut BasicBlock) {
        self.preds.retain(|&p| p != pred);
    }
}

impl Use {
    pub fn new() -> Use {
        Use {
            producer: Value::none(),
            consumer: Value::none(),

            links: LinkPair::new()
        }
    }

    pub fn producer(&self) -> Value {
        self.producer.clone()
    }

    pub fn consumer(&self) -> Value {
        self.consumer.clone()
    }

    pub fn set_producer(&mut self, v: Value) {
        assert!(self.producer.is_none());
        assert!(!v.is_none());
        self.producer = v;
    }

    pub fn set_consumer(&mut self, v: Value) {
        assert!(self.consumer.is_none());
        assert!(!v.is_none());
        self.consumer = v;
    }

    pub fn clear(&mut self) {
        self.producer = Value::none();
        self.consumer = Value::none();
    }

    pub fn remove_use(&mut self) {
        unsafe {
            match self.producer.kind {
                ValueKind::Inst(i) => {
                    (*i).remove_use(self);
                }
                ValueKind::Arg(a) => {
                    (*a).remove_use(self);
                }
                ValueKind::Const(c) => {
                    (*c).remove_use(self);
                }
                _ => ()
            }

            self.clear();
        }
    }
}

impl Drop for Use {
    fn drop(&mut self) {
        debug!("Dropping Use");
        unsafe {
            match self.consumer.kind {
                ValueKind::Inst(i) => {
                    let operand = (*i).index_of(self);
                    warn!("Clearing operand {} for instruction \"{:?}\"",
                          operand, (*i).name);
                    (*i).clear_operand(operand);
                }
                _ => ()
            }
        }
    }
}

impl Value {
    pub fn get_type(&self) -> ty::TyRef {
        unsafe {
            match self.kind {
                ValueKind::None => panic!("Tried to get type of None"),
                ValueKind::Inst(i) => (*i).ty,
                ValueKind::Arg(a) => (*a).ty,
                ValueKind::Function(f) => (*f).ty,
                ValueKind::Const(c) => {
                    (*c).get_type()
                }
            }
        }
    }

    pub fn add_use(&mut self) -> UseRef {
        unsafe {
            match self.kind {
                ValueKind::None => panic!("Tried to use None"),
                ValueKind::Inst(i) => (*i).add_use(),
                ValueKind::Arg(a) => (*a).add_use(),
                ValueKind::Function(f) => (*f).add_use(),
                ValueKind::Const(c) => (*c).add_use()
            }
        }
    }

    pub fn none() -> Value {
        Value {
            kind: ValueKind::None
        }
    }

    pub fn func(f: *mut Function) -> Value {
        Value {
            kind: ValueKind::Function(f)
        }
    }

    pub fn inst(inst: *mut Instruction) -> Value {
        Value {
            kind: ValueKind::Inst(inst)
        }
    }

    pub fn arg(arg: *mut Arg) -> Value {
        Value {
            kind: ValueKind::Arg(arg)
        }
    }

    pub fn constant(c: *mut Constant) -> Value {
        Value {
            kind: ValueKind::Const(c)
        }
    }

    pub fn is_inst(&self) -> bool {
        if let ValueKind::Inst(..) = self.kind { true } else { false }
    }

    pub fn is_const(&self) -> bool {
        if let ValueKind::Const(..) = self.kind { true } else { false }
    }

    pub fn as_inst<'a>(&'a self) -> Option<&'a Instruction> {
        if let ValueKind::Inst(i)= self.kind {
            unsafe {
                Some(&*i)
            }
        } else {
            None
        }
    }

    pub fn as_const<'a>(&'a self) -> Option<&'a Constant> {
        if let ValueKind::Const(c)= self.kind {
            unsafe {
                Some(&*c)
            }
        } else {
            None
        }
    }

    pub fn as_inst_mut<'a>(&'a mut self) -> Option<&'a mut Instruction> {
        if let ValueKind::Inst(i)= self.kind {
            unsafe {
                Some(&mut *i)
            }
        } else {
            None
        }
    }

    pub fn is_none(&self) -> bool {
        if let ValueKind::None = self.kind { true } else { false }
    }
}

impl Constant {
    pub fn zero(cx: &Context, ty: ty::TyRef) -> *mut Constant {
        match &*ty {
            &ty::Ty::Bool => Constant::bool(cx, false),
            &ty::Ty::Int(sz) => Constant::int(cx, 0, sz as u16),
            &ty::Ty::Uint(sz) => Constant::uint(cx, 0, sz as u16),
            _ => panic!("Invalid type")
        }
    }

    pub fn all_ones(cx: &Context, ty: ty::TyRef) -> *mut Constant {
        match &*ty {
            &ty::Ty::Bool => Constant::bool(cx, true),
            &ty::Ty::Int(sz) => Constant::int(cx, -1, sz as u16),
            &ty::Ty::Uint(sz) => Constant::uint(cx, !0, sz as u16),
            _ => panic!("Invalid type")
        }
    }

    pub fn undef(cx: &Context, ty: ty::TyRef) -> *mut Constant {
        cx.intern_const(ConstData::Undef(ty))
    }
    pub fn nil(cx: &Context) -> *mut Constant {
        cx.intern_const(ConstData::Nil)
    }
    pub fn bool(cx: &Context, val: bool) -> *mut Constant {
        cx.intern_const(ConstData::Bool(val))
    }
    pub fn isize(cx: &Context, val: isize) -> *mut Constant {
        Constant::int(cx, val as i64, 0)
    }
    pub fn i8(cx: &Context, val: i8) -> *mut Constant {
        Constant::int(cx, val as i64, 8)
    }
    pub fn i16(cx: &Context, val: i16) -> *mut Constant {
        Constant::int(cx, val as i64, 16)
    }
    pub fn i32(cx: &Context, val: i32) -> *mut Constant {
        Constant::int(cx, val as i64, 32)
    }
    pub fn i64(cx: &Context, val: i64) -> *mut Constant {
        Constant::int(cx, val, 64)
    }
    pub fn int(cx: &Context, val: i64, sz: u16) -> *mut Constant {
        // Is this right wrt to negative numbers
        let val = val % (2 << sz);
        cx.intern_const(ConstData::Int(val, sz))
    }
    pub fn usize(cx: &Context, val: usize) -> *mut Constant {
        Constant::uint(cx, val as u64, 0)
    }
    pub fn u8(cx: &Context, val: u8) -> *mut Constant {
        Constant::uint(cx, val as u64, 8)
    }
    pub fn u16(cx: &Context, val: u16) -> *mut Constant {
        Constant::uint(cx, val as u64, 16)
    }
    pub fn u32(cx: &Context, val: u32) -> *mut Constant {
        Constant::uint(cx, val as u64, 32)
    }
    pub fn u64(cx: &Context, val: u64) -> *mut Constant {
        Constant::uint(cx, val, 64)
    }
    pub fn uint(cx: &Context, val: u64, sz: u16) -> *mut Constant {
        let val = val % (2 << sz);
        cx.intern_const(ConstData::Uint(val, sz))
    }

    pub fn new(cx: &Context, data: ConstData) -> Constant {
        Constant {
            cx: cx,
            data: data,
            uses: IList::new()
        }
    }

    pub fn add_use(&mut self) -> UseRef {
        let mut u = box Use::new();
        u.set_producer(Value::constant(self));

        let up = &mut *u as *mut _;
        self.uses.push_back(u);

        UseRef::new(up)
    }

    pub fn remove_use(&mut self, u: &mut Use) -> Box<Use> {
        unsafe {
            if let ValueKind::Const(c) = u.producer.kind {
                assert!(c == (self as *mut _), "Use not for this argument");
                self.uses.remove_node(u)
            } else {
                panic!("Invalid use");
            }
        }
    }

    pub fn get_type(&self) -> ty::TyRef {
        let cx = unsafe { &*self.cx };

        match self.data {
            ConstData::Undef(ty) => ty,
            ConstData::Nil => cx.types.nil,
            ConstData::Bool(_) => cx.types.bool,
            ConstData::Int(_, sz) => ty::Ty::int(cx, sz as usize),
            ConstData::Uint(_, sz) => ty::Ty::uint(cx, sz as usize),
            ConstData::String(_) => cx.types.str
        }
    }

    pub fn is_undef(&self) -> bool {
        match self.data {
            ConstData::Undef(_) => true,
            _ => false
        }
    }

    pub fn is_zero(&self) -> bool {
        match self.data {
            ConstData::Bool(false) |
            ConstData::Int(0, _) |
            ConstData::Uint(0, _) => true,
            _ => false
        }
    }

    pub fn is_one(&self) -> bool {
        match self.data {
            ConstData::Bool(true) |
            ConstData::Int(1, _) |
            ConstData::Uint(1, _) => true,
            _ => false
        }
    }

    pub fn is_all_ones(&self) -> bool {
        match self.data {
            ConstData::Bool(true) |
            ConstData::Int(-1, _) => true,
            ConstData::Uint(val, _) if val == !0 => true,
            _ => false
        }
    }
}

impl Instruction {
    pub fn new<S:IntoImmString>(name: Option<S>, ty: ty::TyRef, op: Op) -> Box<Instruction> {
        let name = if let Some(name) = name {
            let name = name.into_imm_string();
            if name.len() == 0 { None } else { Some(name) }
        } else {
            None
        };

        box Instruction {
            name: name,
            op: op,
            ty: ty,

            bb: ptr::null_mut(),
            uses: IList::new(),
            links: LinkPair::new()
        }
    }

    pub fn add_use(&mut self) -> UseRef {
        let mut u = box Use::new();
        u.set_producer(Value::inst(self));

        let up = &mut *u as *mut _;
        self.uses.push_back(u);

        UseRef::new(up)
    }

    pub fn remove_use(&mut self, u: &mut Use) -> Box<Use> {
        unsafe {
            if let ValueKind::Inst(i) = u.producer.kind {
                assert!(i == (self as *mut _), "Use not for this instruction");
                self.uses.remove_node(u)
            } else {
                panic!("Invalid use");
            }
        }
    }

    pub fn set_name<S:IntoImmString>(&mut self, name: S) {
        self.name = Some(name.into_imm_string());
    }

    pub fn clear_name<S:IntoImmString>(&mut self) {
        self.name = None;
    }

    pub fn set_operand(&mut self, idx: u32, u: UseRef) {
        let op = self.operands_mut().skip(idx as usize).next().expect("Invalid Use Index");
        *op = u;
    }

    pub fn clear_operand(&mut self, idx: u32) {
        let op = self.operands_mut().skip(idx as usize).next().expect("Invalid Use Index");
        op.clear()
    }


    pub fn index_of(&self, u: &Use) -> u32 {
        let up = u as *const _;
        self.operands().position(|u| {
            (u.p as *const _) == up
        }).expect("Invalid Use") as u32

    }

    pub fn operands<'a>(&'a self) -> OperandIter<'a> {
        OperandIter {
            inst: self,
            idx: 0
        }
    }

    pub fn operands_mut<'a>(&'a mut self) -> OperandIterMut<'a> {
        OperandIterMut {
            inst: self,
            idx: 0
        }
    }

    pub fn replace_all_uses_with(&mut self, mut val: Value) {
        // Pop each use off of the uses list
        while let Some(mut u) = self.uses.pop_back() {
            let mut consumer = u.consumer();

            // Instructions can only be used by other instructions
            if let Some(inst) = consumer.as_inst_mut() {
                // Set the appropriate operand of the user to use
                // the new value
                let idx = inst.index_of(&*u);
                debug!("Replacing use of {:?}, {:?}, operand {}", self.name, inst, idx);
                u.clear();
                inst.set_operand(idx, val.add_use());
            } else {
                panic!("Invalid Use for instruction");
            }
        }
    }

    pub fn get_type(&self) -> ty::TyRef {
        self.ty
    }
}

impl Drop for Instruction {
    fn drop(&mut self) {
        if self.uses.len() > 0 {
            self.uses.clear();
        }
        for u in self.operands_mut() {
            if let Some(u) = u.get_use_mut() {
                u.remove_use();
            }
        }

        // Update the preds list of target blocks
        match self.op {
            Op::Br(target) => unsafe {
                (*target).remove_pred(self.bb);
            },
            Op::CondBr(_, then, els) => unsafe {
                (*then).remove_pred(self.bb);
                (*els).remove_pred(self.bb);
            },
            _ => ()
        }
    }
}

pub struct OperandIter<'a> {
    inst: &'a Instruction,
    idx: u32
}

impl<'a> Iterator for OperandIter<'a> {
    type Item = &'a UseRef;

    fn next(&mut self) -> Option<&'a UseRef> {
        let idx = self.idx;
        self.idx += 1;
        match self.inst.op {
            Op::Add(ref l, ref r) |
            Op::Sub(ref l, ref r) |
            Op::Mul(ref l, ref r) |
            Op::Div(ref l, ref r) |
            Op::Rem(ref l, ref r) |
            Op::And(ref l, ref r) |
            Op::Or(ref l, ref r) |
            Op::Xor(ref l, ref r) |
            Op::Cmp(_, ref l, ref r) |
            Op::Index(ref l, ref r) |
            Op::Store(ref l, ref r) => {
                if idx == 0 {
                    Some(l)
                } else if idx == 1 {
                    Some(r)
                } else {
                    None
                }
            }

            Op::Call(ref f, ref args) => {
                if idx == 0 {
                    Some(f)
                } else if idx <= args.len() as u32 {
                    Some(&args[(idx as usize)-1])
                } else {
                    None
                }
            }

            Op::Load(ref u) |
            Op::ExtractField(ref u, _) |
            Op::Not(ref u) |
            Op::CondBr(ref u, _, _) |
            Op::Return(ref u) => {
                if idx == 0 {
                    Some(u)
                } else {
                    None
                }
            }
            Op::Br(_) => None,

            Op::Phi(ref ins) => {
                if (idx as usize) < ins.len() {
                    Some(&ins[idx as usize].0)
                } else {
                    None
                }
            }
        }
    }
}

pub struct OperandIterMut<'a> {
    inst: &'a mut Instruction,
    idx: u32
}

impl<'a> Iterator for OperandIterMut<'a> {
    type Item = &'a mut UseRef;

    fn next(&mut self) -> Option<&'a mut UseRef> {
        let idx = self.idx;
        self.idx += 1;
        unsafe {
            mem::transmute(match self.inst.op {
                Op::Add(ref mut l, ref mut r) |
                Op::Sub(ref mut l, ref mut r) |
                Op::Mul(ref mut l, ref mut r) |
                Op::Div(ref mut l, ref mut r) |
                Op::Rem(ref mut l, ref mut r) |
                Op::And(ref mut l, ref mut r) |
                Op::Or(ref mut l, ref mut r) |
                Op::Xor(ref mut l, ref mut r) |
                Op::Cmp(_, ref mut l, ref mut r) |
                Op::Store(ref mut l, ref mut r) |
                Op::Index(ref mut l, ref mut r) => {
                    if idx == 0 {
                        Some(l)
                    } else if idx == 1 {
                        Some(r)
                    } else {
                        None
                    }
                }

                Op::Call(ref mut f, ref mut args) => {
                    if idx == 0 {
                        Some(f)
                    } else if idx <= args.len() as u32 {
                        Some(&mut args[(idx as usize)-1])
                    } else {
                        None
                    }
                }

                Op::Load(ref mut u) |
                Op::ExtractField(ref mut u, _) |
                Op::Not(ref mut u) |
                Op::CondBr(ref mut u, _, _) |
                Op::Return(ref mut u) => {
                    if idx == 0 {
                        Some(u)
                    } else {
                        None
                    }
                }
                Op::Br(_) => None,

                Op::Phi(ref mut ins) => {
                    if (idx as usize) < ins.len() {
                        Some(&mut ins[idx as usize].0)
                    } else {
                        None
                    }
                }
            })
        }
    }
}

#[derive(Eq)]
pub struct InternedConst {
    pub c: *mut Constant
}

impl<S:Hasher+Writer> Hash<S> for InternedConst {
    fn hash(&self, s: &mut S) {
        unsafe {
            (*self.c).data.hash(s);
        }
    }
}

impl PartialEq for InternedConst {
    fn eq(&self, other: &InternedConst) -> bool {
        unsafe {
            (*self.c).data == (*other.c).data
        }
    }
}

impl BorrowFrom<InternedConst> for ConstData {
    fn borrow_from(owned: &InternedConst) -> &ConstData {
        unsafe { &(*owned.c).data }
    }
}

pub trait ToValue {
    fn to_value(&self) -> Value;
}

impl ToValue for Value {
    fn to_value(&self) -> Value { self.clone() }
}

impl ToValue for Use {
    fn to_value(&self) -> Value { self.producer() }
}

macro_rules! impl_ilist_node {
    ($ty:ty, $field:ident) => {
        unsafe impl IListNode for $ty {
            fn prev<'a>(&'a self) -> Option<&'a $ty> {
                self.$field.prev()
            }
            fn next<'a>(&'a self) -> Option<&'a $ty> {
                self.$field.next()
            }

            fn prev_mut<'a>(&'a mut self) -> Option<&'a mut $ty> {
                self.$field.prev_mut()
            }
            fn next_mut<'a>(&'a mut self) -> Option<&'a mut $ty> {
                self.$field.next_mut()
            }

            fn set_next_link(&mut self, next: ilist::Link<$ty>) -> ilist::Link<$ty> {
                self.$field.set_next_link(next)
            }
            fn set_prev_link(&mut self, prev: ilist::RawLink<$ty>) -> ilist::RawLink<$ty> {
                self.$field.set_prev_link(prev)
            }
        }
    }
}

impl_ilist_node!(Arg, links);
impl_ilist_node!(BasicBlock, links);
impl_ilist_node!(Instruction, links);
impl_ilist_node!(Use, links);

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "fn {}(", self.name));
        let mut args = self.args.iter();
        if let Some(arg) = args.next() {
            if let Some(ref n) = arg.name {
                try!(write!(f, "%{}: ", n));
            }

            try!(fmt::Debug::fmt(&*arg.ty, f));
        }
        while let Some(arg) = args.next() {
            try!(write!(f, ", "));
            if let Some(ref n) = arg.name {
                try!(write!(f, "%{}: ", n));
            }
            try!(fmt::Debug::fmt(&*arg.ty, f));
        }

        try!(write!(f, ") -> {:?}", self.get_return_type()));

        if self.has_body() {
            try!(f.write_str(" {\n"));

            for blk in self.blocks.iter() {
                try!(write!(f, "{}:", blk.name));

                if blk.preds.len() > 0 {
                    unsafe {
                        let mut pred_iter = blk.preds.iter();
                        if let Some(pred) = pred_iter.next() {
                            try!(write!(f, " // preds: {}", (**pred).name));
                        }
                        while let Some(pred) = pred_iter.next() {
                            try!(write!(f, ", {}", (**pred).name));
                        }

                    }
                }

                try!(f.write_str("\n"));

                for i in blk.instructions.iter() {
                    try!(write!(f, "  {:?}\n", i))
                }
                try!(f.write_str("\n"))
            }

            f.write_str("}\n")
        } else {
            f.write_str(";\n")
        }

    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print_binop(f: &mut fmt::Formatter, code: &str,
                       name: &Option<ImmString>, l: &UseRef, r: &UseRef) -> fmt::Result {
            let name = name.as_ref().map(|n| {
                n.as_slice()
            }).unwrap_or("<unknown>");

            write!(f, "%{} = {} {:?}, {:?}", name, code, l, r)
        }
        match self.op {
            Op::Add(ref l, ref r) => {
                print_binop(f, "add", &self.name, l, r)
            },
            Op::Sub(ref l, ref r) => {
                print_binop(f, "sub", &self.name, l, r)
            },
            Op::Mul(ref l, ref r) => {
                print_binop(f, "mul", &self.name, l, r)
            },
            Op::Div(ref l, ref r) => {
                print_binop(f, "div", &self.name, l, r)
            },
            Op::Rem(ref l, ref r) => {
                print_binop(f, "rem", &self.name, l, r)
            },
            Op::And(ref l, ref r) => {
                print_binop(f, "and", &self.name, l, r)
            },
            Op::Or(ref l, ref r) => {
                print_binop(f, "or", &self.name, l, r)
            },
            Op::Xor(ref l, ref r) => {
                print_binop(f, "xor", &self.name, l, r)
            },
            Op::Not(ref val) => {
                let name = self.name.as_ref().map(|n| {
                    n.as_slice()
                }).unwrap_or("<unknown>");

                write!(f, "%{} = not {:?}", name, val)
            },
            Op::Cmp(ref cmp, ref l, ref r) => {
                let name = self.name.as_ref().map(|n| {
                    n.as_slice()
                }).unwrap_or("<unknown>");

                let cmp = match *cmp {
                    Cmp::Eq => "eq",
                    Cmp::Ne => "ne",
                    Cmp::Lt => "lt",
                    Cmp::Le => "le",
                    Cmp::Gt => "gt",
                    Cmp::Ge => "ge",
                };

                write!(f, "%{} = cmp {}, {:?}, {:?}", name, cmp, l, r)
            },

            Op::Call(ref fun, ref args) => {
                let fun = fun.get_use().unwrap().producer();

                let retty = if let &ty::Ty::Fn(_, ref out) = &*fun.get_type() {
                    out.clone()
                } else {
                    panic!("Call function is not a function type");
                };

                match retty {
                    ty::FnOutput::Diverging => (),
                    ty::FnOutput::Converging(ty) => if (*ty) != ty::Ty::Nil {
                        let name = self.name.as_ref().map(|n| {
                            n.as_slice()
                        }).unwrap_or("<unknown>");
                        try!(write!(f, "%{} = ", name));
                    }
                }

                try!(write!(f, "call {:?}(", fun));
                let mut args = args.iter();
                if let Some(arg) = args.next() {
                    try!(write!(f, "{:?}", arg));
                }

                while let Some(arg) = args.next() {
                    try!(write!(f, ", {:?}", arg));
                }

                f.write_str(")")
            },

            Op::Store(ref dst, ref val) => {
                write!(f, "store {:?}, {:?}", dst, val)
            },
            Op::Load(ref src) => {
                let name = self.name.as_ref().map(|n| {
                    n.as_slice()
                }).unwrap_or("<unknown>");

                write!(f, "%{} = load {:?}", name, src)
            },
            Op::ExtractField(ref val, fld) => {
                let name = self.name.as_ref().map(|n| {
                    n.as_slice()
                }).unwrap_or("<unknown>");

                write!(f, "%{} = extractfield {:?}, {}",
                       name, val, fld)
            },
            Op::Index(ref val, ref idx) => {
                let name = self.name.as_ref().map(|n| {
                    n.as_slice()
                }).unwrap_or("<unknown>");

                write!(f, "%{} = index {:?}, {:?}", name, val, idx)
            },

            Op::Br(ref blk) => unsafe {
                let blk = &**blk;

                write!(f, "br {}", blk.name)
            },
            Op::CondBr(ref u, ref then, ref els) => unsafe {
                let then = &**then;
                let els = &**els;

                write!(f, "condbr {:?}, {}, {}", u, then.name, els.name)
            },
            Op::Return(ref u) => {
                write!(f, "ret {:?}", u)
            },

            Op::Phi(ref ins) => {
                let name = self.name.as_ref().map(|n| {
                    n.as_slice()
                }).unwrap_or("<unknown>");

                try!(write!(f, "%{} = phi ", name));
                let mut iter = ins.iter();
                if let Some(&(ref val, ref blk)) = iter.next() {
                    let blk = unsafe { &**blk };
                    try!(write!(f, "({:?}, {})", val, blk.name));
                }
                while let Some(&(ref val, ref blk)) = iter.next() {
                    let blk = unsafe { &**blk };
                    try!(write!(f, ", ({:?}, {})", val, blk.name));
                }
                Ok(())
            }
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ValueKind::None => f.write_str("none"),
            ValueKind::Inst(i) => unsafe {
                if let Some(ref n) = (*i).name {
                    write!(f, "%{}", n.as_slice())
                } else {
                    f.write_str("<unknown>")
                }
            },
            ValueKind::Const(c) => unsafe {
                fmt::Debug::fmt(&*c, f)
            },
            ValueKind::Arg(a) => unsafe {
                if let Some(ref n) = (*a).name {
                    write!(f, "%{}", n.as_slice())
                } else {
                    write!(f, "%arg.{}", (*a).idx)
                }
            },
            ValueKind::Function(fun) => unsafe {
                write!(f, "@{}", (*fun).name)
            }
        }
    }
}

impl fmt::Debug for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.data {
            ConstData::Undef(_) => f.write_str("undef"),
            ConstData::Nil => f.write_str("()"),
            ConstData::Bool(true) => f.write_str("true"),
            ConstData::Bool(false) => f.write_str("false"),
            ConstData::Int(val, 0) => write!(f, "{}is", val),
            ConstData::Uint(val, 0) => write!(f, "{}us", val),
            ConstData::Int(val, sz) => write!(f, "{}i{}", val, sz),
            ConstData::Uint(val, sz) => write!(f, "{}u{}", val, sz),
            ConstData::String(ref s) => write!(f, "{:?}", s),
        }
    }
}

impl fmt::Debug for UseRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(u) = self.get_use() {
            fmt::Debug::fmt(&u.producer(), f)
        } else {
            f.write_str("<no-def>")
        }
    }
}
