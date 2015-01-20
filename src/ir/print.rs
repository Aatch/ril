use std::collections::HashMap;

use ir;
use ir::ty;

pub struct IRPrinter<'a, 'tcx:'a> {
    f: &'a ir::Function<'tcx>,
    inst_names: HashMap<ir::InstName<'tcx>, String>,
    name_count: usize,
}

impl<'a, 'tcx> IRPrinter<'a, 'tcx> {

    pub fn new(f: &'a ir::Function<'tcx>) -> IRPrinter<'a, 'tcx> {
        IRPrinter {
            f: f,
            inst_names: HashMap::new(),
            name_count: 0
        }
    }

    pub fn print(&mut self) {
        self.print_type(self.f.ty);
        println!(" {{");

        for &bn in self.f.blocks.iter() {
            if let Some(blk) = self.f.block_map.get(bn) {
                self.print_block(blk);
            }
        }

        println!("}}");
    }

    fn print_block(&mut self, blk: &ir::BasicBlock<'tcx>) {
        println!("block.{}:", blk.name.get());

        for &i in blk.instructions.iter() {
            if let Some(inst) = self.f.inst_map.get(i) {
                print!("  ");
                self.print_instruction(inst);
                print!("\n");
            }
        }
    }

    fn print_instruction(&mut self, inst: &ir::insts::Instruction<'tcx>) {
        use ir::insts::Cmp as CmpKind;
        use ir::insts::Op::*;

        match inst.op {
            Add(l, r) => self.print_binop("add", inst.name, inst.ty, l, r),
            Sub(l, r) => self.print_binop("sub", inst.name, inst.ty, l, r),
            Mul(l, r) => self.print_binop("mul", inst.name, inst.ty, l, r),
            Div(l, r) => self.print_binop("div", inst.name, inst.ty, l, r),
            Rem(l, r) => self.print_binop("rem", inst.name, inst.ty, l, r),
            Shl(l, r) => self.print_binop("shl", inst.name, inst.ty, l, r),
            Shr(l, r) => self.print_binop("shr", inst.name, inst.ty, l, r),
            And(l, r) => self.print_binop("and", inst.name, inst.ty, l, r),
            Or(l, r)  => self.print_binop("or", inst.name, inst.ty, l, r),
            Xor(l, r) => self.print_binop("xor", inst.name, inst.ty, l, r),
            Index(base, offset) => self.print_binop("index", inst.name, inst.ty, base, offset),
            Not(v) => {
                {
                    let name = self.get_name(inst.name);
                    print!("{}:", name);
                }
                self.print_type(inst.ty);

                print!(" = not ");
                self.print_value(v);
            }
            Cmp(kind, l, r) => {
                {
                    let name = self.get_name(inst.name);
                    print!("{}:", name);
                }
                self.print_type(inst.ty);
                print!(" = cmp ");
                match kind {
                    CmpKind::Eq => print!("eq "),
                    CmpKind::Ne => print!("ne "),
                    CmpKind::Lt => print!("lt "),
                    CmpKind::Le => print!("le "),
                    CmpKind::Gt => print!("gt "),
                    CmpKind::Ge => print!("ge "),
                }
                self.print_value(l);
                print!(", ");
                self.print_value(r);
            }
            Local(init) => {
                {
                    let name = self.get_name(inst.name);
                    print!("{}:", name);
                }
                self.print_type(inst.ty);
                print!(" = local ");
                self.print_value(init);
            }
            Load(v) => {
                {
                    let name = self.get_name(inst.name);
                    print!("{}:", name);
                }
                self.print_type(inst.ty);
                print!(" = load ");
                self.print_value(v);
            }
            Store(to, val) => {
                print!("store ");
                self.print_value(to);
                print!(", ");
                self.print_value(val);
            }
            Phi(ref set) => {
                {
                    let name = self.get_name(inst.name);
                    print!("{}:", name);
                }
                self.print_type(inst.ty);
                print!(" = phi ");

                let mut comma = false;
                for &(val, bn) in set.iter() {
                    if comma {
                        print!(", ");
                    }
                    print!("(");
                    self.print_value(val);
                    print!(", block.{})", bn.get());
                    comma = true;
                }
            }
            Br(blk) => print!("br block.{}", blk.get()),
            CondBr(val, blk_then, blk_else) => {
                print!("condbr ");
                self.print_value(val);
                print!(", block.{}, block.{}", blk_then.get(), blk_else.get());
            }
            Return(val) => {
                print!("return ");
                self.print_value(val);
            }
        }
    }

    fn print_binop(&mut self, op_name: &str, name: ir::InstName<'tcx>, ty: ty::Ty<'tcx>, l: ir::Value<'tcx>, r: ir::Value<'tcx>) {
        {
            let name = self.get_name(name);
            print!("{}:", name);
        }
        self.print_type(ty);
        print!(" = {} ", op_name);
        self.print_value(l);
        print!(", ");
        self.print_value(r);
    }

    fn print_value(&mut self, v: ir::Value<'tcx>) {
        use ir::Value::*;
        use ir::Constant::*;
        match v {
            Const(Nil) => print!("()"),
            Const(Bool(b)) => if b { print!("true") } else { print!("false") },
            Const(Zero(_)) => print!("zeroinitializer"),
            Const(Undef(_)) => print!("undef"),
            Const(Int(_, val)) => print!("{}i", val),
            Const(Uint(_, val)) => print!("{}u", val),
            Inst(nm) => print!("{}", self.get_name(nm)),
            Arg(num) => print!("arg[{}]", num)
        }
    }

    fn print_type(&self, ty: ty::Ty<'tcx>) {
        match ty.data {
            ty::Nil => print!("()"),
            ty::Bool => print!("bool"),
            ty::Char => print!("char"),
            ty::Uint(0) => print!("us"),
            ty::Int(0) => print!("is"),
            ty::Uint(sz) => print!("u{}", sz),
            ty::Int(sz) => print!("i{}", sz),
            ty::Float(sz) => print!("f{}", sz),
            ty::Array(ty, None) => {
                print!("["); self.print_type(ty); print!("]");
            }
            ty::Array(ty, Some(sz)) => {
                print!("["); self.print_type(ty); print!(";{}]",sz);
            }
            ty::Ptr(ref mt) => {
                match mt.mutability {
                    ty::Immutable => print!("*const "),
                    ty::Mutable => print!("*mut "),
                }
                self.print_type(mt.ty);
            }
            ty::Tup(ref tys) => {
                print!("(");
                let mut comma = false;
                for &ty in tys.iter() {
                    if comma {
                        print!(", ");
                    }
                    self.print_type(ty);
                    comma = true;
                }
                print!(")");
            }
            ty::BareFn(ref ty) => {
                match ty.abi {
                    ty::Abi::Rust => (),
                    ty::Abi::RustCall => print!("extern \"rust-call\" "),
                    ty::Abi::RustIntrinsic => print!("extern \"rust-intrinsic\" "),
                    ty::Abi::C => print!("extern \"C\" "),
                }

                self.print_fn_sig(&ty.sig);
            }
            ty::Data(def) => {
                print!("{}", &def.name[]);
            }
        }
    }

    fn print_fn_sig(&self, sig: &ty::FnSig<'tcx>) {
        print!("fn (");
        let mut comma = false;
        for &ty in sig.inputs.iter() {
            if comma {
                print!(", ");
            }
            comma = true;
            self.print_type(ty);
        }

        print!(") -> ");
        if sig.output.diverges() {
            print!("!");
        } else {
            self.print_type(sig.output.unwrap());
        }
    }

    fn get_name<'b>(&'b mut self, name: ir::InstName<'tcx>) -> &'b str {
        let entry = self.inst_names.entry(name);
        match entry.get() {
            Ok(n) => &n[],
            Err(v) => {
                let n = v.insert(format!("%{}", self.name_count));
                self.name_count += 1;
                &n[]
            }
        }
    }
}
