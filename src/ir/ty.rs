use arena::TypedArena;

use std::borrow::BorrowFrom;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher, Writer};

pub use self::TyData::*;
pub use self::DefData::*;
pub use self::Mutability::*;
pub use self::FnOutput::*;

pub type Ty<'tcx> = &'tcx TyS<'tcx>;
pub type Def<'tcx> = &'tcx DefS<'tcx>;

type TyInterner<'tcx> = HashMap<InternedTy<'tcx>, Ty<'tcx>>;
type DefInterner<'tcx> = HashMap<Def<'tcx>, Def<'tcx>>;

pub struct CtxtArenas<'tcx> {
    type_: TypedArena<TyS<'tcx>>,
    def: TypedArena<DefS<'tcx>>,
}

pub struct CommonTypes<'tcx> {
    pub nil: Ty<'tcx>,
    pub bool: Ty<'tcx>,
    pub char: Ty<'tcx>,
    pub u8: Ty<'tcx>,
    pub u16: Ty<'tcx>,
    pub u32: Ty<'tcx>,
    pub u64: Ty<'tcx>,
    pub usize: Ty<'tcx>,
    pub i8: Ty<'tcx>,
    pub i16: Ty<'tcx>,
    pub i32: Ty<'tcx>,
    pub i64: Ty<'tcx>,
    pub isize: Ty<'tcx>,
    pub f32: Ty<'tcx>,
    pub f64: Ty<'tcx>,
}

pub struct Context<'tcx> {
    arenas: &'tcx CtxtArenas<'tcx>,

    ty_interner: RefCell<TyInterner<'tcx>>,
    def_interner: RefCell<DefInterner<'tcx>>,

    pub types: CommonTypes<'tcx>,
}

impl<'tcx> CtxtArenas<'tcx> {
    pub fn new() -> CtxtArenas<'tcx> {
        CtxtArenas {
            type_: TypedArena::new(),
            def: TypedArena::new(),
        }
    }
}

impl<'tcx> CommonTypes<'tcx> {
    fn new(interner: &mut TyInterner<'tcx>, arena: &'tcx TypedArena<TyS<'tcx>>) -> CommonTypes<'tcx> {

        CommonTypes {
            nil:   intern_ty(arena, interner, Nil),
            bool:  intern_ty(arena, interner, Bool),
            char:  intern_ty(arena, interner, Char),
            u8:    intern_ty(arena, interner, Uint( 8)),
            u16:   intern_ty(arena, interner, Uint(16)),
            u32:   intern_ty(arena, interner, Uint(32)),
            u64:   intern_ty(arena, interner, Uint(64)),
            usize: intern_ty(arena, interner, Uint( 0)),
            i8:    intern_ty(arena, interner, Int( 8)),
            i16:   intern_ty(arena, interner, Int(16)),
            i32:   intern_ty(arena, interner, Int(32)),
            i64:   intern_ty(arena, interner, Int(64)),
            isize: intern_ty(arena, interner, Int( 0)),
            f32:   intern_ty(arena, interner, Float(32)),
            f64:   intern_ty(arena, interner, Float(64)),
        }
    }
}

impl<'tcx> Context<'tcx> {
    pub fn new(arenas: &'tcx CtxtArenas<'tcx>) -> Context<'tcx> {
        let mut ty_interner = HashMap::new();
        let types = CommonTypes::new(&mut ty_interner, &arenas.type_);
        Context {
            arenas: arenas,
            types: types,
            ty_interner: RefCell::new(ty_interner),
            def_interner: RefCell::new(HashMap::new()),
        }
    }

    fn def_ty(&self, def: DefS<'tcx>) -> Def<'tcx> {
        if let Some(def) = self.def_interner.borrow().get(&def) {
            return *def;
        }

        let def = self.arenas.def.alloc(def);
        {
            let mut interner = self.def_interner.borrow_mut();
            interner.insert(def, def);
        }
        def
    }

    pub fn def_struct(&self, name: String, tys: Vec<Ty<'tcx>>) -> Def<'tcx> {
        self.def_ty(DefS {
            name: name,
            data: DefStruct(tys)
        })
    }

    fn mk_t(&self, data: TyData<'tcx>) -> Ty<'tcx> {
        let mut interner = self.ty_interner.borrow_mut();
        intern_ty(&self.arenas.type_, &mut *interner, data)
    }

    pub fn mk_array(&self, ty: Ty<'tcx>, len: Option<usize>) -> Ty<'tcx> {
        self.mk_t(Array(ty, len))
    }

    pub fn mk_tup(&self, tys: Vec<Ty<'tcx>>) -> Ty<'tcx> {
        if tys.len() == 0 {
            self.types.nil
        } else {
            self.mk_t(Tup(tys))
        }
    }

    pub fn mk_bare_fn(&self, inputs: Vec<Ty<'tcx>>,
                      output: FnOutput<'tcx>, variadic: bool) -> Ty<'tcx> {
        let bfn = BareFnTy::new(inputs, output, variadic);

        self.mk_t(BareFn(bfn))
    }

    pub fn mk_ptr(&self, mt: MTy<'tcx>) -> Ty<'tcx> {
        self.mk_t(Ptr(mt))
    }

    pub fn mk_mut_ptr(&self, ty: Ty<'tcx>) -> Ty<'tcx> {
        let mt = MTy {
            mutability: Mutable,
            ty: ty
        };
        self.mk_ptr(mt)
    }

    pub fn mk_imm_ptr(&self, ty: Ty<'tcx>) -> Ty<'tcx> {
        let mt = MTy {
            mutability: Immutable,
            ty: ty
        };
        self.mk_ptr(mt)
    }

    pub fn mk_data(&self, def: Def<'tcx>) -> Ty<'tcx> {
        self.mk_t(Data(def))
    }

    pub fn inner_ty(&self, ty: Ty<'tcx>) -> Option<Ty<'tcx>> {
        match ty.data {
            Array(ty, _) => Some(ty),
            Ptr(ref mt) => Some(mt.ty),
            _ => None
        }
    }
}

fn intern_ty<'tcx>(arena: &'tcx TypedArena<TyS<'tcx>>,
                   interner: &mut TyInterner<'tcx>, data: TyData<'tcx>) -> Ty<'tcx> {
    if let Some(ty) = interner.get(&data) {
        return *ty;
    }

    let ty = arena.alloc(TyS {
        data: data,
    });
    interner.insert(InternedTy { ty: ty }, ty);
    ty
}

#[derive(Show, Eq)]
pub struct TyS<'tcx> {
    pub data: TyData<'tcx>
}

impl<'tcx> PartialEq for TyS<'tcx> {
    fn eq(&self, other: &TyS<'tcx>) -> bool {
        (self as *const _) == (other as *const _)
    }
}

impl<'tcx, S: Writer + Hasher> Hash<S> for TyS<'tcx> {
    fn hash(&self, s: &mut S) {
        (self as *const _).hash(s);
    }
}

#[derive(Eq)]
pub struct InternedTy<'tcx> {
    ty: Ty<'tcx>
}

impl<'tcx> PartialEq for InternedTy<'tcx> {
    fn eq(&self, other: &InternedTy<'tcx>) -> bool {
        self.ty.data == other.ty.data
    }
}

impl<'tcx, S: Writer + Hasher> Hash<S> for InternedTy<'tcx> {
    fn hash(&self, s: &mut S) {
        self.ty.data.hash(s);
    }
}

impl<'tcx> BorrowFrom<InternedTy<'tcx>> for TyData<'tcx> {
    fn borrow_from<'a>(ty: &'a InternedTy<'tcx>) -> &'a TyData<'tcx> {
        &ty.ty.data
    }
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub enum TyData<'tcx> {
    Nil,
    Bool,
    Char,
    Uint(u8),
    Int(u8),
    Float(u8),
    Array(Ty<'tcx>, Option<usize>),
    Ptr(MTy<'tcx>),
    Tup(Vec<Ty<'tcx>>),
    BareFn(BareFnTy<'tcx>),
    Data(Def<'tcx>),
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub struct MTy<'tcx> {
    pub mutability: Mutability,
    pub ty: Ty<'tcx>
}

#[derive(Copy, Clone, Show, PartialEq, Eq, Hash)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub struct BareFnTy<'tcx> {
    pub abi: Abi,
    pub sig: FnSig<'tcx>,
}

impl<'tcx> BareFnTy<'tcx> {
    fn new(inputs: Vec<Ty<'tcx>>, output: FnOutput<'tcx>, variadic: bool) -> BareFnTy<'tcx> {
        BareFnTy {
            abi: Abi::Rust,
            sig: FnSig {
                inputs: inputs,
                output: output,
                variadic: variadic
            }
        }
    }
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub struct FnSig<'tcx> {
    pub inputs: Vec<Ty<'tcx>>,
    pub output: FnOutput<'tcx>,
    pub variadic: bool,
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub enum FnOutput<'tcx> {
    FnConverging(Ty<'tcx>),
    FnDiverging
}

impl<'tcx> FnOutput<'tcx> {
    pub fn diverges(&self) -> bool {
        match *self {
            FnDiverging => true,
            FnConverging(_) => false
        }
    }

    pub fn unwrap(&self) -> Ty<'tcx> {
        match *self {
            FnConverging(ty) => ty,
            FnDiverging => panic!("Can't unwrap a diverging type"),
        }
    }
}

#[derive(Copy, Clone, Show, PartialEq, Eq, Hash)]
pub enum Abi {
    Rust,
    RustCall,
    RustIntrinsic,
    C
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
struct DefS<'tcx> {
    pub name: String,
    pub data: DefData<'tcx>,
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub enum DefData<'tcx> {
    DefEmpty, // Empty data type, eg enum Void { }
    DefStruct(Vec<Ty<'tcx>>), // Standard struct data type
    DefEnum(EnumValues), // "C-Like" enum, no variants have fields
    DefADT(Variants<'tcx>), // Full algebraic data type
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub struct Variants<'tcx> {
    fields: Vec<Ty<'tcx>>
}

#[derive(Clone, Show, PartialEq, Eq, Hash)]
pub struct EnumValues {
    values: Vec<u64>
}

impl EnumValues {
    pub fn new() -> EnumValues {
        EnumValues {
            values: Vec::new()
        }
    }

    pub fn add_value(&mut self, val: u64) {
        self.values.push(val)
    }
}
