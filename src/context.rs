
use std::borrow::BorrowFrom;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

use imm_str::ImmString;
use ir;
use ty;

type ConstInterner = HashMap<ir::InternedConst, Box<ir::Constant>>;

pub struct CommonTypes {
    pub nil:   *const ty::Ty,
    pub bool:  *const ty::Ty,
    pub isize: *const ty::Ty,
    pub i8:    *const ty::Ty,
    pub i16:   *const ty::Ty,
    pub i32:   *const ty::Ty,
    pub i64:   *const ty::Ty,
    pub usize: *const ty::Ty,
    pub u8:    *const ty::Ty,
    pub u16:   *const ty::Ty,
    pub u32:   *const ty::Ty,
    pub u64:   *const ty::Ty,
    pub str:   *const ty::Ty,
}

impl CommonTypes {
    fn new(interner: &mut ty::TyInterner) -> CommonTypes {
        CommonTypes {
            nil:    intern_ty(interner, ty::Ty::Nil),
            bool:   intern_ty(interner, ty::Ty::Bool),
            isize:  intern_ty(interner, ty::Ty::Int(0)),
            i8:     intern_ty(interner, ty::Ty::Int(8)),
            i16:    intern_ty(interner, ty::Ty::Int(16)),
            i32:    intern_ty(interner, ty::Ty::Int(32)),
            i64:    intern_ty(interner, ty::Ty::Int(64)),
            usize:  intern_ty(interner, ty::Ty::Uint(0)),
            u8:     intern_ty(interner, ty::Ty::Uint(8)),
            u16:    intern_ty(interner, ty::Ty::Uint(16)),
            u32:    intern_ty(interner, ty::Ty::Uint(32)),
            u64:    intern_ty(interner, ty::Ty::Uint(64)),
            str:    intern_ty(interner, ty::Ty::String),
        }
    }
}

pub struct Context {
    ty_interner: RefCell<ty::TyInterner>,
    const_interner: RefCell<ConstInterner>,
    pub types: CommonTypes,
    funcs: RefCell<HashMap<ImmString, Box<ir::Function>>>,
    named_types: RefCell<HashMap<ImmString, *const ty::Ty>>,
}

impl Context {
    pub fn new() -> Box<Context> {
        let mut interner = HashMap::new();
        let types = CommonTypes::new(&mut interner);

        box Context {
            ty_interner: RefCell::new(interner),
            const_interner: RefCell::new(HashMap::new()),
            types: types,
            funcs: RefCell::new(HashMap::new()),
            named_types: RefCell::new(HashMap::new()),
        }
    }

    pub fn intern_ty(&self, ty: ty::Ty) -> *const ty::Ty {
        let mut interner = self.ty_interner.borrow_mut();
        let typ = intern_ty(&mut *interner, ty);

        let ty = unsafe { &*typ };
        if let Some(n) = ty.get_name() {
            let name = n.clone();
            let mut map = self.named_types.borrow_mut();
            let old = map.insert(name, typ);

            assert!(old.is_none(), "Overwrote an existing type with the same name");
        }

        typ
    }

    pub fn insert_function(&self, mut f: Box<ir::Function>) {
        f.set_context(self);
        let mut funcs = self.funcs.borrow_mut();
        let prev = funcs.insert(f.name.clone(), f);
        assert!(prev.is_none(), "Overrode existing function!");
    }

    pub fn get_function<S:?Sized>(&self, name: &S) -> Option<*mut ir::Function>
    where str: BorrowFrom<S> {
        let name_s : &str = BorrowFrom::borrow_from(name);
        let mut funcs = self.funcs.borrow_mut();
        funcs.get_mut(name_s).map(|f| {
            &mut **f as *mut _
        })
    }

    pub fn intern_const(&self, data: ir::ConstData) -> *mut ir::Constant {
        let mut interner = self.const_interner.borrow_mut();

        match interner.get_mut(&data) {
            Some(mut c) => return &mut **c,
            None => ()
        }

        let mut c = box ir::Constant::new(self, data);
        let cp = &mut *c as *mut _;

        interner.insert(ir::InternedConst {c: cp}, c);

        cp
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        self.funcs.borrow_mut().clear();
        self.const_interner.borrow_mut().clear();
    }
}

fn intern_ty(interner: &mut ty::TyInterner, ty: ty::Ty) -> *const ty::Ty {
    match interner.get(&ty) {
        Some(ty) => return &**ty,
        None => ()
    }
    let ty = box ty;
    let typ = &*ty as *const _;


    interner.insert(ty::InternedTy { ty: typ }, ty);

    typ
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = self.named_types.borrow();
        for &ty in types.values() {
            unsafe {
                match &*ty {
                    &ty::Ty::Struct(ref sty) => try!(write!(f, "{:?}\n", sty)),
                    _ => ()
                }
            }
        }
        try!(write!(f, "\n"));

        let funcs = self.funcs.borrow();
        for func in funcs.values() {
            try!(fmt::Debug::fmt(&**func, f));
            try!(f.write_str("\n"));
        }
        Ok(())
    }
}
