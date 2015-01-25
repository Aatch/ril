#![allow(raw_pointer_derive)]

use std::borrow::BorrowFrom;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Writer, Hasher};
use std::ptr;

use context::Context;
use imm_str::{ImmString,IntoImmString};

pub type TyInterner = HashMap<InternedTy, Box<Ty>>;

#[derive(Hash,PartialEq,Eq)]
pub enum Ty {
    /// Nil type - ()
    Nil,
    /// Boolean type
    Bool,
    /// Signed integer type of arbitrary width, '0' is 'isize'
    Int(usize),
    /// Unsigned integer type of arbitrary width, '0' is 'usize'
    Uint(usize),
    /// String type - str
    String,
    /// Raw Pointer - *T
    Ptr(bool, *const Ty),
    /// Region Pointer - &T
    RPtr(bool, Region, *const Ty),
    /// Fixed-sized array type - [T; n]
    Array(*const Ty, usize),
    /// Slice type - [T]
    Slice(*const Ty),
    /// Tuple Type
    Tuple(Box<[*const Ty]>),
    /// Function Type
    Fn(Box<[*const Ty]>, FnOutput),
    /// Struct Type
    Struct(StructTy),
}

impl Ty {
    pub fn is_fn_ty(&self) -> bool {
        match *self {
            Ty::Fn(..) => true,
            _ => false
        }
    }

    pub fn is_pointer(&self) -> bool {
        match *self {
            Ty::Ptr(..) |
            Ty::RPtr(..) => true,
            _ => false
        }
    }

    pub fn is_uint(&self) -> bool {
        match *self {
            Ty::Uint(..) => true,
            _ => false
        }
    }

    pub fn pointee(&self) -> *const Ty {

        match *self {
            Ty::Ptr(_, ptr) => ptr,
            Ty::RPtr(_, _, ptr) => ptr,
            _ => panic!("Called `pointee` on a non-pointer type")
        }
    }

    pub fn get_name<'a>(&'a self) -> Option<&'a ImmString> {
        match *self {
            Ty::Struct(ref sty) => Some(&sty.name),
            _ => None,
        }
    }

    pub fn int(cx: &Context, sz: usize) -> *const Ty {
        match sz {
            0  => cx.types.isize,
            8  => cx.types.i8,
            16 => cx.types.i16,
            32 => cx.types.i32,
            64 => cx.types.i64,
            _ => cx.intern_ty(Ty::Int(sz)),
        }
    }
    pub fn uint(cx: &Context, sz: usize) -> *const Ty {
        match sz {
            0  => cx.types.usize,
            8  => cx.types.u8,
            16 => cx.types.u16,
            32 => cx.types.u32,
            64 => cx.types.u64,
            _ => cx.intern_ty(Ty::Uint(sz)),
        }
    }

    pub fn ptr(cx: &Context, mutbl: bool, ty: *const Ty) -> *const Ty {
        cx.intern_ty(Ty::Ptr(mutbl, ty))
    }

    pub fn rptr(cx: &Context, mutbl: bool, ty: *const Ty) -> *const Ty {
        cx.intern_ty(Ty::RPtr(mutbl, Region::Anon, ty))
    }

    pub fn array(cx: &Context, ty: *const Ty, sz: usize) -> *const Ty {
        cx.intern_ty(Ty::Array(ty, sz))
    }

    pub fn slice(cx: &Context, ty: *const Ty) -> *const Ty {
        cx.intern_ty(Ty::Slice(ty))
    }

    pub fn tuple(cx: &Context, tys: Box<[*const Ty]>) -> *const Ty {
        cx.intern_ty(Ty::Tuple(tys))
    }

    pub fn fn_(cx: &Context, inputs: Box<[*const Ty]>, output: FnOutput) -> *const Ty {
        cx.intern_ty(Ty::Fn(inputs, output))
    }

    pub fn nil(cx: &Context) -> *const Ty {
        cx.types.nil
    }
    pub fn bool(cx: &Context) -> *const Ty {
        cx.types.bool
    }
    pub fn isize(cx: &Context) -> *const Ty {
        cx.types.isize
    }
    pub fn i8(cx: &Context) -> *const Ty {
        cx.types.i8
    }
    pub fn i16(cx: &Context) -> *const Ty {
        cx.types.i16
    }
    pub fn i32(cx: &Context) -> *const Ty {
        cx.types.i32
    }
    pub fn i64(cx: &Context) -> *const Ty {
        cx.types.i64
    }
    pub fn usize(cx: &Context) -> *const Ty {
        cx.types.usize
    }
    pub fn u8(cx: &Context) -> *const Ty {
        cx.types.u8
    }
    pub fn u16(cx: &Context) -> *const Ty {
        cx.types.u16
    }
    pub fn u32(cx: &Context) -> *const Ty {
        cx.types.u32
    }
    pub fn u64(cx: &Context) -> *const Ty {
        cx.types.u64
    }
    pub fn str(cx: &Context) -> *const Ty {
        cx.types.str
    }

    pub fn struct_<S:IntoImmString>(cx: &Context, name: S, fields: Box<[*const Ty]>) -> *const Ty {
        if fields.len() == 0 {
            cx.intern_ty(Ty::Struct(StructTy {
                name: name.into_imm_string(),
                inner: ptr::null(),
            }))
        } else if fields.len() == 1 {
            let fty = fields[0];
            let fty = unsafe {
                match *fty {
                    Ty::Tuple(..) => Ty::tuple(cx, fields),
                    _ => fty,
                }
            };

            cx.intern_ty(Ty::Struct(StructTy {
                name: name.into_imm_string(),
                inner: fty,
            }))
        } else {
            cx.intern_ty(Ty::Struct(StructTy {
                name: name.into_imm_string(),
                inner: Ty::tuple(cx, fields),
            }))
        }
    }
}

#[derive(Hash,PartialEq,Eq)]
pub enum Region {
    Anon,
}

#[derive(Clone,Hash,PartialEq,Eq)]
pub enum FnOutput {
    Diverging,
    Converging(*const Ty)
}

#[derive(Eq)]
pub struct InternedTy {
    pub ty: *const Ty
}

#[derive(Clone,Hash,PartialEq,Eq)]
pub struct StructTy {
    name: ImmString,
    // Cheat a little here, the inner type is used to represent the contents of the struct, newtype
    // and single-element structs have the wrapped type represented directly. Structs with multiple
    // fields are represented by a tuple inner type. In the relatively-rare case that we need to
    // represented a struct like `struct Foo((A,));`, a newtype over a tuple, a single-element tuple
    // containing the actual tuple is used.
    // Also, we use a null value here to represent a unit type (`struct Bar;`)
    inner: *const Ty
}

impl StructTy {
    #[inline]
    pub fn num_fields(&self) -> u32 {
        if self.inner.is_null() {
            0
        } else {
            let inner = unsafe { &*self.inner };
            match inner {
                &Ty::Tuple(ref tys) => tys.len() as u32,
                _ => 1,
            }
        }
    }

    #[inline]
    pub fn get_field(&self, idx: u32) -> *const Ty {
        assert!(idx < self.num_fields());

        let inner = unsafe { &*self.inner };
        match inner {
            &Ty::Tuple(ref tys) => tys[idx as usize],
            ty => ty
        }
    }
}

impl<S: Hasher + Writer> Hash<S> for InternedTy {
    fn hash(&self, s: &mut S) {
        unsafe {
            (*self.ty).hash(s);
        }
    }
}

impl PartialEq for InternedTy {
    fn eq(&self, other: &InternedTy) -> bool {
        unsafe {
            (*self.ty).eq(&*other.ty)
        }
    }
}

impl BorrowFrom<InternedTy> for Ty {
    fn borrow_from(owned: &InternedTy) -> &Ty {
        unsafe { &*owned.ty }
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Ty::Nil => f.write_str("()"),
            &Ty::Bool => f.write_str("bool"),
            &Ty::Int(0) => f.write_str("isize"),
            &Ty::Uint(0) => f.write_str("usize"),
            &Ty::Int(sz) => write!(f, "i{}", sz),
            &Ty::Uint(sz) => write!(f, "u{}", sz),
            &Ty::String => f.write_str("str"),
            &Ty::Ptr(true, ty) => unsafe {
                write!(f, "*mut {:?}", &*ty)
            },
            &Ty::Ptr(false, ty) => unsafe {
                write!(f, "*const {:?}", &*ty)
            },
            &Ty::RPtr(true, ref region, ty) => unsafe {
                write!(f, "&{:?}mut {:?}", region, &*ty)
            },
            &Ty::RPtr(false, ref region, ty) => unsafe {
                write!(f, "&{:?}{:?}", region, &*ty)
            },
            &Ty::Array(ty, sz) => unsafe {
                write!(f, "[{:?}; {}]", &*ty, sz)
            },
            &Ty::Slice(ty) => unsafe {
                write!(f, "[{:?}]", &*ty)
            },
            &Ty::Tuple(ref tys) => {
                try!(f.write_str("("));
                let mut tys = tys.iter();
                if let Some(ty) = tys.next() {
                    try!(fmt::Debug::fmt(&*ty, f));
                }
                while let Some(ty) = tys.next() {
                    try!(f.write_str(", "));
                    try!(fmt::Debug::fmt(&*ty, f));
                }
                f.write_str(")")
            },
            &Ty::Fn(ref ins, ref out) => unsafe {
                try!(f.write_str("fn("));
                let mut ins = ins.iter();
                if let Some(ty) = ins.next() {
                    try!(fmt::Debug::fmt(&*ty, f));
                }
                while let Some(ty) = ins.next() {
                    try!(f.write_str(", "));
                    try!(fmt::Debug::fmt(&*ty, f));
                }
                try!(f.write_str(") -> "));
                if let &FnOutput::Converging(ty) = out {
                    fmt::Debug::fmt(&*ty, f)
                } else {
                    f.write_str("!")
                }
            },
            &Ty::Struct(ref sty) => {
                write!(f, "%{}", sty.name)
            }
        }
    }
}

impl fmt::Debug for FnOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &FnOutput::Diverging => f.write_str("!"),
            &FnOutput::Converging(ty) => unsafe {
                fmt::Debug::fmt(&*ty, f)
            }
        }
    }
}

impl fmt::Debug for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Region::Anon => Ok(())
        }
    }
}

impl fmt::Debug for StructTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            try!(write!(f, "struct %{}", self.name));

            let num_fields = self.num_fields();
            if num_fields == 0 {
                return f.write_str(";");
            } else if num_fields == 1 {
                return write!(f, "({:?});", &*self.get_field(0));
            }

            try!(write!(f, " {{ {:?}", &*self.get_field(0)));

            for i in (1..self.num_fields()) {
                try!(write!(f, ", {:?}", &*self.get_field(i)));
            }

            f.write_str("}")
        }
    }
}
