use std::borrow::BorrowFrom;
use std::fmt;
use std::hash::{Hash, Hasher, Writer};
use std::mem;
use std::ops;

#[derive(Eq)]
pub enum ImmString {
    Static(&'static str),
    Owned(Box<str>)
}

impl Clone for ImmString {
    fn clone(&self) -> ImmString {
        match self {
            &ImmString::Static(s) => ImmString::Static(s),
            &ImmString::Owned(ref s) => String::from_str(&**s).into_imm_string()
        }
    }
}

impl Str for ImmString {
    fn as_slice<'a>(&'a self) -> &'a str {
        match self {
            &ImmString::Static(s) => s,
            &ImmString::Owned(ref s) => s.as_slice(),
        }
    }
}

impl ops::Deref for ImmString {
    type Target = str;

    fn deref<'a>(&'a self) -> &'a str {
        self.as_slice()
    }
}

pub trait IntoImmString {
    fn into_imm_string(self) -> ImmString;
}

impl IntoImmString for &'static str {
    fn into_imm_string(self) -> ImmString {
        ImmString::Static(self)
    }
}

impl IntoImmString for String {
    fn into_imm_string(self) -> ImmString {
        let bytes = self.into_bytes();
        let bytes = bytes.into_boxed_slice();

        unsafe {
            ImmString::Owned(mem::transmute(bytes))
        }
    }
}

impl IntoImmString for Box<str> {
    fn into_imm_string(self) -> ImmString {
        ImmString::Owned(self)
    }
}

impl IntoImmString for ImmString {
    fn into_imm_string(self) -> ImmString { self }
}

impl fmt::Debug for ImmString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ImmString::Static(s) => fmt::Debug::fmt(&s, f),
            &ImmString::Owned(ref s) => fmt::Debug::fmt(&**s, f),
        }
    }
}

impl fmt::Display for ImmString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ImmString::Static(s) => fmt::Display::fmt(&s, f),
            &ImmString::Owned(ref s) => fmt::Display::fmt(&**s, f),
        }
    }
}

impl<S: Hasher + Writer> Hash<S> for ImmString {
    fn hash(&self, s: &mut S) {
        self.as_slice().hash(s);
    }
}

impl PartialEq for ImmString {
    fn eq(&self, other: &ImmString) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl BorrowFrom<ImmString> for str {
    fn borrow_from(owned: &ImmString) -> &str {
        &**owned
    }
}
