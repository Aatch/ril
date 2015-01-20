use std::collections::HashMap;
use std::hash::{Hash,Hasher,Writer};
use std::fmt;

pub struct Name<T>(usize);
impl<T> Copy for Name<T> { }

impl<T> Clone for Name<T> {
    fn clone(&self) -> Name<T> {
        *self
    }
}

impl<T> PartialEq for Name<T> {
    fn eq(&self, other: &Name<T>) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Name<T> { }

impl<T, H: Hasher+Writer> Hash<H> for Name<T> {
    fn hash(&self, hasher: &mut H) {
        self.0.hash(hasher);
    }
}

impl<T> Name<T> {
    fn new() -> Name<T> {
        Name(1)
    }
    #[inline]
    fn next(&mut self) -> Name<T> {
        let this = *self;
        self.0 += 1;
        this
    }
    #[inline]
    pub fn invalid() -> Name<T> {
        Name(0)
    }
    #[inline]
    pub fn is_valid(&self) -> bool {
        self.0 != 0
    }

    #[inline]
    pub fn get(&self) -> usize {
        self.0
    }
}

pub struct UniqueMap<T> {
    map: HashMap<Name<T>, T>,
    cur_name: Name<T>
}

pub trait Named {
    fn set_name(&mut self, name: Name<Self>);
}

impl<T:Named> UniqueMap<T> {
    pub fn new() -> UniqueMap<T> {
        UniqueMap {
            map: HashMap::new(),
            cur_name: Name::new()
        }
    }

    pub fn unique(&mut self, mut v: T) -> Name<T> {
        let name = self.cur_name.next();
        v.set_name(name);
        let res = self.map.insert(name, v);
        assert!(res.is_none());
        name
    }

    #[inline]
    pub fn get<'a>(&'a self, name: Name<T>) -> Option<&'a T> {
        self.map.get(&name)
    }

    #[inline]
    pub fn get_mut<'a>(&'a mut self, name: Name<T>) -> Option<&'a mut T> {
        self.map.get_mut(&name)
    }

    #[inline]
    pub fn remove(&mut self, name: Name<T>) -> Option<T> {
        self.map.remove(&name)
    }
}

impl<T> fmt::Show for Name<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Name({})", self.0)
    }
}
