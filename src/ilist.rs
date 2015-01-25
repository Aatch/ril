//! An Intrusive Doubly-Linked List.

use core::nonzero::NonZero;

use std::fmt;
use std::iter;
use std::mem;

pub struct IList<T> where T : IListNode {
    length: usize,
    head: Option<Box<T>>,
    tail: Option<NonZero<*mut T>>
}

pub type Link<T> = Option<Box<T>>;
pub type RawLink<T> = Option<NonZero<*mut T>>;

#[inline(always)]
fn mk_rawlink<T>(v: *mut T) -> RawLink<T> {
    unsafe {
        if v.is_null() {
            None
        } else {
            Some(NonZero::new(v))
        }
    }
}

pub unsafe trait IListNode {
    fn next<'a>(&'a self) -> Option<&'a Self>;
    fn prev<'a>(&'a self) -> Option<&'a Self>;

    fn next_mut<'a>(&'a mut self) -> Option<&'a mut Self>;
    fn prev_mut<'a>(&'a mut self) -> Option<&'a mut Self>;

    fn set_next_link(&mut self, next: Link<Self>) -> Link<Self>;
    fn set_prev_link(&mut self, prev: RawLink<Self>) -> RawLink<Self>;

    fn on_insert(&mut self) { }
    fn on_remove(&mut self) { }
}

impl<T> IList<T> where T : IListNode {
    pub fn new() -> IList<T> {
        IList {
            length: 0,
            head: None,
            tail: None
        }
    }

    pub fn head<'a>(&'a self) -> Option<&'a T> {
        self.head.as_ref().map(|x| &**x)
    }

    pub fn head_mut<'a>(&'a mut self) -> Option<&'a mut T> {
        self.head.as_mut().map(|x| &mut **x)
    }

    pub fn tail<'a>(&'a self) -> Option<&'a T> {
        unsafe {
            self.tail.as_ref().map(|x| &***x)
        }
    }

    pub fn tail_mut<'a>(&'a mut self) -> Option<&'a mut T> {
        unsafe {
            self.tail.as_mut().map(|x| &mut ***x)
        }
    }

    pub fn push_front(&mut self, new: Box<T>) {
        unsafe {
            if self.head.is_none() {
                self.push_back(new);
                return;
            }

            let head = {
                let h = self.head.as_mut().unwrap();
                &mut **h as *mut _
            };

            self.insert_before(&mut *head, new);
        }
    }

    pub fn push_back(&mut self, mut new: Box<T>) {
        unsafe {
            if let Some(tail) = self.tail {
                self.insert_after(&mut **tail, new);
            } else {
                assert!(self.head.is_none());

                let newp = &mut *new as *mut _;

                new.on_insert();

                self.head = Some(new);
                self.tail = mk_rawlink(newp);
                self.length = 1;
            }
        }
    }

    pub fn pop_front(&mut self) -> Option<Box<T>> {
        self.head.take().map(|mut head| {
            self.length -= 1;
            head.on_remove();
            match head.set_next_link(None) {
                None => self.tail = None,
                Some(next) => self.head = Some(next)
            }

            head
        })
    }

    pub fn pop_back(&mut self) -> Option<Box<T>> {
        unsafe {
            self.tail.take().and_then(|tail| {
                self.length -= 1;
                (**tail).on_remove();
                match (**tail).prev_mut().take() {
                    // This is the head node
                    None => self.head.take(),
                    Some(prev) => {
                        self.tail = mk_rawlink(prev);
                        prev.set_next_link(None).take()
                    }
                }
            })
        }
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, T> {
        Iter {
            nelem: self.length,
            head: self.head(),
            tail: self.tail()
        }
    }

    pub fn cursor<'a>(&'a mut self) -> Cursor<'a, T> {
        Cursor {
            list: self,
            prev: None,
        }
    }

    pub fn get<'a>(&'a self, idx: u32) -> Option<&'a T> {
        let mut node = self.head();
        let mut i = 0;

        while let Some(n) = node {
            if i == idx { return Some(n); }
            node = n.next();
            i += 1;
        }

        return None;
    }

    pub fn get_mut<'a>(&'a mut self, idx: u32) -> Option<&'a mut T> {
        let mut node = self.head_mut();
        let mut i = 0;

        while let Some(n) = node {
            if i == idx { return Some(n); }
            node = n.next_mut();
            i += 1;
        }

        return None;
    }

    pub fn clear(&mut self) {
        // Pop each element off the front to avoid recursive destruction
        // of the list
        while let Some(_) = self.pop_front() {}
        assert_eq!(self.length, 0);
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
}

#[unsafe_destructor]
impl<T:IListNode> Drop for IList<T> {
    fn drop(&mut self) {
        self.clear()
    }
}

impl<T> IList<T> where T : IListNode {
    pub unsafe fn insert_before(&mut self, node: *mut T, mut new: Box<T>) {
        if let Some(prev) = (*node).prev_mut() {
            // Node is not the head of the list, insert after the previous node
            self.insert_after(prev, new);
        } else {
            // Node is the head of the list
            assert!(new.next().is_none() &&
                    new.prev().is_none(), "New node shouldn't have any links");
            let newp = &mut *new as *mut _;

            self.length += 1;

            let mut old_head = mem::replace(&mut self.head, Some(new));
            {
                if let Some(ref mut old_head) = old_head {
                    old_head.set_prev_link(mk_rawlink(newp));
                }
            }
            self.head_mut().unwrap().set_next_link(old_head);
        }
    }

    pub unsafe fn insert_after(&mut self, node: *mut T, mut new: Box<T>) {
        assert!(new.next().is_none() &&
                new.prev().is_none(), "New node shouldn't have any links");

        new.on_insert();
        let newp = &mut *new as *mut _;

        // Swap the current next link out for the new one
        let old_next = (*node).set_next_link(Some(new));

        self.length += 1;

        // Re-extract the new node
        let next = (*node).next_mut().unwrap();

        if let Some(mut old_next) = old_next {
            // Wire up the prev link for the old next node
            old_next.set_prev_link(mk_rawlink(newp));

            // Wire up the next link for the new node
            next.set_next_link(Some(old_next));
        } else {
            self.tail = mk_rawlink(newp);
        }

        // Wire up the prev link for the new node
        next.set_prev_link(mk_rawlink(node));
    }

    /// Removes the given node from the list. The node must be in the list.
    pub unsafe fn remove_node(&mut self, node: *mut T) -> Box<T> {
        // Check to see if this node is the head or the tail and use the appropriate
        // pop_* methods to remove the node. Since the node given must be in the list
        // already, we know that neither the head nor the tail are None
        let headp = self.head.as_mut().map(|x| &mut **x as *mut _).unwrap();
        if node == headp {
            return self.pop_front().unwrap();
        }
        let tailp = self.tail.map(|x| *x).unwrap();
        if node == tailp {
            return self.pop_back().unwrap();
        }

        // Node is in the middle of the list somewhere, decrement the length now
        self.length -= 1;

        // Get the prev and next links from the passed node.
        let prev = (*node).set_prev_link(None).unwrap();
        let mut next = (*node).set_next_link(None).unwrap();

        // Set the next node's prev link to the prev node
        next.set_prev_link(Some(prev));
        // Set the prev node's next link to the next node, getting the box for the
        // old node out.
        (**prev).set_next_link(Some(next)).unwrap()
    }
}

impl<A: IListNode> iter::FromIterator<Box<A>> for IList<A> {
    fn from_iter<T: Iterator<Item=Box<A>>>(mut iterator: T) -> IList<A> {
        let mut list = IList::new();
        for elt in iterator {
            list.push_back(elt);
        }
        return list;
    }
}

struct Cursor<'a, T: 'a> where T: IListNode {
    list: &'a mut IList<T>,
    prev: RawLink<T>,
}

impl<'a, T> Cursor<'a, T> where T: IListNode {

    #[inline]
    pub fn reset(&mut self) {
        self.prev = None;
    }

    pub fn next<'b>(&'b mut self) -> Option<&'b mut T> {
        unsafe {
            match self.prev {
                None => match self.list.head.as_mut() {
                    None => {
                        None
                    }
                    Some(head) => {
                        self.prev = mk_rawlink(&mut **head);
                        Some(&mut **head)
                    }
                },
                Some(prev) => match (**prev).next_mut() {
                    None => {
                        self.prev = None;
                        None
                    }
                    Some(next) => {
                        self.prev = mk_rawlink(&mut *next);
                        Some(next)
                    }
                }
            }
        }
    }

    pub fn prev<'b>(&'b mut self) -> Option<&'b mut T> {
        unsafe {
            match self.prev {
                None => {
                    self.prev = self.list.tail;
                    None
                }
                Some(prev) => {
                    self.prev = (**prev).prev_mut().map(|x| NonZero::new(x as *mut _));
                    Some(&mut **prev)
                }
            }
        }
    }

    pub fn peek_next<'b>(&'b mut self) -> Option<&'b mut T> {
        let Cursor { ref mut list, prev, ..} = *self;
        match prev {
            None => list.head_mut(),
            Some(prev) => unsafe {
                (**prev).next_mut()
            }
        }
    }

    pub fn peek_prev<'b>(&'b mut self) -> Option<&'b mut T> {
        unsafe {
            self.prev.map(|prev| &mut **prev)
        }
    }

    pub fn insert(&mut self, elem: Box<T>) {
        let Cursor {ref mut list, prev, ..} = *self;

        unsafe {
            match prev {
                None => list.push_back(elem),
                Some(node) => if (**node).next().is_none() {
                    list.push_back(elem);
                } else {
                    list.insert_after(&mut **node, elem);
                }
            }
        }
    }

    pub fn remove(&mut self) -> Option<Box<T>> {
        let Cursor {ref mut list, prev, ..} = *self;

        unsafe {
            match prev {
                // We're at the head, just pop_front
                None => list.pop_front(),
                Some(node) => if (**node).next().is_none() {
                    None
                } else {
                    let next = (**node).next_mut().unwrap();
                    Some(list.remove_node(next))
                }
            }
        }
    }

    pub unsafe fn position_at(&mut self, n: *mut T) {
        match (*n).prev_mut() {
            None => self.prev = None,
            Some(prev) => self.prev = mk_rawlink(prev)
        }
    }

    pub fn seek_forward(&mut self, by: usize) {
        for _ in (0..by) { self.next(); }
    }

    pub fn seek_backward(&mut self, by: usize) {
        for _ in (0..by) { self.prev(); }
    }
}

pub struct Iter<'a, T:'a> {
    nelem: usize,
    head: Option<&'a T>,
    tail: Option<&'a T>
}

impl<'a, T> Iterator for Iter<'a, T> where T: IListNode {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        if self.nelem == 0 {
            return None;
        }
        let cur_head = self.head;
        if let Some(head) = cur_head {
            self.nelem -= 1;
            self.head = head.next()
        }

        cur_head
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.nelem, Some(self.nelem))
    }
}

pub struct LinkPair<T> {
    pub next: Link<T>,
    pub prev: RawLink<T>
}

impl<T> LinkPair<T> {
    pub fn new() -> LinkPair<T> {
        LinkPair {
            next: None,
            prev: None,
        }
    }

    pub fn prev<'a>(&'a self) -> Option<&'a T> {
        unsafe {
            self.prev.as_ref().map(|x| &***x)
        }
    }
    pub fn next<'a>(&'a self) -> Option<&'a T> {
        self.next.as_ref().map(|x| &**x)
    }

    pub fn prev_mut<'a>(&'a mut self) -> Option<&'a mut T> {
        unsafe {
            self.prev.as_mut().map(|x| &mut ***x)
        }
    }
    pub fn next_mut<'a>(&'a mut self) -> Option<&'a mut T> {
        self.next.as_mut().map(|x| &mut **x)
    }

    pub fn set_next_link(&mut self, next: Link<T>) -> Link<T> {
        mem::replace(&mut self.next, next)
    }
    pub fn set_prev_link(&mut self, prev: RawLink<T>) -> RawLink<T> {
        mem::replace(&mut self.prev, prev)
    }
}

impl<T> fmt::Debug for LinkPair<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(f.write_str("LinkPair { next: "));
        match self.next() {
            None => try!(f.write_str("None")),
            Some(v) => try!(write!(f, "*{:p}", v)),
        }
        try!(f.write_str(", prev: "));
        match self.prev() {
            None => try!(f.write_str("None")),
            Some(v) => try!(write!(f, "*{:p}", v)),
        }

        f.write_str(" }")
    }
}

impl<T:fmt::Debug> fmt::Debug for IList<T> where T: IListNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(f.write_str("IList ["));

        let mut node = self.head();
        if let Some(val) = node {
            try!(write!(f, "{:?}", val));
            node = val.next();
        }
        while let Some(val) = node {
            try!(write!(f, ", {:?}", val));
            node = val.next();
        }

        f.write_str("]")
    }
}
