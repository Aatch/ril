#![crate_type="lib"]

#![feature(box_syntax,unsafe_destructor)]
#![allow(dead_code)]
#![allow(unstable)]

extern crate arena;

use ir::*;
use ir::insts::*;

pub mod dlist;
pub mod uniq_map;
pub mod ir;
