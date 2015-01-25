//! Pattern Matching functionality for instructions.
//!
//! Use the vmatch! macro to specify the pattern you want to match against.
//!
//! For example:
//!
//!     vmatch!(val = (add 0 _) || (add _ 0))
//!
//! Will match an add instruction that has either argument as 0.
//!
//! You can also bind posisitons to variables:
//!
//!     vmatch!(val = add x 0)
//!
//! This matches an add instruction with a 0 right-hand side and binds the value
//! of the left-hand-side to x. The variable 'x' must already be in scope and initialised.
//! If the pattern doesn't match, then the value is unspecified (depending on the pattern, a
//! value may get bound, but the match fail overall)
//!
//! Finally, you can match against specific values:
//!
//!    vmatch!(val = sub 0 (match:op))
//!
//! This matches a sub instruction with a 0 left-hand side and where the left-hand side is the same
//! value as `op`. This does variable equality (%x == %x) and constant equality (5 == 5)
//!
//! The test value and `match` patterns accept any type that implements `ToValue` allowing `Use`s to
//! be passed directly.

use ir::{self,ToValue};
use ty;

#[macro_escape]
macro_rules! vmatch {
    ($val:ident = $($pat:tt)*) => {
        ::pattern_match::do_match(&$val, _pat!($($pat)+))
    }
}

macro_rules! _pat {
    // Forwarding case that unwraps the parens
    (($($pat:tt)+)) => { _pat!($($pat)+) };

    // Combinator Patterns
    ($lhs:tt || $rhs:tt) => { ::pattern_match::match_or(_pat!($lhs), _pat!($rhs)) };
    ($lhs:tt && $rhs:tt) => { ::pattern_match::match_and(_pat!($lhs), _pat!($rhs)) };

    // Match against a constant
    (const : $pat:tt) => {
        ::pattern_match::constant(_pat!($pat))
    };

    // Type Matchers
    (uint:$pat:tt) => { ::pattern_match::uint(_pat!($pat)) };
    (int:$pat:tt)  => { ::pattern_match::int(_pat!($pat)) };
    (bool:$pat:tt) => { ::pattern_match::bool(_pat!($pat)) };

    // Bind to a value
    (val : $id:ident) => { ::pattern_match::value(&mut $id) };
    // Match a specific value
    (match : $val:expr) => { ::pattern_match::specific(&($val)) };

    // Instruction Matchers

    (add $lhs:tt $rhs:tt) => {
        <::pattern_match::Add<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    (sub $lhs:tt $rhs:tt) => {
        <::pattern_match::Sub<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    (mul $lhs:tt $rhs:tt) => {
        <::pattern_match::Mul<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    (div $lhs:tt $rhs:tt) => {
        <::pattern_match::Div<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    (rem $lhs:tt $rhs:tt) => {
        <::pattern_match::Rem<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    (and $lhs:tt $rhs:tt) => {
        <::pattern_match::And<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    (or $lhs:tt $rhs:tt) => {
        <::pattern_match::Or<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    (xor $lhs:tt $rhs:tt) => {
        <::pattern_match::Xor<_,_> as ::pattern_match::Binop<_,_>>::new(_pat!($lhs), _pat!($rhs))
    };

    // Shortcuts for common matches
    (true) => { ::pattern_match::one() };
    (false) => { ::pattern_match::zero() };
    (1) => { ::pattern_match::one() };
    (0) => { ::pattern_match::zero() };
    (_) => { ::pattern_match::ignore() };
    ($id:ident) => { ::pattern_match::value(&mut $id) };
}

#[inline]
pub fn do_match<V:ToValue,P:Pattern>(v: &V, pat:P) -> bool {
    pat.do_match(&v.to_value())
}

#[inline]
pub fn match_or<P1:Pattern, P2:Pattern>(p1: P1, p2: P2) -> MatchOrPattern<P1,P2> {
    MatchOrPattern {
        p1: p1,
        p2: p2
    }
}

#[inline]
pub fn match_and<P1:Pattern, P2:Pattern>(p1: P1, p2: P2) -> MatchAndPattern<P1,P2> {
    MatchAndPattern {
        p1: p1,
        p2: p2
    }
}

#[inline(always)]
pub fn ignore() -> IgnorePattern {
    IgnorePattern
}

#[inline]
pub fn value<'a>(v: &'a mut ir::Value) -> BindPattern<'a> {
    BindPattern { var: v }
}

#[inline]
pub fn specific<'a, V:ToValue>(v: &V) -> SpecificPattern {
    SpecificPattern { var: v.to_value() }
}

#[inline]
pub fn constant<P:Pattern>(p: P) -> ConstantPattern<P> {
    ConstantPattern { p: p }
}

#[inline]
pub fn uint<P:Pattern>(p: P) -> UintPattern<P> {
    UintPattern { p: p }
}
#[inline]
pub fn int<P:Pattern>(p: P) -> IntPattern<P> {
    IntPattern { p: p }
}
#[inline]
pub fn bool<P:Pattern>(p: P) -> BoolPattern<P> {
    BoolPattern { p: p }
}

#[inline(always)]
pub fn one() -> OnePattern { OnePattern }

#[inline(always)]
pub fn zero() -> ZeroPattern { ZeroPattern }

pub trait Pattern {
    fn do_match(self, &ir::Value) -> bool;
}

pub struct MatchOrPattern<P1, P2> {
    p1: P1,
    p2: P2,
}
impl<P1:Pattern, P2:Pattern> Pattern for MatchOrPattern<P1,P2> {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        let MatchOrPattern { p1, p2 } = self;
        p1.do_match(v) || p2.do_match(v)
    }
}

pub struct MatchAndPattern<P1, P2> {
    p1: P1,
    p2: P2,
}
impl<P1:Pattern, P2:Pattern> Pattern for MatchAndPattern<P1,P2> {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        let MatchAndPattern { p1, p2 } = self;
        p1.do_match(v) && p2.do_match(v)
    }
}

pub struct IgnorePattern;
impl Pattern for IgnorePattern {
    #[inline(always)]
    fn do_match(self, _: &ir::Value) -> bool { true }
}

pub struct BindPattern<'a> {
    var: &'a mut ir::Value
}
impl<'a> Pattern for BindPattern<'a> {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        *self.var = v.clone();
        true
    }
}

pub struct SpecificPattern {
    var: ir::Value
}
impl Pattern for SpecificPattern {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        &self.var == v
    }
}

pub struct ConstantPattern<P:Pattern> {
    p: P
}
impl<P:Pattern> Pattern for ConstantPattern<P> {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        v.is_const() && self.p.do_match(v)
    }
}

pub struct UintPattern<P:Pattern> {
    p: P
}
impl<P:Pattern> Pattern for UintPattern<P> {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        match &*v.get_type() {
            &ty::Ty::Uint(..) => self.p.do_match(v),
            _ => false
        }
    }
}

pub struct IntPattern<P:Pattern> {
    p: P
}
impl<P:Pattern> Pattern for IntPattern<P> {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        match &*v.get_type() {
            &ty::Ty::Int(..) => self.p.do_match(v),
            _ => false
        }
    }
}

pub struct BoolPattern<P:Pattern> {
    p: P
}
impl<P:Pattern> Pattern for BoolPattern<P> {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        match &*v.get_type() {
            &ty::Ty::Bool(..) => self.p.do_match(v),
            _ => false
        }
    }
}

pub struct OnePattern;
impl Pattern for OnePattern {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        if let Some(c) = v.as_const() {
            c.is_one()
        } else {
            false
        }
    }
}

pub struct ZeroPattern;
impl Pattern for ZeroPattern {
    #[inline(always)]
    fn do_match(self, v: &ir::Value) -> bool {
        if let Some(c) = v.as_const() {
            c.is_zero()
        } else {
            false
        }
    }
}

pub trait Binop<P1:Pattern,P2:Pattern> : Pattern {
    fn new(p1: P1, p2: P2) -> Self;
}

macro_rules! binop_pattern {
    ($op:ident) => {
        pub struct $op<P1,P2> {
            p1: P1,
            p2: P2,
        }

        impl<P1:Pattern,P2:Pattern> Pattern for $op<P1,P2> {
            #[inline(always)]
            fn do_match(self, v: &ir::Value) -> bool {
                if let Some(inst) = v.as_inst() {
                    if let ir::Op::$op(ref lhs, ref rhs) = inst.op {
                        let $op { p1, p2 } = self;

                        if let Some(lhs) = lhs.get_use() {
                            if p1.do_match(&lhs.producer()) {
                                if let Some(rhs) = rhs.get_use() {
                                    return p2.do_match(&rhs.producer());
                                }
                            }
                        }
                    }
                }
                false
            }
        }

        impl<P1:Pattern, P2:Pattern> Binop<P1, P2> for $op<P1,P2> {
            fn new(p1: P1, p2: P2) -> $op<P1,P2> {
                $op { p1: p1, p2: p2 }
            }
        }
    }
}

binop_pattern!(Add);
binop_pattern!(Sub);
binop_pattern!(Mul);
binop_pattern!(Div);
binop_pattern!(Rem);
binop_pattern!(And);
binop_pattern!(Or);
binop_pattern!(Xor);
