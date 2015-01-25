use context::Context;
use ir::{self, Constant};

pub fn add(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Int(l, sz), &Int(r, _)) => Some(Constant::int(cx, l+r, sz)),
            (&Uint(l, sz), &Uint(r, _)) => Some(Constant::uint(cx, l+r, sz)),
            _ => None
        }
    })
}

pub fn sub(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Int(l, sz), &Int(r, _)) => Some(Constant::int(cx, l-r, sz)),
            (&Uint(l, sz), &Uint(r, _)) => Some(Constant::uint(cx, l-r, sz)),
            _ => None
        }
    })
}

pub fn mul(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        // 0 * X == 0
        if l.is_zero() || r.is_zero() {
            return Some(Constant::zero(cx, l.get_type()));
        }

        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Int(l, sz), &Int(r, _)) => Some(Constant::int(cx, l*r, sz)),
            (&Uint(l, sz), &Uint(r, _)) => Some(Constant::uint(cx, l*r, sz)),
            _ => None
        }
    })
}

pub fn div(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        // 0 / X == 0
        if l.is_zero() {
            return Some(Constant::zero(cx, l.get_type()));
        }
        // X / 0 == undef
        if r.is_zero() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Int(lv, sz), &Int(rv, _)) => {
                Some(Constant::int(cx, lv/rv, sz))
            }
            (&Uint(lv, sz), &Uint(rv, _)) => {
                Some(Constant::uint(cx, lv/rv, sz))
            }
            _ => None
        }
    })
}

pub fn rem(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        // 0 % X == 0
        if l.is_zero() {
            return Some(Constant::zero(cx, l.get_type()));
        }
        // X % 0 == undef
        if r.is_zero() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Int(lv, sz), &Int(rv, _)) => {
                Some(Constant::int(cx, lv/rv, sz))
            }
            (&Uint(lv, sz), &Uint(rv, _)) => {
                Some(Constant::uint(cx, lv/rv, sz))
            }
            _ => None
        }
    })
}

pub fn and(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        if l.is_zero() || r.is_zero() {
            return Some(Constant::zero(cx, l.get_type()));
        }

        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Bool(l), &Bool(r)) => {
                Some(Constant::bool(cx, l & r))
            }
            (&Int(l, sz), &Int(r, _)) => {
                Some(Constant::int(cx, l & r, sz))
            }
            (&Uint(l, sz), &Uint(r, _)) => {
                Some(Constant::uint(cx, l & r, sz))
            }
            _ => None
        }
    })
}

pub fn or(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        if l.is_all_ones() || r.is_all_ones() {
            return Some(Constant::all_ones(cx, l.get_type()));
        }

        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Bool(l), &Bool(r)) => {
                Some(Constant::bool(cx, l | r))
            }
            (&Int(l, sz), &Int(r, _)) => {
                Some(Constant::int(cx, l | r, sz))
            }
            (&Uint(l, sz), &Uint(r, _)) => {
                Some(Constant::uint(cx, l | r, sz))
            }
            _ => None
        }
    })
}

pub fn xor(cx: &Context, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        match (&l.data, &r.data) {
            (&Bool(l), &Bool(r)) => {
                Some(Constant::bool(cx, l ^ r))
            }
            (&Int(l, sz), &Int(r, _)) => {
                Some(Constant::int(cx, l ^ r, sz))
            }
            (&Uint(l, sz), &Uint(r, _)) => {
                Some(Constant::uint(cx, l ^ r, sz))
            }
            _ => None
        }
    })
}

pub fn cmp(cx: &Context, cmp: ir::Cmp, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
    use ir::ConstData::*;

    fold_binop(lhs, rhs, |l, r| {
        if l.is_undef() || r.is_undef() {
            return Some(Constant::undef(cx, l.get_type()));
        }

        let res = match (&l.data, &r.data) {
            (&Bool(l), &Bool(r)) => {
                match cmp {
                    ir::Cmp::Eq => l == r,
                    ir::Cmp::Ne => l != r,
                    ir::Cmp::Lt => !l && r,
                    ir::Cmp::Le => l == r || r,
                    ir::Cmp::Gt => l && !r,
                    ir::Cmp::Ge => l == r || l
                }
            }
            (&Int(l, _), &Int(r, _)) => {
                match cmp {
                    ir::Cmp::Eq => l == r,
                    ir::Cmp::Ne => l != r,
                    ir::Cmp::Lt => l < r,
                    ir::Cmp::Le => l <= r,
                    ir::Cmp::Gt => l > r,
                    ir::Cmp::Ge => l >= r,
                }
            }
            (&Uint(l, _), &Uint(r, _)) => {
                match cmp {
                    ir::Cmp::Eq => l == r,
                    ir::Cmp::Ne => l != r,
                    ir::Cmp::Lt => l < r,
                    ir::Cmp::Le => l <= r,
                    ir::Cmp::Gt => l > r,
                    ir::Cmp::Ge => l >= r,
                }
            }
            _ => return None
        };

        Some(Constant::bool(cx, res))
    })
}


fn fold_binop<F>(lhs: ir::Value, rhs: ir::Value, f: F) -> ir::Value
where F: Fn(&Constant, &Constant) -> Option<*mut Constant> {

    match (lhs.as_const(), rhs.as_const()) {
        (Some(l), Some(r)) => {

            let ty = lhs.get_type();

            if ty != rhs.get_type() { return ir::Value::none(); }

            let c = match f(l, r) {
                Some(c) => c,
                None => return ir::Value::none()
            };

            if c.is_null() { return ir::Value::none(); }

            return ir::Value::constant(c);
        }
        _ => return ir::Value::none()
    }
}
