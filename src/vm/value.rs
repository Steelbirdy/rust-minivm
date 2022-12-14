use crate::{
    common::{AsBytes, Int},
    vm::Ptr,
};
use std::cmp::Ordering;
use std::fmt;

pub type Repr = u64;

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Value(Repr);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum ValueKind {
    Int = 0,
    Ptr = 1,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TaggedValue {
    Int(Int),
    Ptr(Ptr),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            ValueKind::Int => write!(f, "{}", self.as_int_unchecked()),
            ValueKind::Ptr => write!(f, "0x{:0x}", self.as_ptr_unchecked().to_usize()),
        }
    }
}

impl AsBytes for Value {
    fn to_bytes(self, buf: &mut [u8]) {
        Repr::to_bytes(self.0, buf);
    }
}

impl From<ValueKind> for u8 {
    fn from(kind: ValueKind) -> Self {
        kind as u8
    }
}

impl From<ValueKind> for u16 {
    fn from(kind: ValueKind) -> Self {
        kind as u16
    }
}

impl Value {
    pub const INT_MAX: Value = Value::int_unchecked(Int::MAX >> 1);
    pub const INT_MIN: Value = Value::int_unchecked(Int::MIN >> 1);

    pub const PTR_MAX: Value = Value::ptr_unchecked(Ptr::MAX);
    pub const PTR_MIN: Value = Value::ptr_unchecked(Ptr::MIN);

    #[must_use]
    #[inline]
    pub fn int(value: Int) -> Self {
        assert!(Self::INT_MIN.as_int_unchecked() <= value);
        assert!(value <= Self::INT_MAX.as_int_unchecked());
        Self::int_unchecked(value)
    }

    #[must_use]
    #[inline]
    pub fn ptr(ptr: Ptr) -> Self {
        assert!(Ptr::MIN.to_usize() <= ptr.to_usize());
        assert!(ptr.to_usize() <= Ptr::MAX.to_usize());
        Self::ptr_unchecked(ptr)
    }

    #[must_use]
    #[inline]
    pub const fn int_unchecked(value: Int) -> Self {
        let bytes = (value << 1).to_le_bytes();
        Self(Repr::from_le_bytes(bytes))
    }

    #[must_use]
    #[inline]
    pub const fn ptr_unchecked(ptr: Ptr) -> Self {
        let bytes = ((ptr.to_usize() << 1) | 1).to_le_bytes();
        Self(Repr::from_le_bytes(bytes))
    }

    #[cfg(feature = "unsafe")]
    #[must_use]
    #[inline]
    pub const fn kind(self) -> ValueKind {
        // SAFETY:
        // * The only possible values of (<int> & 1) are 0 and 1.
        // * Both 0u8 and 1u8 are valid ValueKinds.
        // * Hence the resulting value is a valid ValueKind.
        // qed
        unsafe { std::mem::transmute((self.0 & 1) as u8) }
    }

    #[cfg(not(feature = "unsafe"))]
    #[must_use]
    #[inline]
    pub const fn kind(self) -> ValueKind {
        debug_assert!(ValueKind::Int as u8 == 0 && ValueKind::Ptr as u8 == 1);

        match self.0 & 1 {
            0 => ValueKind::Int,
            1 => ValueKind::Ptr,
            _ => unreachable!(),
        }
    }

    #[must_use]
    #[inline]
    pub const fn is_int(self) -> bool {
        self.kind() as u8 == ValueKind::Int as u8
    }

    #[must_use]
    #[inline]
    pub const fn is_ptr(self) -> bool {
        self.kind() as u8 == ValueKind::Ptr as u8
    }

    #[must_use]
    pub const fn tagged(self) -> TaggedValue {
        if self.is_int() {
            TaggedValue::Int(self.as_int_unchecked())
        } else {
            TaggedValue::Ptr(self.as_ptr_unchecked())
        }
    }

    #[must_use]
    #[inline]
    pub fn as_int(self) -> Int {
        self.try_as_int().unwrap()
    }

    #[must_use]
    #[inline]
    pub fn as_ptr(self) -> Ptr {
        self.try_as_ptr().unwrap()
    }

    #[must_use]
    #[inline]
    pub fn try_as_int(self) -> Option<Int> {
        if self.is_int() {
            Some(self.as_int_unchecked())
        } else {
            None
        }
    }

    #[must_use]
    #[inline]
    pub fn try_as_ptr(self) -> Option<Ptr> {
        if self.is_ptr() {
            Some(self.as_ptr_unchecked())
        } else {
            None
        }
    }

    #[must_use]
    #[inline]
    pub const fn as_int_unchecked(self) -> Int {
        let val = Int::from_le_bytes(self.0.to_le_bytes());
        val >> 1
    }

    #[must_use]
    #[inline]
    pub const fn as_ptr_unchecked(self) -> Ptr {
        Ptr::from_le_bytes((self.0 >> 1).to_le_bytes())
    }

    #[must_use]
    #[inline]
    pub fn into_raw(self) -> Repr {
        self.0
    }
}

macro_rules! impl_op {
    ($($Trait:ident $func:ident;)+) => {
        $(
        impl std::ops::$Trait<Value> for Value {
            type Output = Value;

            #[inline]
            fn $func(self, rhs: Value) -> Value {
                Value::int_unchecked(std::ops::$Trait::$func(self.as_int_unchecked(), rhs.as_int_unchecked()))
            }
        }

        impl std::ops::$Trait<Int> for Value {
            type Output = Value;

            #[inline]
            fn $func(self, rhs: Int) -> Value {
                Value::int_unchecked(std::ops::$Trait::$func(self.as_int_unchecked(), rhs))
            }
        }

        impl std::ops::$Trait<Value> for Int {
            type Output = Value;

            #[inline]
            fn $func(self, rhs: Value) -> Value {
                Value::int_unchecked(std::ops::$Trait::$func(self, rhs.as_int_unchecked()))
            }
        }
        )+
    };
}

impl_op![
    Add add;
    Sub sub;
    Mul mul;
    Div div;
    Rem rem;
];

impl std::ops::AddAssign<Int> for Value {
    #[inline]
    fn add_assign(&mut self, rhs: Int) {
        *self = Self::int_unchecked(self.as_int_unchecked() + rhs);
    }
}

impl std::ops::SubAssign<Int> for Value {
    #[inline]
    fn sub_assign(&mut self, rhs: Int) {
        *self = Self::int_unchecked(self.as_int_unchecked() - rhs);
    }
}

impl std::ops::Neg for Value {
    type Output = Value;

    #[inline]
    fn neg(self) -> Self::Output {
        Self::int_unchecked(-self.as_int_unchecked())
    }
}

impl PartialEq<Value> for Value {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<Int> for Value {
    #[inline]
    fn eq(&self, other: &Int) -> bool {
        self.as_int_unchecked() == *other
    }
}

impl PartialEq<Value> for Int {
    #[inline]
    fn eq(&self, other: &Value) -> bool {
        *self == other.as_int_unchecked()
    }
}

impl PartialOrd<Value> for Value {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        Some(self.as_int_unchecked().cmp(&other.as_int_unchecked()))
    }
}

impl PartialOrd<Int> for Value {
    #[inline]
    fn partial_cmp(&self, other: &Int) -> Option<Ordering> {
        Some(self.as_int_unchecked().cmp(other))
    }
}

impl PartialOrd<Value> for Int {
    #[inline]
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        Some(self.cmp(&other.as_int_unchecked()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cmp_across_types() {
        let v1 = Value::int(10);
        let v2 = Value::ptr(Ptr::from_le_bytes([13, 0, 0, 0, 0, 0, 0, 0]));
        assert_eq!(v2.as_int_unchecked(), 13);

        assert!(v1 < v2);

        let v1 = -v1;
        assert!(v1 < v2);
        assert!(v2 > v1);

        assert_eq!(
            Value::PTR_MAX.as_int_unchecked(),
            Value::int_unchecked(Ptr::MAX.to_usize().try_into().unwrap())
        );
    }

    #[test]
    fn comparisons() {
        assert!(Value::int_unchecked(1) < 2);
    }
}
