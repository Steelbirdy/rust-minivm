#[cfg(feature = "unsafe")]
use std::mem;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Key(lasso::Spur);

#[cfg(feature = "unsafe")]
impl Key {
    /// # Safety
    /// `raw` must not be 0.
    const unsafe fn from_raw_unchecked(raw: u32) -> Self {
        // Spur is repr(transparent) with NonZeroU32.
        // As long as the above condition holds, this is safe.
        Self(mem::transmute(raw))
    }

    pub const fn entry_point() -> Self {
        unsafe { Self::from_raw_unchecked(1) }
    }
}

#[cfg(not(feature = "unsafe"))]
impl Key {
    fn from_raw(raw: usize) -> Option<Self> {
        lasso::Key::try_from_usize(raw).map(Key)
    }

    pub fn entry_point() -> Self {
        Self::from_raw(1).unwrap()
    }
}

pub struct Interner {
    inner: lasso::Rodeo,
}

impl Interner {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn intern(&mut self, s: &str) -> Key {
        Key(self.inner.get_or_intern(s))
    }

    #[must_use]
    pub fn lookup(&self, key: Key) -> &str {
        self.inner.resolve(&key.0)
    }
}

impl Default for Interner {
    fn default() -> Self {
        let mut ret = Self {
            inner: lasso::Rodeo::default(),
        };
        let _ = ret.intern(crate::common::ENTRY_POINT_LBL);
        ret
    }
}
