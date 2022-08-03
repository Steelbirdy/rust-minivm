use std::mem;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Key(lasso::Spur);

impl Key {
    pub fn from_raw(raw: u32) -> Self {
        assert_ne!(raw, 0, "key cannot be 0");
        // SAFETY: `raw != 0`
        unsafe { Self::from_raw_unchecked(raw) }
    }

    /// # Safety
    /// `raw` must not be 0.
    pub const unsafe fn from_raw_unchecked(raw: u32) -> Self {
        // Spur is repr(transparent) with NonZeroU32.
        // As long as the above condition holds, this is safe.
        Self(mem::transmute(raw))
    }

    pub const fn to_raw(self) -> u32 {
        // SAFETY: Spur is repr(transparent) with NonZeroU32.
        // Every possible bit pattern of NonZeroU32 is also a valid
        // bit pattern for u32.
        unsafe { mem::transmute(self.0) }
    }

    pub const fn entry_point() -> Self {
        unsafe { Self::from_raw_unchecked(1) }
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
