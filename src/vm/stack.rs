#[cfg(not(feature = "unsafe"))]
pub type Stack<T, const CAP: usize> = smallvec::SmallVec<[T; CAP]>;

#[cfg(feature = "unsafe")]
pub struct Stack<T, const CAP: usize> {
    arr: [std::mem::MaybeUninit<T>; CAP],
    len: usize,
}

#[cfg(feature = "unsafe")]
impl<T, const CAP: usize> Stack<T, CAP> {
    pub const fn new() -> Self {
        let arr: [std::mem::MaybeUninit<T>; CAP] = unsafe { std::mem::MaybeUninit::uninit().assume_init() };
        Self { arr, len: 0 }
    }

    pub fn push(&mut self, value: T) {
        self.arr[self.len].write(value);
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        Some(unsafe { self.arr[self.len].assume_init_read() })
    }
}
