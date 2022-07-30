use std::mem::MaybeUninit;

pub struct Stack<T, const CAP: usize> {
    arr: [MaybeUninit<T>; CAP],
    len: usize,
}

impl<T, const CAP: usize> Stack<T, CAP> {
    pub const fn new() -> Self {
        let arr: [MaybeUninit<T>; CAP] = unsafe { MaybeUninit::uninit().assume_init() };
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
