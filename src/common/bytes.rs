use std::mem::size_of;

pub trait AsBytes: Copy {
    fn to_bytes(self, buf: &mut [u8]);
}

impl<T, const N: usize> AsBytes for [T; N]
where
    T: AsBytes,
{
    fn to_bytes(self, buf: &mut [u8]) {
        let size = size_of::<T>();
        for (i, item) in self.into_iter().enumerate() {
            item.to_bytes(&mut buf[i * size..]);
        }
    }
}

macro_rules! impl_bytes {
    ($($ty:ty)+) => {
        $(
        impl AsBytes for $ty {
            #[allow(unsafe_code)]
            fn to_bytes(self, buf: &mut [u8]) {
                let bytes = self.to_ne_bytes();
                unsafe {
                    std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf.as_mut_ptr(), bytes.len());
                }
            }
        }
        )+
    };
}

impl_bytes![
    i8 i16 i32 i64 i128 isize
    u8 u16 u32 u64 u128 usize
];

#[derive(Copy, Clone)]
pub struct BytecodeReader<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> BytecodeReader<'a> {
    #[must_use]
    pub fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    #[inline]
    #[must_use]
    pub fn offset(&self) -> usize {
        self.offset
    }

    #[inline]
    pub fn set_offset(&mut self, offset: usize) {
        self.offset = offset;
    }

    #[must_use]
    pub fn is_at_end(&self) -> bool {
        self.offset >= self.bytes.len()
    }

    pub fn skip<T: AsBytes>(&mut self) {
        self.offset += size_of::<T>();
    }

    pub fn skipn<T: AsBytes>(&mut self, n: usize) {
        self.offset += size_of::<T>() * n;
    }

    #[allow(unsafe_code)]
    #[inline]
    #[must_use]
    pub fn read_at<T: AsBytes>(&self, offset: usize) -> T {
        #[cfg(not(feature = "unsafe"))]
        assert!(offset + size_of::<T>() <= self.bytes.len());
        unsafe { *self.bytes.as_ptr().add(offset).cast::<T>() }
    }

    #[inline]
    #[must_use]
    pub fn read<T: AsBytes>(&self) -> T {
        self.read_at(self.offset)
    }

    #[inline]
    #[must_use]
    pub fn take<T: AsBytes>(&mut self) -> T {
        let ret = self.read::<T>();
        self.offset += size_of::<T>();
        ret
    }

    pub fn at_offset<T>(&mut self, offset: usize, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.offset;
        self.offset = offset;
        let ret = f(self);
        self.offset = prev;
        ret
    }

    pub fn at_checkpoint<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.at_offset(self.offset, f)
    }
}

#[derive(Default)]
pub struct BytecodeBuilder {
    buf: Vec<u8>,
}

impl BytecodeBuilder {
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn reader(&self) -> BytecodeReader<'_> {
        BytecodeReader::new(&self.buf)
    }

    pub fn push<T: AsBytes>(&mut self, value: T) {
        let bytes = size_of::<T>();
        let len = self.buf.len();
        self.buf.resize(len + bytes, 0);
        T::to_bytes(value, &mut self.buf[len..]);
    }

    pub fn write_at<T: AsBytes>(&mut self, index: usize, value: T) {
        let bytes = size_of::<T>();
        let len = self.buf.len();
        if index + bytes > len {
            self.buf.resize(index + bytes, 0);
        }
        T::to_bytes(value, &mut self.buf[index..index + bytes]);
    }

    pub fn into_inner(self) -> Box<[u8]> {
        self.buf.into_boxed_slice()
    }
}
