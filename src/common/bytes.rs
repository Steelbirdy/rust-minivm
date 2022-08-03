use std::cell::Cell;
use std::mem::size_of;

#[cfg(feature = "unsafe")]
use std::mem::MaybeUninit;

pub trait FromBytes: Sized {
    fn from_bytes<R: Read>(reader: &R) -> Option<Self>;

    #[cfg(feature = "unsafe")]
    unsafe fn from_bytes_unchecked<R: Read>(reader: &R) -> Self;
}

pub trait ToBytes: Sized {
    fn to_bytes<W: Write>(self, writer: &mut W) -> Option<usize>;
}

impl<T, const N: usize> FromBytes for [T; N]
where
    T: FromBytes,
{
    #[cfg(feature = "unsafe")]
    fn from_bytes<R: Read>(reader: &R) -> Option<Self> {
        if reader.len() - reader.offset() < size_of::<T>() * N {
            return None;
        }
        // FIXME: Replace with `MaybeUninit::uninit_array` once it is stable
        let mut ret: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        let base_offset = reader.offset();
        for (i, init) in ret.iter_mut().enumerate() {
            let offset = base_offset + i * size_of::<T>();
            init.write(reader.read_at(offset)?);
        }
        Some(unsafe { ret.as_ptr().cast::<T>().cast::<[T; N]>().read() })
    }

    #[cfg(not(feature = "unsafe"))]
    fn from_bytes<R: Read>(reader: &R) -> Option<Self> {
        if reader.len() - reader.offset() < size_of::<T>() * N {
            return None;
        }

        let base_offset = reader.offset();
        let mut ret = Vec::with_capacity(N);
        for i in 0..N {
            let offset = base_offset + i * size_of::<T>();
            ret.push(reader.read_at(offset)?);
        }
        ret.try_into().ok()
    }

    #[cfg(feature = "unsafe")]
    unsafe fn from_bytes_unchecked<R: Read>(reader: &R) -> Self {
        // FIXME: Replace with `MaybeUninit::uninit_array` once it is stable
        let mut ret: [MaybeUninit<T>; N] = MaybeUninit::uninit().assume_init();
        let offset = reader.offset();
        for (i, init) in ret.iter_mut().enumerate() {
            let offset = offset + i * size_of::<T>();
            init.write(reader.read_at_unchecked(offset));
        }
        ret.as_ptr().cast::<T>().cast::<[T; N]>().read()
    }
}

macro_rules! int_bytes {
    ($($ty:ty)+) => {
        $(
        impl FromBytes for $ty {
            fn from_bytes<R: Read>(reader: &R) -> Option<Self> {
                reader.read_bytes().map(Self::from_le_bytes)
            }

            #[cfg(feature = "unsafe")]
            unsafe fn from_bytes_unchecked<R: Read>(reader: &R) -> Self {
                Self::from_le_bytes(reader.read_bytes_unchecked())
            }
        }

        impl ToBytes for $ty {
            fn to_bytes<W: Write>(self, writer: &mut W) -> Option<usize> {
                writer.write_bytes(self.to_le_bytes())
            }
        }
        )+
    };
}

int_bytes![
    i8 i16 i32 i64 i128 isize
    u8 u16 u32 u64 u128 usize
];

#[allow(clippy::missing_safety_doc)]
pub trait Read: Sized {
    fn offset(&self) -> usize;

    fn set_offset(&self, new_offset: usize);

    fn reset(&self) {
        self.set_offset(0);
    }

    fn len(&self) -> usize;

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn is_at_end(&self) -> bool {
        self.offset() >= self.len()
    }

    fn read<T: FromBytes>(&self) -> Option<T> {
        T::from_bytes(self)
    }

    fn read_at<T: FromBytes>(&self, offset: usize) -> Option<T> {
        let prev_offset = self.offset();
        self.set_offset(offset);
        let ret = self.read();
        self.set_offset(prev_offset);
        ret
    }

    fn take<T: FromBytes>(&self) -> Option<T> {
        let ret = self.read()?;
        self.skip::<T>();
        Some(ret)
    }

    fn skip<T: FromBytes>(&self) {
        self.set_offset(self.offset() + size_of::<T>());
    }

    fn skipn<T: FromBytes>(&self, count: usize) {
        self.set_offset(self.offset() + count * size_of::<T>());
    }

    fn read_byte(&self) -> Option<u8>;

    fn read_bytes<const N: usize>(&self) -> Option<[u8; N]>;

    fn read_byte_slice(&self, len: usize) -> Option<&[u8]>;

    #[cfg(feature = "unsafe")]
    unsafe fn read_unchecked<T: FromBytes>(&self) -> T {
        T::from_bytes_unchecked(self)
    }

    /// # Safety
    /// See [`Read::read_unchecked`]
    #[cfg(feature = "unsafe")]
    unsafe fn read_at_unchecked<T: FromBytes>(&self, offset: usize) -> T {
        let prev_offset = self.offset();
        self.set_offset(offset);
        let ret = self.read_unchecked();
        self.set_offset(prev_offset);
        ret
    }

    /// # Safety
    /// See [`Read::read_unchecked`]
    #[cfg(feature = "unsafe")]
    unsafe fn take_unchecked<T: FromBytes>(&self) -> T {
        let ret = self.read_unchecked();
        self.skip::<T>();
        ret
    }

    #[cfg(feature = "unsafe")]
    unsafe fn read_byte_unchecked(&self) -> u8;

    #[cfg(feature = "unsafe")]
    unsafe fn read_bytes_unchecked<const N: usize>(&self) -> [u8; N];

    #[cfg(feature = "unsafe")]
    unsafe fn read_byte_slice_unchecked(&self, len: usize) -> &[u8];
}

pub trait Write: Sized {
    fn write<T: ToBytes>(&mut self, value: T) -> Option<usize> {
        T::to_bytes(value, self)
    }

    fn write_byte(&mut self, byte: u8) -> Option<usize>;

    fn write_bytes<const N: usize>(&mut self, bytes: [u8; N]) -> Option<usize>;

    fn write_byte_slice(&mut self, bytes: &[u8]) -> Option<usize>;
}

pub trait Push {
    fn push<T: ToBytes>(&mut self, value: T);
}

impl<T: Read> Read for &T {
    fn offset(&self) -> usize {
        T::offset(self)
    }

    fn set_offset(&self, new_offset: usize) {
        T::set_offset(self, new_offset);
    }

    fn len(&self) -> usize {
        T::len(self)
    }

    fn read_byte(&self) -> Option<u8> {
        T::read_byte(self)
    }

    fn read_bytes<const N: usize>(&self) -> Option<[u8; N]> {
        T::read_bytes(self)
    }

    fn read_byte_slice(&self, len: usize) -> Option<&[u8]> {
        T::read_byte_slice(self, len)
    }

    #[cfg(feature = "unsafe")]
    unsafe fn read_byte_unchecked(&self) -> u8 {
        T::read_byte_unchecked(self)
    }

    #[cfg(feature = "unsafe")]
    unsafe fn read_bytes_unchecked<const N: usize>(&self) -> [u8; N] {
        T::read_bytes_unchecked(self)
    }

    #[cfg(feature = "unsafe")]
    unsafe fn read_byte_slice_unchecked(&self, len: usize) -> &[u8] {
        T::read_byte_slice_unchecked(self, len)
    }
}

impl<T: Write> Write for &mut T {
    fn write_byte(&mut self, byte: u8) -> Option<usize> {
        T::write_byte(self, byte)
    }

    fn write_bytes<const N: usize>(&mut self, bytes: [u8; N]) -> Option<usize> {
        T::write_bytes(self, bytes)
    }

    fn write_byte_slice(&mut self, bytes: &[u8]) -> Option<usize> {
        T::write_byte_slice(self, bytes)
    }
}

impl<P: Push> Push for &mut P {
    fn push<T: ToBytes>(&mut self, value: T) {
        P::push(self, value);
    }
}

#[derive(Default)]
pub struct ByteBuffer<T> {
    code: T,
    cursor: Cell<usize>,
}

pub type ByteReader<'a> = ByteBuffer<&'a [u8]>;
pub type ByteEditor<'a> = ByteBuffer<&'a mut [u8]>;
pub type ByteWriter = ByteBuffer<Vec<u8>>;
pub type OwnedByteBuffer = ByteBuffer<Box<[u8]>>;

impl<B: AsRef<[u8]>> Read for ByteBuffer<B> {
    fn offset(&self) -> usize {
        self.cursor.get()
    }

    fn set_offset(&self, new_offset: usize) {
        self.cursor.set(new_offset);
    }

    fn len(&self) -> usize {
        self.code.as_ref().len()
    }

    fn read_byte(&self) -> Option<u8> {
        let code = self.code.as_ref();
        code.get(self.cursor.get()).copied()
    }

    #[cfg(feature = "unsafe")]
    fn read_bytes<const N: usize>(&self) -> Option<[u8; N]> {
        let code = self.code.as_ref();
        let offset = self.cursor.get();

        if offset + N > code.len() {
            return None;
        }
        let mut ret = [0; N];
        unsafe {
            let ptr = code.as_ptr().add(offset);
            std::ptr::copy_nonoverlapping(ptr, ret.as_mut_ptr(), N);
        }
        Some(ret)
    }

    #[cfg(not(feature = "unsafe"))]
    fn read_bytes<const N: usize>(&self) -> Option<[u8; N]> {
        let code = self.code.as_ref();
        let offset = self.cursor.get();

        let slice = code.get(offset..offset + N)?;
        slice.try_into().ok()
    }

    fn read_byte_slice(&self, len: usize) -> Option<&[u8]> {
        let code = self.code.as_ref();
        let offset = self.cursor.get();
        code.get(offset..offset + len)
    }

    /// # Safety
    /// The buffer must have at least one byte left to read.
    #[cfg(feature = "unsafe")]
    unsafe fn read_byte_unchecked(&self) -> u8 {
        let code = self.code.as_ref();
        let offset = self.cursor.get();
        *code.get_unchecked(offset)
    }

    /// # Safety
    /// The buffer must have at least `N` bytes left to read.
    #[cfg(feature = "unsafe")]
    unsafe fn read_bytes_unchecked<const N: usize>(&self) -> [u8; N] {
        let code = self.code.as_ref();
        let offset = self.cursor.get();
        let mut ret = [0; N];
        let ptr = code.as_ptr().add(offset);
        std::ptr::copy_nonoverlapping(ptr, ret.as_mut_ptr(), N);
        ret
    }

    /// # Safety
    /// The buffer must have at least `len` bytes left to read.
    #[cfg(feature = "unsafe")]
    unsafe fn read_byte_slice_unchecked(&self, len: usize) -> &[u8] {
        let code = self.code.as_ref();
        let offset = self.cursor.get();
        &code[offset..offset + len]
    }
}

impl<B: AsRef<[u8]> + AsMut<[u8]>> Write for ByteBuffer<B> {
    fn write_byte(&mut self, byte: u8) -> Option<usize> {
        if self.is_at_end() {
            return None;
        }
        let offset = self.offset();
        self.code.as_mut()[offset] = byte;
        self.advance(1);
        Some(1)
    }

    #[cfg(feature = "unsafe")]
    fn write_bytes<const N: usize>(&mut self, bytes: [u8; N]) -> Option<usize> {
        let offset = self.offset();
        if offset + N > self.len() {
            return None;
        }
        unsafe {
            let ptr = self.code.as_mut().as_mut_ptr().add(offset);
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, N);
        }
        Some(N)
    }

    #[cfg(not(feature = "unsafe"))]
    fn write_bytes<const N: usize>(&mut self, bytes: [u8; N]) -> Option<usize> {
        let offset = self.offset();
        let code = self.code.as_mut();
        let slice = code.get_mut(offset..offset + N)?;
        for (to, from) in slice.iter_mut().zip(bytes) {
            *to = from;
        }
        Some(N)
    }

    #[cfg(feature = "unsafe")]
    fn write_byte_slice(&mut self, bytes: &[u8]) -> Option<usize> {
        let offset = self.offset();
        if offset + bytes.len() > self.len() {
            return None;
        }
        unsafe {
            let ptr = self.code.as_mut().as_mut_ptr().add(offset);
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, bytes.len());
        }
        Some(bytes.len())
    }

    #[cfg(not(feature = "unsafe"))]
    fn write_byte_slice(&mut self, bytes: &[u8]) -> Option<usize> {
        let offset = self.offset();
        let code = self.code.as_mut();
        let slice = code.get_mut(offset..offset + bytes.len())?;
        for (to, &from) in slice.iter_mut().zip(bytes) {
            *to = from;
        }
        Some(bytes.len())
    }
}

impl<B: AsRef<[u8]>> ByteBuffer<B> {
    pub fn new(code: B) -> Self {
        Self {
            code,
            cursor: Cell::new(0),
        }
    }

    pub fn advance(&self, count: usize) {
        self.cursor.set(self.cursor.get() + count);
    }

    pub fn at_checkpoint<T>(&self, f: impl FnOnce() -> T) -> T {
        self.at_offset(self.offset(), f)
    }

    pub fn at_start<T>(&self, f: impl FnOnce() -> T) -> T {
        self.at_offset(0, f)
    }

    pub fn at_offset<T>(&self, new_offset: usize, f: impl FnOnce() -> T) -> T {
        let prev_offset = self.offset();
        self.set_offset(new_offset);
        let ret = f();
        self.set_offset(prev_offset);
        ret
    }
}

impl ByteBuffer<Vec<u8>> {
    pub fn into_inner(self) -> Box<[u8]> {
        self.code.into_boxed_slice()
    }

    pub fn into_boxed_buffer(self) -> ByteBuffer<Box<[u8]>> {
        ByteBuffer {
            code: self.code.into_boxed_slice(),
            cursor: self.cursor,
        }
    }
}

impl Push for ByteBuffer<Vec<u8>> {
    fn push<T: ToBytes>(&mut self, value: T) {
        assert!(
            self.is_at_end(),
            "offset={}, end={}",
            self.offset(),
            self.len()
        );
        self.code.extend(std::iter::repeat(0).take(size_of::<T>()));
        self.write(value);
        self.advance(size_of::<T>());
    }
}

impl<T: AsRef<[u8]>> ByteBuffer<T> {
    pub fn as_ref(&self) -> ByteBuffer<&[u8]> {
        ByteBuffer {
            code: self.code.as_ref(),
            cursor: Cell::clone(&self.cursor),
        }
    }
}

impl<T: AsMut<[u8]>> ByteBuffer<T> {
    pub fn as_mut(&mut self) -> ByteBuffer<&mut [u8]> {
        ByteBuffer {
            code: self.code.as_mut(),
            cursor: Cell::clone(&self.cursor),
        }
    }
}
