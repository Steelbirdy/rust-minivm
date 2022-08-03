use crate::{
    common::{config::NUM_REGISTERS, Reg},
    vm::{
        value::{Value, ValueKind},
        Len,
    },
};
use std::ops::{Index, IndexMut};

const GC_MIN_THRESHOLD: usize = 1 << 12;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
pub struct Ptr(usize);

impl Ptr {
    pub const MIN: Self = Ptr(usize::MIN >> 1);
    pub const MAX: Self = Ptr(usize::MAX >> 1);

    pub const fn to_usize(self) -> usize {
        self.0
    }

    pub(in crate::vm) const fn from_le_bytes(bytes: [u8; std::mem::size_of::<Ptr>()]) -> Self {
        Self(usize::from_le_bytes(bytes))
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
struct GcHeader {
    mark: u16,
    _kind: u16,
    size: Len,
}

impl GcHeader {
    fn is_marked(self) -> bool {
        self.mark == 1
    }

    fn set_marked(&mut self, marked: bool) {
        self.mark = marked.into();
    }

    #[cfg(debug_assertions)]
    fn len(self) -> usize {
        usize::try_from(self.size).unwrap()
    }

    #[cfg(not(debug_assertions))]
    fn len(self) -> usize {
        self.size as usize
    }
}

#[derive(Copy, Clone)]
union RawGcData {
    value: Value,
    header: GcHeader,
}

#[derive(Copy, Clone)]
#[repr(transparent)]
struct GcData(RawGcData);

#[allow(unsafe_code)]
impl GcData {
    pub fn new_header(marked: bool, kind: ValueKind, len: Len) -> Self {
        let header = GcHeader {
            mark: marked.into(),
            _kind: kind.into(),
            size: len,
        };
        Self(RawGcData { header })
    }

    pub fn header(self) -> GcHeader {
        unsafe { self.0.header }
    }

    pub fn header_mut(&mut self) -> &mut GcHeader {
        unsafe { &mut self.0.header }
    }

    pub fn value(self) -> Value {
        unsafe { self.0.value }
    }

    pub fn value_mut(&mut self) -> &mut Value {
        unsafe { &mut self.0.value }
    }
}

impl Default for GcData {
    fn default() -> Self {
        Self(RawGcData {
            header: GcHeader {
                mark: 0,
                _kind: 0,
                size: 0,
            },
        })
    }
}

pub struct Gc {
    buf: Vec<GcData>,
    buf_used: usize,
    move_buf: Vec<usize>,
    registers: Vec<Value>,
    frame_start: usize,
    cur_alloc: usize,
    max_alloc: usize,
}

impl Gc {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_frame(&mut self, num_regs: Reg) {
        self.frame_start += num_regs as usize;
    }

    pub fn pop_frame(&mut self, num_regs: Reg) {
        self.frame_start -= num_regs as usize;
    }

    #[must_use]
    pub fn alloc(&mut self, len: Len) -> Ptr {
        let len_usize = usize::try_from(len).unwrap();

        let next_head = self.buf_used + len_usize + 1;
        if self.buf.capacity() <= next_head + 4 {
            let new_capacity = (next_head + 4) * 2;
            self.buf.reserve(new_capacity - self.buf.capacity());
            self.buf.resize_with(self.buf.capacity(), Default::default);
        }
        let ret = self.buf_used;
        self.buf[ret] = GcData::new_header(false, ValueKind::Ptr, len);
        self.cur_alloc += len_usize + 1;
        self.buf_used = next_head;
        Ptr(ret + 1)
    }

    #[must_use]
    pub fn array_len(&self, ptr: Ptr) -> Len {
        self.buf[ptr.0 - 1].header().size
    }

    #[cfg(feature = "check_bounds")]
    #[must_use]
    fn array_len_usize(&self, ptr: Ptr) -> usize {
        self.buf[ptr.0 - 1].header().len()
    }

    #[cfg(feature = "check_bounds")]
    fn check_bounds(&self, ptr: Ptr, idx: usize) {
        let len = self.array_len_usize(ptr);
        if idx >= len {
            panic!("array index out of bounds: the length is {len} but the index is {idx}")
        }
    }

    #[must_use]
    pub fn get(&self, ptr: Ptr, idx: usize) -> Value {
        #[cfg(feature = "check_bounds")]
        self.check_bounds(ptr, idx);

        self.buf[ptr.0 + idx].value()
    }

    #[must_use]
    pub fn get_mut(&mut self, ptr: Ptr, idx: usize) -> &mut Value {
        #[cfg(feature = "check_bounds")]
        self.check_bounds(ptr, idx);

        self.buf[ptr.0 + idx].value_mut()
    }

    #[must_use]
    fn should_run(&self) -> bool {
        self.cur_alloc >= self.max_alloc
    }

    pub fn run(&mut self) {
        macro_rules! move_array {
            (|$i:ident| $get:expr) => {{
                if $get.is_ptr() {
                    let n = $get.as_ptr_unchecked().0;
                    if n < self.buf_used {
                        let new_pos = self.move_buf[n];
                        assert_ne!(
                            new_pos,
                            usize::MAX,
                            "indexed into an uninitialized section of `move_buf`"
                        );
                        $get = Value::ptr_unchecked(Ptr(new_pos));
                    }
                }
            }};
        }

        if !self.should_run() {
            return;
        }

        for i in 0..self.registers.len() {
            self.mark(self.registers[i]);
        }

        if self.buf_used > self.move_buf.len() {
            self.move_buf.resize(self.buf_used + self.buf_used / 2, 0);
        }

        let mut used = 0;
        let mut i = 0;
        while i < self.buf_used {
            let mut data = self.buf[i];
            let header = data.header();
            i += 1;

            if header.is_marked() {
                data.header_mut().set_marked(false);
                self.buf[used] = data;
                used += 1;
                if self.move_buf.len() <= i {
                    self.move_buf.resize(i + 1, usize::MAX);
                }
                self.move_buf[i] = used;
                for _ in 0..header.size {
                    self.buf[used] = self.buf[i];
                    used += 1;
                    i += 1;
                }
            } else {
                i += header.len();
            }
        }

        for i in 0..self.registers.len() {
            move_array!(|i| self.registers[i]);
        }

        let mut i = 0;
        while i < used {
            let data = self.buf[i];
            i += 1;
            for _ in 0..data.header().len() {
                move_array!(|i| *self.buf[i].value_mut());
                i += 1;
            }
        }

        self.buf.truncate(used);
        self.cur_alloc = 0;
        self.max_alloc = used.max(GC_MIN_THRESHOLD);
    }

    fn mark(&mut self, value: Value) {
        if value.is_ptr() {
            let index = value.as_ptr_unchecked().0;
            if index < self.buf_used {
                let header = self.buf[index - 1].header_mut();
                if header.is_marked() {
                    return;
                }
                header.set_marked(true);
                for i in 0..header.len() {
                    let value = self.buf[index + i].value();
                    self.mark(value);
                }
            }
        }
    }
}

impl Default for Gc {
    fn default() -> Self {
        Self {
            buf: Vec::new(),
            buf_used: 0,
            move_buf: Vec::new(),
            registers: vec![Value::int_unchecked(0); NUM_REGISTERS],
            frame_start: 0,
            cur_alloc: 0,
            max_alloc: GC_MIN_THRESHOLD,
        }
    }
}

impl Index<Reg> for Gc {
    type Output = Value;

    #[inline]
    fn index(&self, reg: Reg) -> &Self::Output {
        let index: usize = reg.into();
        &self.registers[self.frame_start + index]
    }
}

impl IndexMut<Reg> for Gc {
    #[inline]
    fn index_mut(&mut self, reg: Reg) -> &mut Self::Output {
        let index: usize = reg.into();
        &mut self.registers[self.frame_start + index]
    }
}