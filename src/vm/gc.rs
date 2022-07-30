use crate::{
    common::{
        config::NUM_REGISTERS,
        Reg
    },
    vm::{
        value::{Value, ValueKind},
        Len, Ptr,
    },
};
use std::ops::{Index, IndexMut};

const GC_MIN_THRESHOLD: usize = 1 << 12;

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

impl GcData {
    pub fn new_header(marked: bool, kind: ValueKind, len: Len) -> Self {
        let header = GcHeader {
            mark: marked.into(),
            _kind: kind.into(),
            size: len,
        };
        Self(RawGcData { header })
    }

    pub fn new_value(value: Value) -> Self {
        Self(RawGcData { value })
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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_frame(&mut self, num_regs: Reg) {
        self.frame_start += num_regs as usize;
    }

    pub fn pop_frame(&mut self, num_regs: Reg) {
        self.frame_start -= num_regs as usize;
    }

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
        ret + 1
    }

    pub fn array_len(&self, ptr: Ptr) -> Len {
        self.buf[ptr - 1].header().size
    }

    fn array_len_usize(&self, ptr: Ptr) -> usize {
        self.buf[ptr - 1].header().len()
    }

    pub fn array(&self, ptr: Ptr) -> &[Value] {
        let len = self.array_len(ptr) as usize;
        assert!(ptr + len <= self.buf.len());
        // SAFETY:
        // * `GcData` has the same layout as `Value` since
        //   `GcData` is a union with `Value` as one of its fields.
        // * The slice below is equivalent to &self.buf[start..start + len]
        //   except in type. Since we assert that these indices are in bounds,
        //   the slice is in bounds.
        // qed
        unsafe {
            let ptr = self.buf.as_ptr().add(ptr);
            std::slice::from_raw_parts(ptr as *const Value, len)
        }
    }

    pub fn array_mut(&mut self, ptr: Ptr) -> &mut [Value] {
        let len = self.array_len_usize(ptr);
        // SAFETY: See `Gc::array`.
        unsafe {
            let ptr = self.buf.as_mut_ptr().add(ptr);
            std::slice::from_raw_parts_mut(ptr as *mut Value, len)
        }
    }

    fn should_run(&self) -> bool {
        self.cur_alloc >= self.max_alloc
    }

    pub fn run(&mut self) {
        macro_rules! move_array {
            (|$i:ident| $get:expr) => {{
                if $get.is_ptr() {
                    let n = $get.as_ptr_unchecked();
                    if n < self.buf_used {
                        let new_pos = self.move_buf[n];
                        assert_ne!(
                            new_pos,
                            usize::MAX,
                            "indexed into an uninitialized section of `move_buf`"
                        );
                        $get = Value::ptr_unchecked(new_pos);
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
            let index = value.as_ptr_unchecked();
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

impl Index<Ptr> for Gc {
    type Output = [Value];

    #[inline]
    fn index(&self, ptr: Ptr) -> &Self::Output {
        self.array(ptr)
    }
}

impl IndexMut<Ptr> for Gc {
    #[inline]
    fn index_mut(&mut self, ptr: Ptr) -> &mut Self::Output {
        self.array_mut(ptr)
    }
}
