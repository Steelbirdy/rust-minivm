use crate::{
    common::Reg,
    vm::{Value, ValueKind},
};
use std::ops::{Index, IndexMut};

pub struct Registers {
    regs: Vec<RegState>,
}

impl Registers {
    pub fn reset(&mut self) {
        for reg in &mut self.regs {
            reg.reset();
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (Reg, ConstantReg<'_>)> {
        self.regs
            .iter()
            .enumerate()
            .filter_map(|(i, reg)| reg.as_constant().map(|reg| (i as Reg, reg)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Reg, ConstantRegMut<'_>)> {
        self.regs
            .iter_mut()
            .enumerate()
            .filter_map(|(i, reg)| reg.as_constant_mut().map(|reg| (i as Reg, reg)))
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            regs: vec![RegState::default(); 16],
        }
    }
}

impl Index<Reg> for Registers {
    type Output = RegState;

    fn index(&self, reg: Reg) -> &Self::Output {
        &self.regs[reg as usize]
    }
}

impl IndexMut<Reg> for Registers {
    fn index_mut(&mut self, reg: Reg) -> &mut Self::Output {
        &mut self.regs[reg as usize]
    }
}

#[derive(Default, Copy, Clone)]
pub struct RegState {
    value: Option<Value>,
    in_scope: bool,
}

impl RegState {
    pub fn is_constant(&self) -> bool {
        self.value.is_some()
    }

    pub fn get(&self) -> Option<Value> {
        self.value
    }

    pub fn set(&mut self, value: Value) {
        self.in_scope = false;
        self.value = Some(value);
    }

    pub fn is_in_scope(&self) -> bool {
        self.in_scope || self.value.is_none()
    }

    pub fn put_in_scope(&mut self) {
        if self.value.is_some() {
            self.in_scope = true;
        }
    }

    pub fn ty(&self) -> Option<ValueKind> {
        self.value.map(|v| v.kind())
    }

    pub fn reset(&mut self) {
        *self = Self::default();
    }

    fn as_constant(&self) -> Option<ConstantReg> {
        self.is_constant().then(|| ConstantReg(self))
    }

    fn as_constant_mut(&mut self) -> Option<ConstantRegMut> {
        self.is_constant().then(|| ConstantRegMut(self))
    }
}

pub struct ConstantReg<'a>(&'a RegState);

impl<'a> ConstantReg<'a> {
    pub fn get(&self) -> Value {
        self.0.value.unwrap()
    }

    pub fn is_in_scope(&self) -> bool {
        self.0.in_scope
    }

    pub fn ty(&self) -> ValueKind {
        self.get().kind()
    }
}

pub struct ConstantRegMut<'a>(&'a mut RegState);

impl<'a> ConstantRegMut<'a> {
    pub fn get(&self) -> Value {
        self.0.value.unwrap()
    }

    pub fn set(&mut self, value: Value) {
        self.0.value = Some(value);
    }

    pub fn is_in_scope(&self) -> bool {
        self.0.in_scope
    }

    pub fn put_in_scope(&mut self) {
        self.0.in_scope = true;
    }

    pub fn ty(&self) -> ValueKind {
        self.get().kind()
    }
}
