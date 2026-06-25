// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Default, Hash)]
pub struct IrValueId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Default)]
pub struct ParamPosition(pub usize);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StackSlotId(pub usize);

impl Add for StackSlotId {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self(self.0 + rhs.0)
    }
}

impl Sub for StackSlotId {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self(self.0 - rhs.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IrAddress {
	StackSlot(StackSlotId),
	BaseOffset(IrValueId, StackSlotId)
}

impl Add for IrAddress {
    type Output = IrAddress;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (IrAddress::StackSlot(lhs), IrAddress::StackSlot(rhs)) => IrAddress::StackSlot(StackSlotId(lhs.0 + rhs.0)),
            (IrAddress::BaseOffset(base, lhs, ), IrAddress::StackSlot(rhs))
            | (IrAddress::StackSlot(lhs), IrAddress::BaseOffset(base, rhs)) => IrAddress::BaseOffset(base, StackSlotId(lhs.0 + rhs.0)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IrValue {
    Constant(i64),
    Register(IrValueId), // virtual register
	Address(IrAddress),
}

impl IrValue {
    pub fn as_value_id(&self) -> Option<IrValueId> {
        match self {
            IrValue::Register(id) => Some(*id),
            _ => None,
        }
    }
}