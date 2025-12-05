use crate::dynasm::{i2a_slice, update_label_table};
use crate::errors::RuntimeError;
use crate::instr::Instr;
use dynasmrt::x64::Assembler;
use dynasmrt::{dynasm, AssemblyOffset, DynasmApi, DynasmLabelApi};
use std::collections::HashMap;

#[derive(derive_more::Display)]
pub enum ReturnValue {
    #[display("{_0}")]
    Num(i64),
    #[display("{_0}")]
    Bool(bool),
}

// this struct will own the assembler and control all usage of it
pub struct EaterOfWords {
    ops: dynasmrt::x64::Assembler,
    labels: HashMap<String, dynasmrt::DynamicLabel>,
    start: AssemblyOffset,
}

pub(crate) fn snek_end(val: i64) -> Result<ReturnValue, RuntimeError> {
    // let test: i64 = val & 0b111;
    match val & 7 {
        0b111 => match (val >> 3) & 7 {
            0 => Err(RuntimeError::TypeError),
            1 => Err(RuntimeError::OverflowError),
            2 => Err(RuntimeError::CastError),
            _ => Err(RuntimeError::BadError),
        },
        0b011 => Ok(ReturnValue::Bool(true)),
        0b001 => Ok(ReturnValue::Bool(false)),
        _ if val & 1 == 0 => Ok(ReturnValue::Num(val >> 1)),
        _ => Err(RuntimeError::BadError),
    }
}

impl EaterOfWords {
    pub fn new() -> EaterOfWords {
        let ops: Assembler = Assembler::new().unwrap();
        EaterOfWords {
            start: ops.offset(),
            ops,
            labels: HashMap::new(),
        }
    }

    pub fn prep_fn(&mut self, fname: String) -> Option<()> {
        let xsxx = self.ops.new_dynamic_label();
        self.labels.insert(fname, xsxx);
        Some(())
    }

    /// Consume the body of instructions `instrs` for a function with name `name` and
    pub fn consume_slice(&mut self, name: String, instrs: &[Instr]) -> Option<()> {
        // actually do eat the same function twice, someone else will validate this
        let label = match self.labels.get(&name) {
            Some(label) => *label,
            None => {
                // always write what you ate
                let label = self.ops.new_dynamic_label();
                self.labels.insert(name.to_string(), label);
                label
            }
        };

        update_label_table(&mut self.ops, instrs, &mut self.labels);

        dynasm!(self.ops ; =>label);
        i2a_slice(&mut self.ops, instrs, &self.labels)
    }

    pub fn digest(&mut self) {
        // dynasm!(self.ops ; .arch x64 ; ret);
        self.ops.commit().unwrap();
    }

    #[allow(dead_code)]
    pub fn exert(&mut self, arg: Option<*mut i64>) -> Result<ReturnValue, RuntimeError> {
        let reader = self.ops.reader();
        let buf = reader.lock();
        snek_end(match arg {
            Some(arg_ptr) => {
                let jitted_fn: extern "C" fn(*mut i64) -> i64 =
                    unsafe { std::mem::transmute(buf.ptr(self.start)) };
                jitted_fn(arg_ptr)
            }
            None => {
                let jitted_fn: extern "C" fn() -> i64 =
                    unsafe { std::mem::transmute(buf.ptr(self.start)) };
                jitted_fn()
            }
        })
    }

    pub fn current_offset(&self) -> *const u8 {
        let reader = self.ops.reader();
        let buf = reader.lock();
        buf.ptr(self.start)
    }

    pub fn discard(&mut self) {
        self.start = self.ops.offset();
    }
}
