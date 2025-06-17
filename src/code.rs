use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};

#[derive(Clone, PartialEq)]
pub(crate) struct Instructions(pub(crate) Vec<u8>);

impl Debug for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Instructions {
    fn fmt_instruction(&self, def: Definition, operands: Vec<usize>) -> String {
        let operand_count = def.operand_widths.len();

        if operands.len() != operand_count {
            return format!("ERROR: operand len {} does not match defined {}\n",
            operands.len(), operand_count)
        }

        match operand_count {
            0 => return String::from(def.name),
            1 => return format!("{} {}", def.name, operands[0]),
            _ => ()
        }

        format!("ERROR: unhandled operand_count for {}\n", def.name)
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut out = vec![];

        let mut i = 0;
        while i < self.len() {
            let def: Definition = self[i].into();

            let (operands, read) = read_operands(&def, &self[i+1..]);

            out.push(format!("{:04} {}\n", i, self.fmt_instruction(def, operands)));

            i += 1 + read
        }

        write!(f, "{}", out.join(""))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Opcode {
    OpConstant,
    OpAdd,
    OpPop,
    OpSub,
    OpMul,
    OpDiv,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpMinus,
    OpBang,
    OpJumpNotTruthy,
    OpJump,
    OpNull,
    OpGetGlobal,
    OpSetGlobal,
    OpArray,
    OpHash,
    OpIndex,
    OpCall,
    OpReturnValue,
    OpReturn,
    OpGetLocal,
    OpSetLocal,
    OpGetBuiltin,
    OpClosure,
}

impl Into<u8> for Opcode {
    fn into(self) -> u8 {
        match self {
            Opcode::OpConstant => 0,
            Opcode::OpAdd => 1,
            Opcode::OpPop => 2,
            Opcode::OpSub => 3,
            Opcode::OpMul => 4,
            Opcode::OpDiv => 5,
            Opcode::OpTrue => 6,
            Opcode::OpFalse => 7,
            Opcode::OpEqual => 8,
            Opcode::OpNotEqual => 9,
            Opcode::OpGreaterThan => 10,
            Opcode::OpMinus => 11,
            Opcode::OpBang => 12,
            Opcode::OpJumpNotTruthy => 13,
            Opcode::OpJump => 14,
            Opcode::OpNull => 15,
            Opcode::OpGetGlobal => 16,
            Opcode::OpSetGlobal => 17,
            Opcode::OpArray => 18,
            Opcode::OpHash => 19,
            Opcode::OpIndex => 20,
            Opcode::OpCall => 21,
            Opcode::OpReturnValue => 22,
            Opcode::OpReturn => 23,
            Opcode::OpGetLocal => 24,
            Opcode::OpSetLocal => 25,
            Opcode::OpGetBuiltin => 26,
            Opcode::OpClosure => 27,
        }
    }
}

impl Into<Opcode> for u8 {
    fn into(self) -> Opcode {
        <Self as Into<Definition>>::into(self).opcode
    }
}

// additional "opcode" field not present in Go implementation makes Into<Opcode> cleaner
struct Definition {
    opcode: Opcode,
    name: &'static str,
    operand_widths: &'static [usize],
}

// series of "const" bindings is equivalent to "var definitions" in Go implementation
const OP_CONSTANT: Definition = Definition{
    opcode: Opcode::OpConstant,
    name: "OpConstant",
    operand_widths: &[2],
};

const OP_ADD: Definition = Definition{
    opcode: Opcode::OpAdd,
    name: "OpAdd",
    operand_widths: &[],
};

const OP_POP: Definition = Definition{
    opcode: Opcode::OpPop,
    name: "OpPop",
    operand_widths: &[],
};

const OP_SUB: Definition = Definition{
    opcode: Opcode::OpSub,
    name: "OpSub",
    operand_widths: &[],
};

const OP_MUL: Definition = Definition{
    opcode: Opcode::OpMul,
    name: "OpMul",
    operand_widths: &[],
};

const OP_DIV: Definition = Definition{
    opcode: Opcode::OpDiv,
    name: "OpDiv",
    operand_widths: &[],
};

const OP_TRUE: Definition = Definition{
    opcode: Opcode::OpTrue,
    name: "OpTrue",
    operand_widths: &[],
};

const OP_FALSE: Definition = Definition{
    opcode: Opcode::OpFalse,
    name: "OpFalse",
    operand_widths: &[],
};

const OP_EQUAL: Definition = Definition{
    opcode: Opcode::OpEqual,
    name: "OpEqual",
    operand_widths: &[],
};

const OP_NOT_EQUAL: Definition = Definition{
    opcode: Opcode::OpNotEqual,
    name: "OpNotEqual",
    operand_widths: &[],
};

const OP_GREATER_THAN: Definition = Definition{
    opcode: Opcode::OpGreaterThan,
    name: "OpGreaterThan",
    operand_widths: &[],
};

const OP_MINUS: Definition = Definition{
    opcode: Opcode::OpMinus,
    name: "OpMinus",
    operand_widths: &[],
};

const OP_BANG: Definition = Definition{
    opcode: Opcode::OpBang,
    name: "OpBang",
    operand_widths: &[],
};

const OP_JUMP_NOT_TRUTHY: Definition = Definition{
    opcode: Opcode::OpJumpNotTruthy,
    name: "OpJumpNotTruthy",
    operand_widths: &[2],
};

const OP_JUMP: Definition = Definition{
    opcode: Opcode::OpJump,
    name: "OpJump",
    operand_widths: &[2],
};

const OP_NULL: Definition = Definition{
    opcode: Opcode::OpNull,
    name: "OpNull",
    operand_widths: &[],
};

const OP_GET_GLOBAL: Definition = Definition{
    opcode: Opcode::OpGetGlobal,
    name: "OpGetGlobal",
    operand_widths: &[2],
};

const OP_SET_GLOBAL: Definition = Definition{
    opcode: Opcode::OpSetGlobal,
    name: "OpSetGlobal",
    operand_widths: &[2],
};

const OP_ARRAY: Definition = Definition{
    opcode: Opcode::OpArray,
    name: "OpArray",
    operand_widths: &[2],
};

const OP_HASH: Definition = Definition{
    opcode: Opcode::OpHash,
    name: "OpHash",
    operand_widths: &[2],
};

const OP_INDEX: Definition = Definition{
    opcode: Opcode::OpIndex,
    name: "OpIndex",
    operand_widths: &[],
};

const OP_CALL: Definition = Definition{
    opcode: Opcode::OpCall,
    name: "OpCall",
    operand_widths: &[1],
};

const OP_RETURN_VALUE: Definition = Definition{
    opcode: Opcode::OpReturnValue,
    name: "OpReturnValue",
    operand_widths: &[],
};

const OP_RETURN: Definition = Definition{
    opcode: Opcode::OpReturn,
    name: "OpReturn",
    operand_widths: &[],
};

const OP_GET_LOCAL: Definition = Definition{
    opcode: Opcode::OpGetLocal,
    name: "OpGetLocal",
    operand_widths: &[1],
};

const OP_SET_LOCAL: Definition = Definition{
    opcode: Opcode::OpSetLocal,
    name: "OpSetLocal",
    operand_widths: &[1],
};

const OP_GET_BUILTIN: Definition = Definition{
    opcode: Opcode::OpGetBuiltin,
    name: "OpGetBuiltin",
    operand_widths: &[1],
};

const OP_CLOSURE: Definition = Definition{
    opcode: Opcode::OpClosure,
    name: "OpClosure",
    operand_widths: &[2, 1],
};

// equivalent of "func Lookup()" in Go implementation
impl Into<Definition> for u8 {
    fn into(self) -> Definition {
        match self {
            0 => OP_CONSTANT,
            1 => OP_ADD,
            2 => OP_POP,
            3 => OP_SUB,
            4 => OP_MUL,
            5 => OP_DIV,
            6 => OP_TRUE,
            7 => OP_FALSE,
            8 => OP_EQUAL,
            9 => OP_NOT_EQUAL,
            10 => OP_GREATER_THAN,
            11 => OP_MINUS,
            12 => OP_BANG,
            13 => OP_JUMP_NOT_TRUTHY,
            14 => OP_JUMP,
            15 => OP_NULL,
            16 => OP_GET_GLOBAL,
            17 => OP_SET_GLOBAL,
            18 => OP_ARRAY,
            19 => OP_HASH,
            20 => OP_INDEX,
            21 => OP_CALL,
            22 => OP_RETURN_VALUE,
            23 => OP_RETURN,
            24 => OP_GET_LOCAL,
            25 => OP_SET_LOCAL,
            26 => OP_GET_BUILTIN,
            27 => OP_CLOSURE,
            _ => panic!("opcode {} undefined", self)
        }
    }
}

impl Into<Definition> for Opcode {
    fn into(self) -> Definition {
        Into::<u8>::into(self).into()
    }
}

pub(crate) fn make(op: Opcode, operands: &Vec<usize>) -> Vec<u8> {
    let def: Definition = op.into();

    let mut instruction_len = 1;
    for w in def.operand_widths.iter() {
        instruction_len += w
    }

    let mut instruction: Vec<u8> = vec![];
    instruction.push(op.into());

    let mut offset = 1;
    for (i, o) in operands.iter().enumerate() {
        let width = def.operand_widths.get(i).unwrap();
        match width {
            2 => u16::try_from(*o).unwrap().to_be_bytes().iter().for_each(|b| instruction.push(*b)),
            1 => instruction.push(*o as u8),
            _ => panic!("unimplemented")
        }
        offset += width
    }

    instruction
}

fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = vec![];
    let mut offset = 0;

    for width in def.operand_widths.iter() {
        match *width {
            2 => operands.push(read_two_bytes(&ins[offset..])),
            1 => operands.push(read_one_byte(&ins[offset..])),
            _ => panic!()
        }

        offset += width
    }

    (operands, offset)
}

pub(crate) fn read_two_bytes(ins: &[u8]) -> usize {
    // https://stackoverflow.com/a/50244328/2925434
    ((ins[0] as usize) << 8) | ins[1] as usize
}

pub(crate) fn read_one_byte(ins: &[u8]) -> usize {
    ins[0] as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        struct Test {
            op: Opcode,
            operands: Vec<usize>,
            expected: Vec<u8>,
        }

        impl Test {
            fn new(op: Opcode, operands: Vec<usize>, expected: Vec<u8>) -> Self {
                Self { op, operands, expected }
            }
        }

        let tests = vec![
            Test::new(Opcode::OpConstant, vec![65534], vec![Opcode::OpConstant.into(), 255, 254]),
            Test::new(Opcode::OpAdd, vec![], vec![Opcode::OpAdd.into()]),
            Test::new(Opcode::OpGetLocal, vec![255], vec![Opcode::OpGetLocal.into(), 255]),
        ];

        for tt in tests.into_iter() {
            let instruction: Vec<u8> = make(tt.op, &tt.operands);

            if instruction.len() != tt.expected.len() {
                eprintln!("instruction has wrong length. want={}, got={}",
                          tt.expected.len(), instruction.len())
            }

            for (i, b) in tt.expected.iter().enumerate() {
                if instruction.get(i) != tt.expected.get(i) {
                    eprintln!("wrong byte at pos {}. want={}, got={:?}",
                              i, b, instruction.get(i))
                }
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Opcode::OpAdd, &vec![]),
            make(Opcode::OpGetLocal, &vec![1]),
            make(Opcode::OpConstant, &vec![2]),
            make(Opcode::OpConstant, &vec![65535]),
        ];

        let expected = r#"0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
"#;
        let mut concatted = Instructions(vec![]);
        for ins in instructions.iter() {
            for byte in ins.iter() {
                concatted.push(*byte)
            }
        }

        let mut should_panic = false;

        if concatted.to_string() != expected {
            should_panic = true;
            eprintln!("instructions wrongly formatted.\nwant={:?}\ngot ={:?}",
                expected, concatted.to_string())
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_read_operands() {
        struct Test {
            op: Opcode,
            operands: Vec<usize>,
            bytes_read: usize,
        }

        impl Test {
            fn new(op: Opcode, operands: Vec<usize>, bytes_read: usize) -> Self {
                Self { op, operands, bytes_read }
            }
        }

        let tests = vec![
            Test::new(Opcode::OpConstant, vec![65535], 2),
            Test::new(Opcode::OpGetLocal, vec![255], 1),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let instruction = make(tt.op, &tt.operands);

            let def: Definition = tt.op.into();

            let (operands_read, n) = read_operands(&def, &instruction[1..]);
            if n != tt.bytes_read {
                panic!("n wrong. want={}, got={}", tt.bytes_read, n)
            }

            for (i, want) in tt.operands.iter().enumerate() {
                if operands_read.get(i) != Some(want) {
                    should_panic = true;
                    eprintln!("operand wrong. want={}, got={:?}", want, operands_read.get(i))
                }
            }
        }
    }
}
