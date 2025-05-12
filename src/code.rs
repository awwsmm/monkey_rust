use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub(crate) struct Instructions(pub(crate) Vec<u8>);

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

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut out = vec![];

        let mut i = 0;
        while i < self.0.len() {
            let def: Definition = self.0[i].into();

            let (operands, read) = read_operands(&def, &self.0[i+1..]);

            out.push(format!("{:04} {}\n", i, self.fmt_instruction(def, operands)));

            i += 1 + read
        }

        write!(f, "{}", out.join(""))
    }
}

#[derive(Clone, Copy, Debug)]
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
            2 => operands.push(read_u16(&ins[offset..]) as usize),
            _ => panic!()
        }

        offset += width
    }

    (operands, offset)
}

pub(crate) fn read_u16(ins: &[u8]) -> u16 {
    // https://stackoverflow.com/a/50244328/2925434
    ((ins[0] as u16) << 8) | ins[1] as u16
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
            make(Opcode::OpConstant, &vec![2]),
            make(Opcode::OpConstant, &vec![65535]),
        ];

        let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
"#;
        let mut concatted = Instructions(vec![]);
        for ins in instructions.iter() {
            for byte in ins.iter() {
                concatted.0.push(*byte)
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
