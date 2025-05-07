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

#[derive(Clone, Copy)]
pub(crate) enum Opcode {
    OpConstant,
    OpAdd,
}

impl Into<u8> for Opcode {
    fn into(self) -> u8 {
        match self {
            Opcode::OpConstant => 0,
            Opcode::OpAdd => 1,
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

// equivalent of "func Lookup()" in Go implementation
impl Into<Definition> for u8 {
    fn into(self) -> Definition {
        match self {
            0 => OP_CONSTANT,
            1 => OP_ADD,
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

    for (i, width) in def.operand_widths.iter().enumerate() {
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
            make(Opcode::OpConstant, &vec![1]),
            make(Opcode::OpConstant, &vec![2]),
            make(Opcode::OpConstant, &vec![65535]),
        ];

        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
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
            eprintln!("instructions wrongly formatted.\nwant={:?}\ngot={:?}",
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
