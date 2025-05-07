#[derive(Clone, Debug)]
pub(crate) struct Instructions(pub(crate) Vec<u8>);

#[derive(Clone, Copy)]
pub(crate) enum Opcode {
    OpConstant,
}

impl Into<u8> for Opcode {
    fn into(self) -> u8 {
        match self {
            Opcode::OpConstant => 0,
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
    operand_widths: &'static [i32],
}

// series of "const" bindings is equivalent to "var definitions" in Go implementation
const OP_CONSTANT: Definition = Definition{
    opcode: Opcode::OpConstant,
    name: "OpConstant",
    operand_widths: &[2],
};

// equivalent of "func Lookup()" in Go implementation
impl Into<Definition> for u8 {
    fn into(self) -> Definition {
        match self {
            0 => OP_CONSTANT,
            _ => panic!("opcode {} undefined", self)
        }
    }
}

pub(crate) fn make(op: Opcode, operands: Vec<i32>) -> Vec<u8> {
    let def: Definition = Into::<u8>::into(op).into();

    let mut instruction_len = 1;
    for w in def.operand_widths.iter() {
        instruction_len += w
    }

    let mut instruction: Vec<u8> = vec![];
    instruction.push(op.into());

    let mut offset = 1;
    for (i, o) in operands.into_iter().enumerate() {
        let width = def.operand_widths.get(i).unwrap();
        match width {
            2 => u16::try_from(o).unwrap().to_be_bytes().iter().for_each(|b| instruction.push(*b)),
            _ => panic!("unimplemented")
        }
        offset += width
    }

    instruction
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        struct Test {
            op: Opcode,
            operands: Vec<i32>,
            expected: Vec<u8>,
        }

        impl Test {
            fn new(op: Opcode, operands: Vec<i32>, expected: Vec<u8>) -> Self {
                Self { op, operands, expected }
            }
        }

        let tests = vec![
            Test::new(Opcode::OpConstant, vec![65534], vec![Opcode::OpConstant.into(), 255, 254])
        ];

        for tt in tests.into_iter() {
            let instruction: Vec<u8> = make(tt.op, tt.operands);

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
}
