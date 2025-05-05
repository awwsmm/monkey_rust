struct Instructions(Vec<u8>);

enum Opcode {
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
        match self {
            0 => Opcode::OpConstant,
            _ => panic!("unknown opcode discriminant: {}", self)
        }
    }
}