use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BitWidth {
    Bit8,
    Bit32,
}

const ALL_BIT_WIDTH: [BitWidth; 2] = [BitWidth::Bit8, BitWidth::Bit32];

impl BitWidth {
    pub fn all() -> &'static [BitWidth] {
        &ALL_BIT_WIDTH
    }

    pub fn int(self) -> i32 {
        match self {
            BitWidth::Bit8 => 8,
            BitWidth::Bit32 => 32,
        }
    }
}

impl PartialOrd for BitWidth {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BitWidth {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.int().cmp(&other.int())
    }
}

impl Display for BitWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BitWidth::Bit8 => write!(f, "8"),
            BitWidth::Bit32 => write!(f, "32"),
        }
    }
}
