use std::fmt::Display;

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L: Display, R: Display> Display for Either<L, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Either::Left(l) => l.fmt(f),
            Either::Right(r) => r.fmt(f),
        }
    }
}
