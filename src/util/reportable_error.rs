pub trait Reportable {
    fn report(&self) -> !;
}
