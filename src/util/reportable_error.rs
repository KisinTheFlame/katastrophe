pub trait ReportableError {
    fn report(&self) -> !;
}
