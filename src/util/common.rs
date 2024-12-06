use std::rc::Rc;

pub type Arr<T> = Rc<[T]>;

pub type Array<T> = Rc<[Rc<T>]>;
