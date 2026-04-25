use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::rc::Rc;

use crate::CompileResult;
use crate::sys_error;

use super::err::CompileError;
use super::syntax::ast::crumb::Identifier;

pub enum Tag {
    Anonymous,
    Named(&'static str),
    Function(Rc<Identifier>),
    Global,
    Builtin,
}

impl Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tag::Anonymous => write!(f, "(Anonymous)"),
            Tag::Named(name) => write!(f, "Named {name}"),
            Tag::Function(name) => write!(f, "Function {name}"),
            Tag::Global => write!(f, "Global"),
            Tag::Builtin => write!(f, "Builtin"),
        }
    }
}

impl PartialEq for Tag {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Tag::Anonymous, Tag::Anonymous) | (Tag::Builtin, Tag::Builtin) | (Tag::Global, Tag::Global) => true,
            (Tag::Named(l), Tag::Named(r)) => l == r,
            (Tag::Function(l), Tag::Function(r)) => l == r,
            _ => false,
        }
    }
}

pub struct Scope<T: Clone> {
    inner: Rc<RefCell<ScopeInner<T>>>,
}

struct ScopeInner<T: Clone> {
    current_layer: Option<Box<Layer<T>>>,
}

impl<T: Clone> Scope<T> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(ScopeInner { current_layer: None })),
        }
    }

    /// Pushes a new layer and returns a guard that pops it on drop.
    ///
    /// Bind to a named local (`let _guard = ...`); using a bare `_` causes the
    /// guard to drop immediately and the layer to be popped right away.
    #[must_use = "the returned ScopeGuard pops the layer when dropped; bind it to control the layer lifetime"]
    pub fn enter(&self, tag: Tag) -> ScopeGuard<T> {
        let mut inner = self.inner.borrow_mut();
        let outer = inner.current_layer.take();
        inner.current_layer = Some(Box::new(Layer::new(tag, outer)));
        ScopeGuard {
            inner: self.inner.clone(),
        }
    }

    /// # Errors
    /// Returns `CompileError::DuplicateIdentifierInSameScope` when the identifier already exists in the current layer.
    pub fn declare(&self, symbol: Rc<Identifier>, symbol_info: T) -> CompileResult<()> {
        let mut inner = self.inner.borrow_mut();
        let layer = inner
            .current_layer
            .as_mut()
            .unwrap_or_else(|| sys_error!("using scope before entering a layer"));
        layer.declare(symbol, symbol_info)
    }

    pub fn overwrite(&self, symbol: Rc<Identifier>, symbol_info: T) {
        let mut inner = self.inner.borrow_mut();
        let layer = inner
            .current_layer
            .as_mut()
            .unwrap_or_else(|| sys_error!("using scope before entering a layer"));
        layer.overwrite(symbol, symbol_info);
    }

    #[must_use]
    pub fn lookup(&self, symbol: &String) -> Option<T> {
        let inner = self.inner.borrow();
        let layer = inner
            .current_layer
            .as_ref()
            .unwrap_or_else(|| sys_error!("using scope before entering a layer"));
        layer.lookup(symbol)
    }

    #[must_use]
    pub fn is_global(&self) -> bool {
        let inner = self.inner.borrow();
        let layer = inner
            .current_layer
            .as_ref()
            .unwrap_or_else(|| sys_error!("checking global scope before entering a layer"));
        layer.tag == Tag::Global
    }

    #[must_use]
    pub fn current_function(&self) -> Rc<Identifier> {
        let inner = self.inner.borrow();
        let layer = inner
            .current_layer
            .as_ref()
            .unwrap_or_else(|| sys_error!("checking current function before entering a layer"));
        layer.get_current_function_name()
    }
}

impl<T: Clone> Default for Scope<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ScopeGuard<T: Clone> {
    inner: Rc<RefCell<ScopeInner<T>>>,
}

impl<T: Clone> Drop for ScopeGuard<T> {
    fn drop(&mut self) {
        let mut inner = self.inner.borrow_mut();
        let layer = inner
            .current_layer
            .take()
            .unwrap_or_else(|| sys_error!("scope guard dropped without an active layer"));
        inner.current_layer = layer.outer;
    }
}

type LayerLink<T> = Option<Box<Layer<T>>>;

struct Layer<T: Clone> {
    tag: Tag,
    symbol_table: HashMap<Rc<Identifier>, T>,
    outer: LayerLink<T>,
}

impl<T: Clone> Layer<T> {
    fn new(tag: Tag, outer: LayerLink<T>) -> Layer<T> {
        Layer {
            tag,
            symbol_table: HashMap::new(),
            outer,
        }
    }

    fn declare(&mut self, symbol: Rc<Identifier>, symbol_info: T) -> CompileResult<()> {
        if self.symbol_table.contains_key(&symbol) {
            return Err(CompileError::DuplicateIdentifierInSameScope(symbol.clone()));
        }
        self.symbol_table.insert(symbol, symbol_info);
        Ok(())
    }

    fn overwrite(&mut self, symbol: Rc<Identifier>, symbol_info: T) {
        self.symbol_table.insert(symbol, symbol_info);
    }

    fn lookup(&self, symbol: &String) -> Option<T> {
        if let Some(value) = self.symbol_table.get(symbol) {
            return Some(value.clone());
        }
        self.outer.as_ref().and_then(|outer| outer.lookup(symbol))
    }

    fn get_current_function_name(&self) -> Rc<Identifier> {
        if let Tag::Function(name) = &self.tag {
            return name.clone();
        }
        self.outer.as_ref().map_or_else(
            || sys_error!("current scope is not inside a function"),
            |outer| outer.get_current_function_name(),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::Scope;
    use super::Tag;
    use crate::CompileResult;
    use crate::compiler::err::CompileError;
    use crate::compiler::syntax::ast::crumb::Identifier;

    fn ident(name: &str) -> Rc<Identifier> {
        Rc::new(name.to_string())
    }

    fn lookup(scope: &Scope<i32>, name: &str) -> Option<i32> {
        scope.lookup(&name.to_string())
    }

    fn declare(scope: &Scope<i32>, name: &str, value: i32) {
        if scope.declare(ident(name), value).is_err() {
            panic!("unexpected declare error");
        }
    }

    #[test]
    fn guard_pops_layer_on_drop() {
        let scope = Scope::<i32>::new();
        let _outer = scope.enter(Tag::Global);
        {
            let _inner = scope.enter(Tag::Anonymous);
            declare(&scope, "x", 1);
            assert_eq!(lookup(&scope, "x"), Some(1));
        }
        assert_eq!(lookup(&scope, "x"), None);
    }

    #[test]
    fn nested_layers_shadow() {
        let scope = Scope::<i32>::new();
        let _outer = scope.enter(Tag::Global);
        declare(&scope, "x", 1);
        {
            let _inner = scope.enter(Tag::Anonymous);
            declare(&scope, "x", 2);
            assert_eq!(lookup(&scope, "x"), Some(2));
        }
        assert_eq!(lookup(&scope, "x"), Some(1));
    }

    #[test]
    fn early_return_through_question_mark_pops_layer() {
        fn inner_op(scope: &Scope<i32>) -> CompileResult<()> {
            let _g = scope.enter(Tag::Anonymous);
            scope.declare(ident("x"), 1)?;
            Err(CompileError::IllegalLValue)?;
            unreachable!()
        }
        let scope = Scope::<i32>::new();
        let _outer = scope.enter(Tag::Global);
        assert!(inner_op(&scope).is_err());
        assert_eq!(lookup(&scope, "x"), None);
    }

    #[test]
    fn is_global_reflects_current_layer_tag() {
        let scope = Scope::<i32>::new();
        let _outer = scope.enter(Tag::Global);
        assert!(scope.is_global());
        let _inner = scope.enter(Tag::Anonymous);
        assert!(!scope.is_global());
    }

    #[test]
    fn current_function_walks_up_to_function_layer() {
        let scope = Scope::<i32>::new();
        let _g_global = scope.enter(Tag::Global);
        let _g_func = scope.enter(Tag::Function(ident("foo")));
        let _g_block = scope.enter(Tag::Anonymous);
        assert_eq!(scope.current_function().as_ref(), "foo");
    }

    #[test]
    fn duplicate_declare_in_same_layer_errors() {
        let scope = Scope::<i32>::new();
        let _g = scope.enter(Tag::Global);
        declare(&scope, "x", 1);
        let result = scope.declare(ident("x"), 2);
        assert!(matches!(result, Err(CompileError::DuplicateIdentifierInSameScope(_))));
    }

    #[test]
    fn lookup_walks_outer_layers() {
        let scope = Scope::<i32>::new();
        let _g_global = scope.enter(Tag::Global);
        declare(&scope, "g", 10);
        let _g_func = scope.enter(Tag::Function(ident("f")));
        declare(&scope, "f", 20);
        let _g_block = scope.enter(Tag::Anonymous);
        assert_eq!(lookup(&scope, "g"), Some(10));
        assert_eq!(lookup(&scope, "f"), Some(20));
    }
}
