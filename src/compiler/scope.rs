use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::CompileResult;

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
            Tag::Anonymous => {
                write!(f, "(Anonymous)")
            }
            Tag::Named(name) => {
                write!(f, "Named {name}")
            }
            Tag::Function(name) => {
                write!(f, "Function {name}")
            }
            Tag::Global => {
                write!(f, "Global")
            }
            Tag::Builtin => {
                write!(f, "Builtin")
            }
        }
    }
}

impl PartialEq for Tag {
    fn eq(&self, other: &Self) -> bool {
        use Tag::Anonymous;
        use Tag::Builtin;
        use Tag::Function;
        use Tag::Global;
        use Tag::Named;
        match (self, other) {
            (Anonymous, Anonymous) | (Builtin, Builtin) | (Global, Global) => true,
            (Named(l), Named(r)) => l == r,
            (Function(l), Function(r)) => l == r,
            _ => false,
        }
    }
}

pub struct Scope<T: Clone> {
    current_layer: LayerLink<T>,
}

impl<T: Clone> Scope<T> {
    pub fn new() -> Scope<T> {
        Scope { current_layer: None }
    }

    pub fn enter(&mut self, tag: Tag) {
        let new_layer = Box::new(Layer::new(tag, self.current_layer.take()));
        self.current_layer = Some(new_layer);
    }

    pub fn leave(&mut self, tag: Tag) -> CompileResult<()> {
        if self.current_layer.is_none() {
            return Err(CompileError::NullScope);
        }
        let layer = self.current_layer.take().unwrap();
        if layer.tag != tag {
            return Err(CompileError::ScopeMismatch {
                expected: tag,
                encountered: layer.tag,
            });
        }
        self.current_layer = layer.outer;
        Ok(())
    }

    pub fn declare(&mut self, symbol: Rc<Identifier>, symbol_info: T) -> CompileResult<()> {
        self.execute_mut(|layer| layer.declare(symbol, symbol_info))
    }

    pub fn overwrite(&mut self, symbol: Rc<Identifier>, symbol_info: T) -> CompileResult<()> {
        self.execute_mut(|layer| {
            layer.overwrite(symbol, symbol_info);
            Ok(())
        })
    }

    pub fn lookup(&self, symbol: &String) -> CompileResult<Option<T>> {
        self.execute(|layer| Ok(layer.lookup(symbol)))
    }

    fn execute<S>(&self, f: impl FnOnce(&Box<Layer<T>>) -> CompileResult<S>) -> CompileResult<S> {
        self.current_layer.as_ref().map_or(Err(CompileError::NullScope), f)
    }

    fn execute_mut<S>(&mut self, f: impl FnOnce(&mut Box<Layer<T>>) -> CompileResult<S>) -> CompileResult<S> {
        self.current_layer.as_mut().map_or(Err(CompileError::NullScope), f)
    }

    pub fn exist(&self, symbol: &String) -> CompileResult<bool> {
        Ok(self.lookup(symbol)?.is_some())
    }

    pub fn is_global(&self) -> CompileResult<bool> {
        self.current_layer
            .as_ref()
            .map_or(Err(CompileError::NullScope), |layer| Ok(layer.tag == Tag::Global))
    }

    pub fn current_function(&self) -> CompileResult<Rc<Identifier>> {
        self.current_layer
            .as_ref()
            .map_or(Err(CompileError::NullScope), |layer| layer.get_current_function_name())
    }
}

impl<T: Clone> Default for Scope<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Display for Scope<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(layer) = &self.current_layer {
            write!(f, "{layer}")
        } else {
            Ok(())
        }
    }
}

pub type LayerLink<T> = Option<Box<Layer<T>>>;

pub struct Layer<T: Clone> {
    pub tag: Tag,
    pub symbol_table: HashMap<Rc<Identifier>, T>,
    pub outer: LayerLink<T>,
}

impl<T: Clone> Layer<T> {
    pub fn new(tag: Tag, outer: LayerLink<T>) -> Layer<T> {
        Layer {
            tag,
            symbol_table: HashMap::new(),
            outer,
        }
    }

    pub fn declare(&mut self, symbol: Rc<Identifier>, symbol_info: T) -> CompileResult<()> {
        if self.symbol_table.contains_key(&symbol) {
            return Err(CompileError::DuplicateIdentifierInSameScope(symbol.clone()));
        }
        self.symbol_table.insert(symbol, symbol_info);
        Ok(())
    }

    pub fn overwrite(&mut self, symbol: Rc<Identifier>, symbol_info: T) {
        self.symbol_table.insert(symbol, symbol_info);
    }

    pub fn lookup(&self, symbol: &String) -> Option<T> {
        let result = self.symbol_table.get(symbol);
        if result.is_some() {
            return result.cloned();
        }
        match self.outer.as_ref() {
            None => None,
            Some(outer) => outer.lookup(symbol),
        }
    }

    pub fn get_current_function_name(&self) -> CompileResult<Rc<Identifier>> {
        if let Tag::Function(name) = &self.tag {
            return Ok(name.clone());
        }
        self.outer.as_ref().map_or(Err(CompileError::NotInFunction), |outer| {
            outer.get_current_function_name()
        })
    }
}

impl<T: Clone> Display for Layer<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tag = &self.tag;
        write!(f, "{tag}")?;
        if let Some(outer) = &self.outer {
            write!(f, ", {outer}")?;
        }
        writeln!(f)
    }
}
