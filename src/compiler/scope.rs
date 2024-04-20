use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use self::err::ScopeError;

use super::err::CompileError;

mod err;

pub enum Tag {
    Anonymous,
    Named(&'static str),
    Function(String),
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
        use Tag::{Anonymous, Builtin, Function, Global, Named};
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
        Scope {
            current_layer: None,
        }
    }

    pub fn enter(&mut self, tag: Tag) {
        let new_layer = Box::new(Layer::new(tag, self.current_layer.take()));
        self.current_layer = Some(new_layer);
    }

    pub fn leave(&mut self, tag: Tag) -> Result<(), CompileError> {
        if self.current_layer.is_none() {
            return Err(ScopeError::NullScope.into());
        }
        let layer = self.current_layer.take().unwrap();
        if layer.tag != tag {
            return Err(ScopeError::ScopeMismatch {
                expected: tag,
                encountered: layer.tag,
            }
            .into());
        }
        self.current_layer = layer.outer;
        Ok(())
    }

    pub fn declare(&mut self, symbol: String, symbol_info: T) -> Result<(), CompileError> {
        self.current_layer
            .as_mut()
            .map_or(Err(ScopeError::NullScope.into()), |layer| {
                layer.declare(symbol, symbol_info)
            })
    }

    pub fn lookup_symbol(&self, symbol: &String) -> Result<Option<T>, CompileError> {
        self.current_layer
            .as_ref()
            .map_or(Err(ScopeError::NullScope.into()), |layer| {
                Ok(layer.lookup(symbol))
            })
    }

    pub fn exist_symbol(&self, symbol: &String) -> Result<bool, CompileError> {
        Ok(self.lookup_symbol(symbol)?.is_some())
    }

    pub fn is_global(&self) -> Result<bool, CompileError> {
        self.current_layer
            .as_ref()
            .map_or(Err(ScopeError::NullScope.into()), |layer| {
                Ok(layer.tag == Tag::Global)
            })
    }

    pub fn get_current_function_name(&self) -> Result<String, CompileError> {
        self.current_layer
            .as_ref()
            .map_or(Err(ScopeError::NullScope.into()), |layer| {
                layer.get_current_function_name()
            })
    }
}

impl<T: Clone> Default for Scope<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub type LayerLink<T> = Option<Box<Layer<T>>>;

pub struct Layer<T: Clone> {
    pub tag: Tag,
    pub symbol_table: HashMap<String, T>,
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

    pub fn declare(&mut self, symbol: String, symbol_info: T) -> Result<(), CompileError> {
        if self.symbol_table.contains_key(&symbol) {
            return Err(ScopeError::DuplicateIdentifierInSameScope(symbol.clone()).into());
        }
        self.symbol_table.insert(symbol, symbol_info);
        Ok(())
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

    pub fn get_current_function_name(&self) -> Result<String, CompileError> {
        if let Tag::Function(name) = &self.tag {
            return Ok(name.clone());
        }
        self.outer
            .as_ref()
            .map_or(Err(ScopeError::NotInFunction.into()), |outer| {
                outer.get_current_function_name()
            })
    }
}
