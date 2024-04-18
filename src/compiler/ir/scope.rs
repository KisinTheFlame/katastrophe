use std::collections::HashMap;

use super::{
    err::{IrError, IrErrorKind},
    id_generator::next_id,
    instruction::{IrType, Value},
};

#[derive(Debug)]
pub enum Tag {
    Anonymous,
    Named(&'static str),
    Dynamic(String),
    Global,
    Builtin,
}

impl PartialEq for Tag {
    fn eq(&self, other: &Self) -> bool {
        use Tag::{Anonymous, Builtin, Dynamic, Global, Named};
        match (self, other) {
            (Anonymous, Anonymous) | (Builtin, Builtin) | (Global, Global) => true,
            (Named(l), Named(r)) => l == r,
            (Dynamic(l), Dynamic(r)) => l == r,
            _ => false,
        }
    }
}

type LayerLink = Option<Box<ScopeLayer>>;

pub struct Scope {
    current_layer: LayerLink,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            current_layer: None,
        }
    }

    pub fn enter(&mut self, tag: Tag) {
        let new_layer = Box::new(ScopeLayer::new(tag, self.current_layer.take()));
        self.current_layer = Some(new_layer);
    }

    pub fn leave(&mut self, tag: Tag) -> Result<(), IrError> {
        if self.current_layer.is_none() {
            return Err(IrError {
                kind: IrErrorKind::NullScope,
            });
        }
        let layer = self.current_layer.take().unwrap();
        if layer.tag != tag {
            return Err(IrError {
                kind: IrErrorKind::ScopeMismatch {
                    expected: tag,
                    encountered: layer.tag,
                },
            });
        }
        self.current_layer = layer.outer;
        Ok(())
    }

    pub fn declare_anonymous(&self) -> Value {
        let id = next_id();
        Value::Register(format!("t{id}"))
    }

    // pub fn declare_immutable(&mut self, symbol: &String) -> Result<Value, IrError> {
    //     self.declare(symbol, Value::Register(next_id()))
    // }

    pub fn declare_parameter(
        &mut self,
        symbol: &String,
        data_type: &IrType,
    ) -> Result<Value, IrError> {
        let id = next_id();
        self.declare(
            symbol,
            data_type,
            Value::Parameter(format!("p{id}.{symbol}")),
        )
    }

    pub fn declare_mutable(
        &mut self,
        symbol: &String,
        data_type: &IrType,
    ) -> Result<Value, IrError> {
        let id = next_id();
        self.declare(
            symbol,
            data_type,
            Value::StackPointer(format!("v{id}.{symbol}")),
        )
    }

    pub fn declare_global(
        &mut self,
        symbol: &String,
        data_type: &IrType,
    ) -> Result<Value, IrError> {
        let id = next_id();
        self.declare(
            symbol,
            data_type,
            Value::GlobalPointer(format!("g{id}.{symbol}")),
        )
    }

    pub fn declare_function(
        &mut self,
        symbol: &String,
        data_type: &IrType,
    ) -> Result<Value, IrError> {
        let id = next_id();
        self.declare(
            symbol,
            data_type,
            Value::Function(format!("f{id}.{symbol}")),
        )
    }

    fn declare(
        &mut self,
        symbol: &String,
        data_type: &IrType,
        value: Value,
    ) -> Result<Value, IrError> {
        self.current_layer.as_mut().map_or(
            Err(IrError {
                kind: IrErrorKind::NullScope,
            }),
            |layer| {
                layer.declare(symbol, data_type, &value)?;
                Ok(value)
            },
        )
    }

    pub fn lookup_symbol(&self, symbol: &String) -> Result<Option<(Value, IrType)>, IrError> {
        self.current_layer.as_ref().map_or(
            Err(IrError {
                kind: IrErrorKind::NullScope,
            }),
            |layer| Ok(layer.lookup(symbol)),
        )
    }

    // pub fn exist_symbol(&self, symbol: &String) -> Result<bool, IrError> {
    //     Ok(self.lookup_symbol(symbol)?.is_some())
    // }

    pub fn is_global(&self) -> Result<bool, IrError> {
        self.current_layer.as_ref().map_or(
            Err(IrError {
                kind: IrErrorKind::NullScope,
            }),
            |layer| Ok(layer.tag == Tag::Global),
        )
    }
}

struct ScopeLayer {
    tag: Tag,
    symbol_table: HashMap<String, (Value, IrType)>,
    outer: LayerLink,
}

impl ScopeLayer {
    pub fn new(tag: Tag, outer: LayerLink) -> ScopeLayer {
        ScopeLayer {
            tag,
            symbol_table: HashMap::new(),
            outer,
        }
    }

    pub fn declare(
        &mut self,
        symbol: &String,
        data_type: &IrType,
        value: &Value,
    ) -> Result<(), IrError> {
        if self.symbol_table.contains_key(symbol) {
            return Err(IrError {
                kind: IrErrorKind::DuplicateIdentifierInSameScope,
            });
        }
        self.symbol_table
            .insert(symbol.clone(), (value.clone(), data_type.clone()));
        Ok(())
    }

    pub fn lookup(&self, symbol: &String) -> Option<(Value, IrType)> {
        let result = self.symbol_table.get(symbol);
        if result.is_some() {
            return result.cloned();
        }
        match self.outer.as_ref() {
            None => None,
            Some(outer) => outer.lookup(symbol),
        }
    }
}
