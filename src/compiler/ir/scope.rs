use std::collections::HashMap;

use super::{
    err::{IrError, IrErrorKind},
    id_generator::next_id,
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

    pub fn declare_anonymous(&self) -> u32 {
        next_id()
    }

    pub fn declare_symbol(&mut self, symbol: &String) -> Result<u32, IrError> {
        self.current_layer.as_mut().map_or(
            Err(IrError {
                kind: IrErrorKind::NullScope,
            }),
            |layer| layer.declare_symbol(symbol),
        )
    }

    pub fn lookup_symbol(&self, symbol: &String) -> Result<Option<u32>, IrError> {
        self.current_layer.as_ref().map_or(
            Err(IrError {
                kind: IrErrorKind::NullScope,
            }),
            |layer| Ok(layer.lookup_symbol(symbol)),
        )
    }

    pub fn exist_symbol(&self, symbol: &String) -> Result<bool, IrError> {
        Ok(self.lookup_symbol(symbol)?.is_some())
    }

    // pub fn is_global(&self) -> Result<bool, IrError> {
    //     self.current_layer.as_ref().map_or(
    //         Err(IrError {
    //             kind: IrErrorKind::NullScope,
    //         }),
    //         |layer| Ok(layer.tag == ScopeTag::Global),
    //     )
    // }
}

struct ScopeLayer {
    tag: Tag,
    symbol_table: HashMap<String, u32>,
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

    pub fn declare_symbol(&mut self, symbol: &String) -> Result<u32, IrError> {
        if self.symbol_table.contains_key(symbol) {
            return Err(IrError {
                kind: IrErrorKind::DuplicateIdentifierInSameScope,
            });
        }
        let id = next_id();
        self.symbol_table.insert(symbol.clone(), id);
        Ok(id)
    }

    pub fn lookup_symbol(&self, symbol: &String) -> Option<u32> {
        let result = self.symbol_table.get(symbol);
        if result.is_some() {
            return result.copied();
        }
        match self.outer.as_ref() {
            None => None,
            Some(outer) => outer.lookup_symbol(symbol),
        }
    }
}
