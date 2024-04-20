use std::sync::atomic::{AtomicU32, Ordering};

use crate::define_id_generator;

define_id_generator!(anonymous, pub);
define_id_generator!(parameter, pub);
define_id_generator!(mutable, pub);
define_id_generator!(global, pub);
define_id_generator!(function, pub);
define_id_generator!(label, pub);
