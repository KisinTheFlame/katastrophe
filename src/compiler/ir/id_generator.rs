use std::sync::atomic::{AtomicU32, Ordering};

macro_rules! define_id_generator {
    ($tag:ident) => {
        ::paste::paste! {
            static [<$tag:upper _ID_GENERATOR>]: AtomicU32 = AtomicU32::new(0);

            pub fn [<next_ $tag _id>]() -> u32 {
                [<$tag:upper _ID_GENERATOR>].fetch_add(1, Ordering::Relaxed)
            }

            #[allow(dead_code)]
            pub fn [<reset_ $tag _id>]() {
                [<$tag:upper _ID_GENERATOR>].store(0, Ordering::Relaxed);
            }
        }
    };
}

define_id_generator!(anonymous);
define_id_generator!(parameter);
define_id_generator!(mutable);
define_id_generator!(global);
define_id_generator!(function);
define_id_generator!(label);
