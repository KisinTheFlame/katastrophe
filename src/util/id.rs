#[macro_export]
macro_rules! define_id_generator {
    ($tag:ident$(, $visual:vis)?) => {
        ::paste::paste! {
            static [<$tag:upper _ID_GENERATOR>]: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

            $($visual)* fn [<next_ $tag _id>]() -> u32 {
                [<$tag:upper _ID_GENERATOR>].fetch_add(1, std::sync::atomic::Ordering::Relaxed)
            }

            #[allow(dead_code)]
            $($visual)* fn [<reset_ $tag _id>]() {
                [<$tag:upper _ID_GENERATOR>].store(0, std::sync::atomic::Ordering::Relaxed);
            }
        }
    };
}
