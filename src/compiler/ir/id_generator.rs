use std::sync::atomic::{AtomicU32, Ordering};

static ATOMIC: AtomicU32 = AtomicU32::new(0);

pub fn next_id() -> u32 {
    ATOMIC.fetch_add(1, Ordering::Relaxed)
}
