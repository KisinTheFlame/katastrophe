use const_format::formatcp;

pub const KATASTROPHE: &str = "katastrophe";
pub const KATAS_EXT: &str = ".katas";
pub const SYSTEM_TMP: &str = "/tmp";
pub const TMP: &str = formatcp!("{SYSTEM_TMP}/{KATASTROPHE}");
pub const STD_ROOT: &str = formatcp!("{LIBRARY}");
pub const LIBC_DECLARATION: &str = formatcp!("{LIBRARY}/libc_declaration.ll");

#[cfg(debug_assertions)]
pub const LIBRARY: &str = "library";
#[cfg(not(debug_assertions))]
pub const LIBRARY: &str = "/usr/share/katastrophe";
