use const_format::formatcp;

pub const KATASTROPHE: &str = "katastrophe";
pub const KATAS_EXT: &str = ".katas";
pub const SYSTEM_TMP: &str = "/tmp";
pub const TMP: &str = formatcp!("{SYSTEM_TMP}/{KATASTROPHE}");
