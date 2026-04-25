use std::fs;
use std::path::Path;

use rand::Rng;
use rand::distributions::Alphanumeric;

use crate::CompileResult;
use crate::compiler::err::CompileError;
use crate::constants::common::TMP;

/// # Errors
pub fn gen_tmp_ir_path() -> CompileResult<String> {
    let dir = make_tmp_dir("ir")?;
    let filename = gen_filename();
    Ok(format!("{dir}/{filename}.ll"))
}

/// # Errors
pub fn gen_tmp_exe_path() -> CompileResult<String> {
    let dir = make_tmp_dir("exe")?;
    let filename = gen_filename();
    Ok(format!("{dir}/{filename}"))
}

fn make_tmp_dir(tag: &str) -> CompileResult<String> {
    let dir = format!("{TMP}/{tag}");
    if !Path::new(&dir).exists() {
        fs::create_dir_all(&dir).map_err(|error| CompileError::FileWriteFailed {
            path: dir.clone(),
            error: error.to_string(),
        })?;
    }
    Ok(dir)
}

fn gen_filename() -> String {
    rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(40)
        .map(char::from)
        .collect::<String>()
}
