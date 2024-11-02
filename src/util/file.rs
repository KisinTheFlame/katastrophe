use std::{fs, path::Path};

use rand::{distributions::Alphanumeric, Rng};

use crate::constants::common::TMP;

#[must_use]
pub fn gen_tmp_ir_path() -> String {
    let dir = make_tmp_dir("ir");
    let filename = gen_filename();
    format!("{dir}/{filename}.ll")
}

#[must_use]
pub fn gen_tmp_exe_path() -> String {
    let dir = make_tmp_dir("exe");
    let filename = gen_filename();
    format!("{dir}/{filename}")
}

fn make_tmp_dir(tag: &str) -> String {
    let dir = format!("{TMP}/{tag}");
    if !Path::new(&dir).exists() {
        fs::create_dir_all(dir.clone()).unwrap();
    }
    dir
}

fn gen_filename() -> String {
    rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(40)
        .map(char::from)
        .collect::<String>()
}
