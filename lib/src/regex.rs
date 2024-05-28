pub mod lib;
use crate::error::HestiaErr;
use crate::regex::lib::eval;

pub fn evaluate(to_match: &str) -> Result<bool, HestiaErr> {
    eval(to_match)
}
