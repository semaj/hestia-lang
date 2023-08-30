#[macro_use]
extern crate lazy_static;
use hestia_lib::*;
use std::sync::Mutex;
use wasm_bindgen::prelude::*;

lazy_static! {
    static ref EVALUATOR: Mutex<evaluator::Evaluator> =
        Mutex::new(evaluator::Evaluator::loaded().unwrap());
}

// #[wasm_bindgen]
// extern "C" {
//     pub fn alert(s: &str);
// }

#[wasm_bindgen]
pub fn ev(line: &str) -> String {
    let mut evalr = EVALUATOR.lock().unwrap();
    match parser::parse(line.to_string()) {
        Ok(parsed) => {
            let mut v = Vec::new();
            for expr in parsed {
                match evalr.eval_top(expr) {
                    Ok(evaluated) => {
                        v.push(format!("{}", evaluated));
                    }
                    Err(e) => {
                        return format!("{}", e);
                    }
                }
            }
            v.join("\n")
        }
        Err(e) => {
            format!("{}", e)
        }
    }
}
