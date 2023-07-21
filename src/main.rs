// use std::fmt;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

fn main() -> Result<()> {
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    if rl.load_history("hestia.history").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("hestia> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                println!("=> {}", evaluator::evaluate(line).unwrap());
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
        rl.save_history("hestia.history")?;
    }
    Ok(())
}
