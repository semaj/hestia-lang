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
    let mut evaluator = evaluator::Evaluator::new();
    loop {
        let readline = rl.readline("hestia> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                rl.save_history("hestia.history")?;
                match parser::parse(line) {
                    Ok(parsed) => {
                        match evaluator.eval_top(parsed) {
                            Ok(evaluated) => {
                                println!("=> {}", evaluated)
                            }
                            Err(e) => {
                                eprintln!("{}", e)
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("{}", e);
                    }
                }
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
    }
    Ok(())
}
