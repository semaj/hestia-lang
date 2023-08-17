// use std::fmt;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;

use error::HestiaErr;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::{env, fs};

fn load(evaluator: &mut evaluator::Evaluator, filename: &str) -> Result<(), HestiaErr> {
    let contents = match fs::read_to_string(filename) {
        Ok(s) => Ok(s),
        Err(e) => Err(HestiaErr::Runtime(format!("{}", e))),
    }?;
    let parsed = parser::parse(contents)?;
    for expr in parsed {
        evaluator.eval_top(expr)?;
    }
    Ok(())
}

fn main() -> rustyline::Result<()> {
    let mut evaluator = evaluator::Evaluator::new();
    let args: Vec<String> = env::args().collect();
    if let Some(s) = args.get(1) {
        match load(&mut evaluator, s) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("error loading file {}: {}", s, e);
                return Ok(());
            }
        }
    }
    let filename = "stdlib/list.hea";
    match load(&mut evaluator, filename) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("error loading stdlib file {}: {}", filename, e);
            return Ok(());
        }
    }

    let mut rl = DefaultEditor::new()?;
    if rl.load_history("hestia.history").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("hestia> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                rl.save_history("hestia.history")?;
                match parser::parse(line) {
                    Ok(parsed) => {
                        for expr in parsed {
                            match evaluator.eval_top(expr) {
                                Ok(evaluated) => {
                                    println!("=> {}", evaluated)
                                }
                                Err(e) => {
                                    eprintln!("{}", e);
                                    break;
                                }
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
