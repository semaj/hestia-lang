use hestia_lib::*;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::{env, fs};

fn load(evaluator: &mut evaluator::Evaluator, filename: &str) -> Result<(), error::HestiaErr> {
    let contents = match fs::read_to_string(filename) {
        Ok(s) => Ok(s),
        Err(e) => Err(error::HestiaErr::Runtime(format!("{}", e))),
    }?;
    let parsed = parser::parse(contents)?;
    for expr in parsed {
        evaluator.eval_top(expr)?;
    }
    Ok(())
}

fn main() -> rustyline::Result<()> {
    let mut evaluator = match evaluator::Evaluator::loaded() {
        Ok(evalr) => evalr,
        Err(e) => {
            eprintln!("failed to load stdlib: {}", e);
            return Ok(());
        }
    };
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

    let mut rl = DefaultEditor::new()?;
    if rl.load_history("hestia.history").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("hestia> ");
        match readline {
            Ok(line) => {
                if line == "exit" || line == "quit" {
                    break;
                }
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
