use std::{fs, path::PathBuf};

use clap::Parser;
use platform_dirs::AppDirs;
use rustyline::{error::ReadlineError, DefaultEditor};
use saft_lexer::lex::Lexer;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    script: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    match args.script {
        Some(path) => {
            if !path.exists() {
                panic!(
                    "Cannot open file `{}`",
                    path.to_str().expect("Path not valid utf-8")
                );
            }

            let s = fs::read_to_string(path).expect("Could not read file");
            interpret(&s);
        }
        None => {
            repl();
        }
    }
}

fn repl() {
    let mut rl = DefaultEditor::new().unwrap();

    let appdirs = AppDirs::new(Some("saft"), false).unwrap();

    let history_file = appdirs.state_dir.join("history.txt");

    let _ = rl.load_history(&history_file);

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line).unwrap();
                interpret(&line);
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history(&history_file)
        .expect("Could not save history");
}

fn interpret(s: &str) {
    let spanned_tokens: Vec<_> = Lexer::new(s).collect();
    println!("{:?}", spanned_tokens);
}
