use std::{
    fs::{self, create_dir_all, File},
    path::PathBuf,
};

use clap::Parser;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
// use codespan_reporting::files::SimpleFiles;
use platform_dirs::AppDirs;
use rustyline::{error::ReadlineError, DefaultEditor};
use saft_lexer::{lex::Lexer, token::Token};

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
    if !history_file.exists() {
        create_dir_all(history_file.parent().unwrap()).expect("Could not create saft directory");
        File::create(&history_file).expect("Could not create history file");
    }

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
    let mut files = SimpleFiles::new();
    let id = files.add("input", s);

    let mut spanned_tokens = Vec::new();
    let mut lexer = Lexer::new(s);
    loop {
        let next_token = lexer.next_token();
        match &next_token.v {
            Token::Eof => break,
            Token::Unknown => {
                let diag = Diagnostic::error()
                    .with_message(format!(
                        "Failed to tokenize '{}' when scanning for tokens",
                        &s[next_token.s.r.clone()]
                    ))
                    .with_labels(vec![Label::primary(id, next_token.s.r.clone())]);
                let writer = StandardStream::stdout(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag).expect("Could not do stuff");
                return;
            }
            _ => spanned_tokens.push(next_token),
        }
    }
    println!("{:?}", spanned_tokens);
}
