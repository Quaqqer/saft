use std::{
    fs::{self, create_dir_all, File},
    path::PathBuf,
};

use clap::Parser;
use indoc::indoc;
use platform_dirs::AppDirs;
use rustyline::{error::ReadlineError, DefaultEditor};
use saft::Saft;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    script: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let mut saft = Saft::new();

    match args.script {
        Some(path) => {
            if !path.exists() {
                panic!(
                    "Cannot open file `{}`",
                    path.to_str().expect("Path not valid utf-8")
                );
            }

            let s = fs::read_to_string(&path).expect("Could not read file");

            saft.interpret_module(path.file_name().unwrap().to_str().unwrap(), &s);
        }
        None => {
            repl(saft);
        }
    }
}

fn repl(mut saft: Saft) {
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
                if line.starts_with(':') {
                    match line.as_str().trim() {
                        ":h" => println!(indoc! {"
                        :h   - print help
                        :q   - quit"}),
                        ":q" => return,
                        _ => println!("Unknown command, :h for help"),
                    }
                } else {
                    saft.interpret_module("stdin", &line);
                    // saft.interpret_stmt(&line);
                }
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
