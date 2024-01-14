use std::{
    fs::{self, create_dir_all, File},
    path::PathBuf,
};

use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use indoc::indoc;
use platform_dirs::AppDirs;
use rustyline::{error::ReadlineError, DefaultEditor};
use saft_eval::interpreter::Interpreter;

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

            let s = fs::read_to_string(&path).expect("Could not read file");
            let mut interpreter = Interpreter::new();
            interpret_module(
                &mut interpreter,
                path.file_name().unwrap().to_str().unwrap(),
                &s,
            );
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

    let mut interpreter = Interpreter::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line).unwrap();
                if line.starts_with(':') {
                    match line.as_str() {
                        ":h" => println!(indoc! {"
                            :h   - print help
                            :env - print env"}),
                        ":env" => interpreter.print_env(),
                        _ => println!("Unknown command, :h for help"),
                    }
                } else {
                    interpret_stmt(&mut interpreter, &line);
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

fn interpret_stmt(interpreter: &mut Interpreter, s: &str) {
    let mut files = SimpleFiles::new();
    let id = files.add("stdin", s);

    let writer = StandardStream::stdout(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    match saft_parser::Parser::new(s).parse_single_statment() {
        Ok(spanned_stmt) => {
            match saft_ast_to_ir::Lowerer::new().lower_statement(&spanned_stmt) {
                Ok(ir) => {
                    println!("{:#?}", ir.unwrap())
                }
                Err(err) => {
                    term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap()
                }
            }

            match &spanned_stmt.v {
                saft_ast::Statement::Expr(se) => match interpreter.eval_outer_expr(se) {
                    Ok(v) => match v.v {
                        saft_eval::value::Value::Nil => {}
                        v => println!("{}", v.repr()),
                    },
                    Err(err) => {
                        term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id))
                            .unwrap()
                    }
                },

                _ => match interpreter.exec_outer_statement(&spanned_stmt) {
                    Ok(..) => {}
                    Err(err) => {
                        term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id))
                            .unwrap()
                    }
                },
            }
        }
        Err(err) => term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap(),
    };
}

fn interpret_module(interpreter: &mut Interpreter, fname: &str, s: &str) {
    let mut files = SimpleFiles::new();
    let id = files.add(fname, s);

    let writer = StandardStream::stdout(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();
    match saft_parser::Parser::new(s).parse_file() {
        Ok(module) => {
            match interpreter.exec_module(module) {
                Ok(..) => {}
                Err(err) => {
                    term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap()
                }
            };
        }
        Err(err) => term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap(),
    }
}
