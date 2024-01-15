use std::{
    fs::{self, create_dir_all, File},
    path::PathBuf,
    rc::Rc,
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
use saft_bytecode::vm::Vm;

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
            interpret_module(path.file_name().unwrap().to_str().unwrap(), &s);
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
                if line.starts_with(':') {
                    match line.as_str() {
                        ":h" => println!(indoc! {"
                        :h   - print help"}),
                        _ => println!("Unknown command, :h for help"),
                    }
                } else {
                    interpret_stmt(&line);
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

fn interpret_stmt(s: &str) {
    let mut files = SimpleFiles::new();
    let id = files.add("stdin", s);

    let writer = StandardStream::stdout(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let stmt = match saft_parser::Parser::new(s).parse_single_statment() {
        Ok(stmt) => stmt,
        Err(err) => {
            term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
            return;
        }
    };

    let stmt = match saft_ast_to_ir::Lowerer::new().lower_statement(&stmt) {
        Ok(ir) => ir.unwrap(),
        Err(err) => {
            term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
            return;
        }
    };

    match &stmt.v {
        saft_ir::Stmt::Expr(expr) => {
            let chunk = match saft_bytecode::compiler::Compiler::new().compile_expr(expr) {
                Ok(chunk) => chunk,
                Err(err) => {
                    term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
                    return;
                }
            };

            let mut vm = Vm::new(vec![]);
            match vm.interpret_expr(Rc::new(chunk)) {
                Ok(val) => println!("{}", val.repr()),
                Err(err) => {
                    term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
                }
            };
        }
        _ => {
            let chunk = match saft_bytecode::compiler::Compiler::new().compile_stmt(&stmt) {
                Ok(chunk) => chunk,
                Err(err) => {
                    term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
                    return;
                }
            };

            let mut vm = Vm::new(vec![]);
            match vm.interpret_chunk(Rc::new(chunk)) {
                Ok(()) => {}
                Err(err) => {
                    term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
                }
            };
        }
    }
}

fn interpret_module(fname: &str, s: &str) {
    let mut files = SimpleFiles::new();
    let id = files.add(fname, s);

    let writer = StandardStream::stdout(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let module = match saft_parser::Parser::new(s).parse_file() {
        Ok(module) => module,
        Err(err) => {
            term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
            return;
        }
    };

    let module = match saft_ast_to_ir::Lowerer::new().lower_module(&module) {
        Ok(ir) => ir,
        Err(err) => {
            term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
            return;
        }
    };

    let chunk = match saft_bytecode::compiler::Compiler::new().compile_module(&module) {
        Ok(chunk) => chunk,
        Err(err) => {
            term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
            return;
        }
    };

    let mut vm = Vm::new(vec![]);
    match vm.interpret_chunk(Rc::new(chunk)) {
        Ok(()) => {}
        Err(err) => {
            term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
        }
    };
}
