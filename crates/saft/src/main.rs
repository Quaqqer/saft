use std::{
    collections::HashMap,
    fs::{self, create_dir_all, File},
    path::PathBuf,
    rc::Rc,
};

use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use indoc::indoc;
use platform_dirs::AppDirs;
use rustyline::{error::ReadlineError, DefaultEditor};
use saft_bytecode::{chunk::Chunk, vm::Vm};

use saft_ast as ast;
use saft_bytecode as bytecode;
use saft_ir as ir;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    script: Option<PathBuf>,
}

pub struct Saft {
    var_counter: usize,
    item_counter: usize,
    outer_scope: HashMap<String, saft_ir::Ref>,

    vm: Vm,
    diagnostic_writer: StandardStream,
    diagnostic_config: codespan_reporting::term::Config,
}

#[allow(clippy::new_without_default)]
impl Saft {
    pub fn new() -> Self {
        Self {
            var_counter: 0,
            item_counter: 0,
            outer_scope: HashMap::new(),
            vm: Vm::new(),
            diagnostic_writer: codespan_reporting::term::termcolor::StandardStream::stdout(
                codespan_reporting::term::termcolor::ColorChoice::Auto,
            ),
            diagnostic_config: codespan_reporting::term::Config::default(),
        }
    }

    fn try_parse(&mut self, fname: &str, s: &str) -> Option<ast::Module> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        match saft_parser::Parser::new(s).parse_file() {
            Ok(module) => Some(module),
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer,
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                None
            }
        }
    }

    fn try_lower(
        &mut self,
        fname: &str,
        s: &str,
        module: &ast::Module,
    ) -> Option<ir::Module<bytecode::item::NativeFunction>> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        match saft_ast_to_ir::Lowerer::new().lower_module(module) {
            Ok(ir) => Some(ir),
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer,
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                None
            }
        }
    }

    fn try_compile(
        &mut self,
        fname: &str,
        s: &str,
        module: &ir::Module<bytecode::item::NativeFunction>,
    ) -> Option<(bytecode::chunk::Chunk, Vec<bytecode::item::Item>)> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        match saft_bytecode::compiler::Compiler::new().compile_module(module) {
            Ok((chunk, items)) => Some((chunk, items)),
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer.lock(),
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                None
            }
        }
    }

    pub fn try_interpret(&mut self, fname: &str, s: &str, chunk: Rc<Chunk>) -> Option<()> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        match self.vm.interpret_chunk(chunk) {
            Ok(()) => Some(()),
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer,
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                None
            }
        }
    }

    pub fn interpret_module(&mut self, fname: &str, s: &str) -> Option<()> {
        let ast = self.try_parse(fname, s)?;
        let ir = self.try_lower(fname, s, &ast)?;
        let (chunk, items) = self.try_compile(fname, s, &ir)?;

        self.vm.add_items(items);
        self.try_interpret(fname, s, Rc::new(chunk))?;

        Some(())
    }

    // fn interpret_stmt(&mut self, s: &str) {
    //     let mut files = SimpleFiles::new();
    //     let id = files.add("stdin", s);
    //
    //     let writer = StandardStream::stdout(ColorChoice::Auto);
    //     let config = codespan_reporting::term::Config::default();
    //
    //     let stmt = match saft_parser::Parser::new(s).parse_single_statment() {
    //         Ok(stmt) => stmt,
    //         Err(err) => {
    //             term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
    //             return;
    //         }
    //     };
    //
    //     let stmt = match saft_ast_to_ir::Lowerer::new().lower_statement(&stmt) {
    //         Ok(ir) => ir.unwrap(),
    //         Err(err) => {
    //             term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id)).unwrap();
    //             return;
    //         }
    //     };
    //
    //     match &stmt.v {
    //         saft_ir::Stmt::Expr(expr) => {
    //             let chunk = match saft_bytecode::compiler::Compiler::new().compile_expr(expr) {
    //                 Ok(chunk) => chunk,
    //                 Err(err) => {
    //                     term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id))
    //                         .unwrap();
    //                     return;
    //                 }
    //             };
    //
    //             match self.vm.interpret_expr(Rc::new(chunk)) {
    //                 Ok(val) => println!("{}", val.repr()),
    //                 Err(err) => {
    //                     term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id))
    //                         .unwrap();
    //                 }
    //             };
    //         }
    //         _ => {
    //             let chunk = match saft_bytecode::compiler::Compiler::new().compile_stmt(&stmt) {
    //                 Ok(chunk) => chunk,
    //                 Err(err) => {
    //                     term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id))
    //                         .unwrap();
    //                     return;
    //                 }
    //             };
    //
    //             match self.vm.interpret_chunk(Rc::new(chunk)) {
    //                 Ok(()) => {}
    //                 Err(err) => {
    //                     term::emit(&mut writer.lock(), &config, &files, &err.diagnostic(id))
    //                         .unwrap();
    //                 }
    //             };
    //
    //             let stack = self.vm.get_stack();
    //             if stack.is_empty() {
    //                 eprintln!(
    //                     "Stack was not zero after execution, something has gone wrong...: {:?}",
    //                     stack
    //                 );
    //             }
    //         }
    //     }
    // }
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
