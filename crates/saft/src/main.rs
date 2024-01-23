use std::{
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

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    script: Option<PathBuf>,
}

pub struct Saft {
    lowerer: saft_ast_to_ir::Lowerer<bytecode::value::NativeFunction>,
    compiler: bytecode::compiler::Compiler,
    vm: Vm,
    diagnostic_writer: StandardStream,
    diagnostic_config: codespan_reporting::term::Config,
}

#[allow(clippy::new_without_default)]
impl Saft {
    pub fn new() -> Self {
        let mut lowerer = saft_ast_to_ir::Lowerer::new();

        lowerer.add_item("print".into(), saft_ir::Item::Builtin(bytecode::natives::print));

        Self {
            lowerer,
            compiler: bytecode::compiler::Compiler::new(),
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

    fn try_lower_and_compile(
        &mut self,
        fname: &str,
        s: &str,
        module: &ast::Module,
    ) -> Option<bytecode::chunk::Chunk> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        let mut lowerer = self.lowerer.clone();

        let ir = match lowerer.lower_module(module) {
            Ok(ir) => ir,
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer,
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                return None;
            }
        };

        let mut compiler = self.compiler.clone();

        let chunk = match compiler.compile_module(&ir, &lowerer.items) {
            Ok(chunk) => chunk,
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer.lock(),
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                return None;
            }
        };

        self.lowerer = lowerer;
        self.compiler = compiler;

        Some(chunk)
    }

    pub fn try_interpret(&mut self, fname: &str, s: &str, chunk: Rc<Chunk>) -> Option<()> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        let constants = &self.compiler.constants;

        match self.vm.interpret_chunk(chunk, constants) {
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
        let chunk = self.try_lower_and_compile(fname, s, &ast)?;
        self.try_interpret(fname, s, Rc::new(chunk));

        Some(())
    }
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
