use std::rc::Rc;

use bytecode::natives;
use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use saft_bytecode::{chunk::Chunk, vm::Vm};

use saft_syntax::{ast, parser::Parser};
use saft_ir::ir;
use saft_ir::lowerer::Lowerer;
use saft_bytecode as bytecode;

pub struct Saft {
    lowerer: Lowerer<bytecode::value::NativeFunction>,
    compiler: bytecode::compiler::Compiler,
    vm: Vm,
    diagnostic_writer: StandardStream,
    diagnostic_config: codespan_reporting::term::Config,
}

#[allow(clippy::new_without_default)]
impl Saft {
    pub fn new() -> Self {
        let mut lowerer = Lowerer::new();

        let mut add_native = |native: bytecode::value::NativeFunction| {
            lowerer.add_item(native.name.to_string(), ir::Item::Builtin(native));
        };

        add_native(natives::print);

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

        match Parser::new(s).parse_file() {
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
