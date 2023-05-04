#![feature(once_cell)]

extern crate core;
extern crate miden_diagnostics;
extern crate miden_parsing;
#[macro_use]
extern crate lalrpop_util;

mod ast;
mod lexer;
mod parser;
mod symbols;

// Simple macro used in the grammar definition for constructing spans
macro_rules! span {
    ($l:expr, $r:expr) => {
        miden_diagnostics::SourceSpan::new($l, $r)
    };
    ($i:expr) => {
        miden_diagnostics::SourceSpan::new($i, $i)
    };
}

lalrpop_mod!(grammar, "/grammar.rs");

use self::parser::Parser;

pub fn main() -> Result<(), ()> {
    let mut args = std::env::args();
    args.next()
        .expect("expected executable path as an argument");
    let input = args
        .next()
        .expect("expected an expression to parse as a string");

    let ast = parse(input.as_str())?;
    println!("{:#?}", &ast);

    Ok(())
}

/// Parses the given input string to an [ast::Script]
///
/// If parsing fails, a diagnostic is emitted to stderr.
pub fn parse(input: &str) -> Result<ast::Script, ()> {
    use std::sync::Arc;

    use miden_diagnostics::term::termcolor::ColorChoice;
    use miden_diagnostics::{CodeMap, DefaultEmitter, DiagnosticsHandler};

    let codemap = Arc::new(CodeMap::new());
    let emitter = Arc::new(DefaultEmitter::new(ColorChoice::Auto));
    let diagnostics = DiagnosticsHandler::new(Default::default(), codemap.clone(), emitter);
    let parser = Parser::new((), codemap);
    match parser.parse_string::<ast::Script, _, _>(&diagnostics, input) {
        Ok(ast) => Ok(ast),
        Err(err) => {
            diagnostics.emit(err);
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use miden_diagnostics::{SourceSpan, Span};

    use crate::ast::*;

    use super::parse;

    macro_rules! let_ {
        ($bindings:expr, $body:expr) => {
            Expr::Let(Let {
                span: SourceSpan::UNKNOWN,
                bindings: $bindings,
                body: Box::new($body),
            })
        };
    }

    macro_rules! lambda {
        ($params:expr, $body:expr) => {
            Expr::Lambda(Lambda {
                span: SourceSpan::UNKNOWN,
                params: $params,
                body: Box::new($body),
            })
        };
    }

    macro_rules! bind {
        ($name:expr, $value:expr) => {
            Binding {
                span: SourceSpan::UNKNOWN,
                name: $name,
                value: $value,
            }
        };
    }

    macro_rules! name {
        ($name:ident) => {
            Expr::Var(ident!($name))
        };
    }

    macro_rules! ident {
        ($name:ident) => {
            Ident::from_str(SourceSpan::UNKNOWN, stringify!($name))
        };
    }

    macro_rules! sym {
        ($name:ident) => {
            Expr::Symbol(ident!($name))
        };
    }

    macro_rules! var {
        ($name:ident) => {
            Expr::Var(ident!($name))
        };
    }

    macro_rules! int {
        ($i:literal) => {
            Expr::Lit(Lit::Int(Span::new(SourceSpan::UNKNOWN, $i)))
        };
    }

    macro_rules! add {
        ($lhs:expr, $rhs:expr) => {
            Expr::Op(Op {
                span: SourceSpan::UNKNOWN,
                op: Operator::Add,
                args: vec![$lhs, $rhs],
            })
        };
    }

    macro_rules! mul {
        ($lhs:expr, $rhs:expr) => {
            Expr::Op(Op {
                span: SourceSpan::UNKNOWN,
                op: Operator::Mul,
                args: vec![$lhs, $rhs],
            })
        };
    }

    macro_rules! apply {
        ($callee:expr, $args:expr) => {
            Expr::Apply(Apply {
                span: SourceSpan::UNKNOWN,
                callee: Box::new($callee),
                args: $args,
            })
        };
    }

    macro_rules! cons {
        ($hd:expr, $tl:expr) => {
            Expr::Cons(Cons {
                span: SourceSpan::UNKNOWN,
                hd: Box::new($hd),
                tl: Box::new($tl),
            })
        };
    }

    #[test]
    fn module_test() {
        let input = r#"
        (module test
           (def square (x)
               (* x x))

           (def main ()
               (let [a 1
                     b (square a)]
                 (println b))))

        (main)
        "#;

        let result = parse(input);
        let square_body = mul!(var!(x), var!(x));
        let main_body = let_!(
            vec![
                bind!(ident!(a), int!(1)),
                bind!(ident!(b), apply!(name!(square), vec![var!(a)]))
            ],
            apply!(name!(println), vec![var!(b)])
        );
        let module_name = ident!(test);
        let functions = vec![
            Function {
                span: SourceSpan::UNKNOWN,
                name: FunctionName::new(module_name, ident!(square)),
                params: vec![ident!(x)],
                body: Box::new(square_body),
            },
            Function {
                span: SourceSpan::UNKNOWN,
                name: FunctionName::new(module_name, ident!(main)),
                params: vec![],
                body: Box::new(main_body),
            },
        ];
        let module = Form::Module(Module::new(SourceSpan::UNKNOWN, module_name, functions));
        let expr = Form::Expr(apply!(name!(main), vec![]));
        let expected = Script {
            span: SourceSpan::UNKNOWN,
            forms: vec![module, expr],
        };
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn parse_test() {
        let input = r#"
        (let [foo (+ 1 2)
              square (lambda (x) (* x x))
              bar (square foo)]
              (println (foo . (bar . 'nil))))
        "#;

        let result = parse(input);
        let expr = let_!(
            vec![
                bind!(ident!(foo), add!(int!(1), int!(2))),
                bind!(
                    ident!(square),
                    lambda!(vec![ident!(x)], mul!(var!(x), var!(x)))
                ),
                bind!(ident!(bar), apply!(name!(square), vec![var!(foo)]))
            ],
            apply!(
                name!(println),
                vec![cons!(var!(foo), cons!(var!(bar), sym!(nil)))]
            )
        );
        let expected = Script {
            span: SourceSpan::UNKNOWN,
            forms: vec![Form::Expr(expr)],
        };
        assert_eq!(result, Ok(expected));
    }
}
