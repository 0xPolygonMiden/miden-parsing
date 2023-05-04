use std::sync::Arc;

use miden_diagnostics::{
    CodeMap, Diagnostic, DiagnosticsHandler, SourceIndex, SourceSpan, ToDiagnostic,
};
use miden_parsing::{Scanner, Source};

use crate::ast;
use crate::grammar;
use crate::lexer::{Lexed, Lexer, LexicalError, Token};

pub type Parser = miden_parsing::Parser<()>;

impl miden_parsing::Parse for ast::Script {
    type Parser = grammar::ScriptParser;
    type Error = ParseError;
    type Config = ();
    type Token = Lexed;

    fn root_file_error(source: std::io::Error, path: std::path::PathBuf) -> Self::Error {
        ParseError::FileError { source, path }
    }

    fn parse<S>(
        parser: &Parser,
        diagnostics: &DiagnosticsHandler,
        source: S,
    ) -> Result<Self, Self::Error>
    where
        S: Source,
    {
        let scanner = Scanner::new(source);
        let lexer = Lexer::new(scanner);
        Self::parse_tokens(diagnostics, parser.codemap.clone(), lexer)
    }

    fn parse_tokens<S: IntoIterator<Item = Lexed>>(
        diagnostics: &DiagnosticsHandler,
        codemap: Arc<CodeMap>,
        tokens: S,
    ) -> Result<Self, Self::Error> {
        let result = Self::Parser::new().parse(diagnostics, &codemap, tokens);
        match result {
            Ok(ast) => Ok(ast),
            Err(lalrpop_util::ParseError::User { error }) => Err(error.into()),
            Err(err) => Err(err.into()),
        }
    }
}

impl miden_parsing::Parse for ast::Expr {
    type Parser = grammar::ExprParser;
    type Error = ParseError;
    type Config = ();
    type Token = Lexed;

    fn root_file_error(source: std::io::Error, path: std::path::PathBuf) -> Self::Error {
        ParseError::FileError { source, path }
    }

    fn parse<S>(
        parser: &Parser,
        diagnostics: &DiagnosticsHandler,
        source: S,
    ) -> Result<Self, Self::Error>
    where
        S: Source,
    {
        let scanner = Scanner::new(source);
        let lexer = Lexer::new(scanner);
        Self::parse_tokens(diagnostics, parser.codemap.clone(), lexer)
    }

    fn parse_tokens<S: IntoIterator<Item = Lexed>>(
        diagnostics: &DiagnosticsHandler,
        codemap: Arc<CodeMap>,
        tokens: S,
    ) -> Result<Self, Self::Error> {
        let result = Self::Parser::new().parse(diagnostics, &codemap, tokens);
        match result {
            Ok(ast) => Ok(ast),
            Err(lalrpop_util::ParseError::User { error }) => Err(error.into()),
            Err(err) => Err(err.into()),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error(transparent)]
    Lexer(#[from] LexicalError),
    #[error("invalid token")]
    InvalidToken(SourceIndex),
    #[error("unexpected end of file")]
    UnexpectedEof {
        at: SourceIndex,
        expected: Vec<String>,
    },
    #[error("unrecognized token '{token}'")]
    UnrecognizedToken {
        span: SourceSpan,
        token: Token,
        expected: Vec<String>,
    },
    #[error("extraneous token '{token}'")]
    ExtraToken { span: SourceSpan, token: Token },
    #[error("error reading {path:?}: {source}")]
    FileError {
        source: std::io::Error,
        path: std::path::PathBuf,
    },
}
impl From<lalrpop_util::ParseError<SourceIndex, Token, ParseError>> for ParseError {
    fn from(err: lalrpop_util::ParseError<SourceIndex, Token, ParseError>) -> Self {
        use lalrpop_util::ParseError as LError;

        match err {
            LError::InvalidToken { location } => Self::InvalidToken(location),
            LError::UnrecognizedEOF {
                location: at,
                expected,
            } => Self::UnexpectedEof { at, expected },
            LError::UnrecognizedToken {
                token: (l, token, r),
                expected,
            } => Self::UnrecognizedToken {
                span: SourceSpan::new(l, r),
                token,
                expected,
            },
            LError::ExtraToken {
                token: (l, token, r),
            } => Self::ExtraToken {
                span: SourceSpan::new(l, r),
                token,
            },
            LError::User { error } => error,
        }
    }
}
impl ToDiagnostic for ParseError {
    fn to_diagnostic(self) -> Diagnostic {
        use miden_diagnostics::Label;

        match self {
            Self::FileError { .. } => Diagnostic::error().with_message(self.to_string()),
            Self::Lexer(err) => err.to_diagnostic(),
            Self::InvalidToken(start) => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary(
                    start.source_id(),
                    SourceSpan::new(start, start),
                )]),
            Self::UnexpectedEof { at, ref expected } => {
                let mut message = "expected one of: ".to_string();
                for (i, t) in expected.iter().enumerate() {
                    if i == 0 {
                        message.push_str(&format!("'{}'", t));
                    } else {
                        message.push_str(&format!(", '{}'", t));
                    }
                }

                Diagnostic::error()
                    .with_message("unexpected eof")
                    .with_labels(vec![Label::primary(
                        at.source_id(),
                        SourceSpan::new(at, at),
                    )
                    .with_message(message)])
            }
            Self::UnrecognizedToken {
                span, ref expected, ..
            } => {
                let mut message = "expected one of: ".to_string();
                for (i, t) in expected.iter().enumerate() {
                    if i == 0 {
                        message.push_str(&format!("'{}'", t));
                    } else {
                        message.push_str(&format!(", '{}'", t));
                    }
                }

                Diagnostic::error()
                    .with_message("unexpected token")
                    .with_labels(vec![
                        Label::primary(span.source_id(), span).with_message(message)
                    ])
            }
            Self::ExtraToken { span, .. } => Diagnostic::error()
                .with_message("extraneous token")
                .with_labels(vec![Label::primary(span.source_id(), span)]),
        }
    }
}
