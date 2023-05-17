use std::error::Error;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use miden_diagnostics::*;

use super::{FileMapSource, Source};

/// [Parser] is used to provide a simple interface for implementations
/// of the [Parse] trait.
///
/// It provides a [miden_diagnostics::CodeMap], and an instance of the
/// configuration type used by the underlying [Parse] implementation.
pub struct Parser<C> {
    /// The configuration provided to the parser
    pub config: C,
    /// The underlying [miden_diagnostics::CodeMap] used by the parser
    pub codemap: Arc<CodeMap>,
}
impl<C: Default> Default for Parser<C> {
    fn default() -> Self {
        Self::new(Default::default(), Arc::new(CodeMap::new()))
    }
}
impl<C> Parser<C> {
    /// Create a new [Parser] from the given configuration and [CodeMap]
    pub fn new(config: C, codemap: Arc<CodeMap>) -> Self {
        Self { config, codemap }
    }

    /// Parse a [T] from the given [SourceFile]
    ///
    /// Requires a [DiagnosticsHandler] to be provided for use by the [Parse] implementation
    pub fn parse<T, E>(
        &self,
        diagnostics: &DiagnosticsHandler,
        source: Arc<SourceFile>,
    ) -> Result<T, E>
    where
        E: Error + ToDiagnostic,
        T: Parse<Config = C, Error = E>,
    {
        <T as Parse<T>>::parse(self, diagnostics, FileMapSource::new(source))
    }

    /// Parse a [T] from the given string.
    ///
    /// Requires a [DiagnosticsHandler] to be provided for use by the [Parse] implementation
    pub fn parse_string<T, S, E>(&self, diagnostics: &DiagnosticsHandler, source: S) -> Result<T, E>
    where
        E: Error + ToDiagnostic,
        T: Parse<Config = C, Error = E>,
        S: AsRef<str>,
    {
        let id = self.codemap.add("nofile", source.as_ref().to_string());
        let file = self.codemap.get(id).unwrap();
        self.parse(diagnostics, file)
    }

    /// Parse a [T] from the given file path.
    ///
    /// Requires a [DiagnosticsHandler] to be provided for use by the [Parse] implementation
    pub fn parse_file<T, S, E>(&self, diagnostics: &DiagnosticsHandler, source: S) -> Result<T, E>
    where
        E: Error + ToDiagnostic,
        T: Parse<Config = C, Error = E>,
        S: AsRef<Path>,
    {
        let path = source.as_ref();
        match std::fs::read_to_string(path) {
            Err(err) => Err(<T as Parse<T>>::root_file_error(err, path.to_owned())),
            Ok(content) => {
                let id = self.codemap.add(path, content);
                let file = self.codemap.get(id).unwrap();
                self.parse(diagnostics, file)
            }
        }
    }
}

/// The [Parse] trait abstracts over the common machinery used to parse some type [T].
pub trait Parse<T = Self> {
    /// The concrete type of the parser implementation
    ///
    /// For example, if using LALRPOP, this would correspond to the specific
    /// generated parser type, e.g. `grammar::FooParser`.
    type Parser;
    /// The concrete type of errors which are produced by the parser
    ///
    /// To better interact with our diagnostics infrastructure, it is
    /// required that this type implement [ToDiagnostic].
    type Error: Error + ToDiagnostic;
    /// The concrete type representing the parser configuration.
    ///
    /// For many use cases, no configuration is needed, in which case you should use `()`.
    type Config;
    /// The concrete type of the lexical token consumed by the parser
    ///
    /// For example, if using LALRPOP, this would correspond go the token type used
    /// in the LALRPOP grammar.
    ///
    /// This crate is built under the assumption that you are using a custom lexer
    /// for greater control in the parser, and to associate a [miden_diagnostic::SourceSpan]
    /// to each token produced by the lexer. If you are building a parser without
    /// a lexer, you are better off using the underlying primitives directly as you
    /// see fit.
    type Token;

    /// Constructs an instance of [Self::Error] when a [std:::io::Error] is raised
    ///
    /// This allows us to handle the machinery of reading files from disk without
    /// having to know anything about the specific error type produced by a parser.
    fn root_file_error(err: std::io::Error, path: PathBuf) -> Self::Error;

    /// Parses a [T] from the given [Source].
    ///
    /// Internally, this is expected to construct a token stream from `source`,
    /// typically by constructing a lexer which implements `Iterator` for the
    /// expected token type, and then invoke `parse_tokens` to handle the actual parsing.
    fn parse<S>(
        parser: &Parser<Self::Config>,
        diagnostics: &DiagnosticsHandler,
        source: S,
    ) -> Result<T, Self::Error>
    where
        S: Source;

    /// Parses a [T] from the given token stream.
    ///
    /// If using LALRPOP, this is where you would invoke the generated parser,
    /// passing it the token iterator.
    fn parse_tokens<S>(
        diagnostics: &DiagnosticsHandler,
        codemap: Arc<CodeMap>,
        tokens: S,
    ) -> Result<T, Self::Error>
    where
        S: IntoIterator<Item = Self::Token>;
}
