use core::fmt;
use core::mem;
use core::num::IntErrorKind;

use miden_diagnostics::*;
use miden_parsing::{Scanner, Source};

use crate::parser::ParseError;
use crate::symbols::Symbol;

/// The value produced by the Lexer when iterated
pub type Lexed = Result<(SourceIndex, Token, SourceIndex), ParseError>;

/// Errors that may occur during lexing of the source
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum LexicalError {
    #[error("invalid integer value: {reason:?}")]
    InvalidInt {
        span: SourceSpan,
        reason: IntErrorKind,
    },
    #[error("encountered unexpected character '{found}'")]
    UnexpectedCharacter { start: SourceIndex, found: char },
}
impl ToDiagnostic for LexicalError {
    fn to_diagnostic(self) -> Diagnostic {
        match self {
            Self::InvalidInt { span, reason } => Diagnostic::error()
                .with_message("invalid integer literal")
                .with_labels(vec![
                    Label::primary(span.source_id(), span).with_message(format!("{:?}", reason))
                ]),
            Self::UnexpectedCharacter { start, .. } => Diagnostic::error()
                .with_message("unexpected character")
                .with_labels(vec![Label::primary(
                    start.source_id(),
                    SourceSpan::new(start, start),
                )]),
        }
    }
}

/// Lexical tokens recognized by the parser
#[derive(Debug, Clone)]
pub enum Token {
    // Signifies end of input
    EOF,
    // A tokenization error which may be recovered from
    Error(LexicalError),
    Int(i64),
    Ident(Symbol),
    LParen,
    RParen,
    LBracket,
    RBracket,
    Quote,
    Dot,
    Colon,
    Plus,
    Minus,
    Star,
    Slash,
    Module,
    Def,
    Let,
    Lambda,
    Hd,
    Tl,
    If,
}
impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match self {
            Self::Int(i) => {
                if let Self::Int(i2) = other {
                    return *i == *i2;
                }
            }
            Self::Error(_) => {
                if let Self::Error(_) = other {
                    return true;
                }
            }
            Self::Ident(i) => {
                if let Self::Ident(i2) = other {
                    return i == i2;
                }
            }
            _ => return mem::discriminant(self) == mem::discriminant(other),
        }
        return false;
    }
}
impl Eq for Token {}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::EOF => write!(f, "EOF"),
            Self::Error(_) => write!(f, "ERROR"),
            // Literals
            Self::Int(ref i) => write!(f, "{}", i),
            Self::Ident(ref s) => write!(f, "{}", s),
            // Punctuation
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::Quote => write!(f, "'"),
            Self::Dot => write!(f, "."),
            Self::Colon => write!(f, ":"),
            // Keywords
            Self::Lambda => write!(f, "lambda"),
            Self::Let => write!(f, "let"),
            Self::Module => write!(f, "module"),
            Self::Def => write!(f, "def"),
            // Operators
            Self::Star => write!(f, "*"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Slash => write!(f, "/"),
            Self::Hd => write!(f, "hd"),
            Self::Tl => write!(f, "tl"),
            Self::If => write!(f, "if"),
        }
    }
}

pub struct Lexer<S> {
    /// The scanner produces a sequence of chars + location, and can be controlled
    /// The location produces is a SourceIndex
    scanner: Scanner<S>,

    /// The most recent token to be lexed.
    /// At the start and end, this should be Token::EOF
    token: Token,

    /// The position in the input where the current token starts
    /// At the start this will be the byte index of the beginning of the input
    token_start: SourceIndex,

    /// The position in the input where the current token ends
    /// At the start this will be the byte index of the beginning of the input
    token_end: SourceIndex,

    /// When we have reached true EOF, this gets set to true, and the only token
    /// produced after that point is Token::EOF, or None, depending on how you are
    /// consuming the lexer
    eof: bool,
}

macro_rules! pop {
    ($lex:ident) => {{
        $lex.skip();
    }};
    ($lex:ident, $code:expr) => {{
        $lex.skip();
        $code
    }};
}

impl<S> Lexer<S>
where
    S: Source,
{
    /// Produces an instance of the lexer with the lexical analysis to be performed on the `input`
    /// string. Note that no lexical analysis occurs until the lexer has been iterated over.
    pub fn new(scanner: Scanner<S>) -> Self {
        let start = scanner.start();
        let mut lexer = Lexer {
            scanner,
            token: Token::EOF,
            token_start: start + ByteOffset(0),
            token_end: start + ByteOffset(0),
            eof: false,
        };
        lexer.advance();
        lexer
    }

    pub fn lex(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.eof && self.token == Token::EOF {
            return None;
        }

        let token = mem::replace(&mut self.token, Token::EOF);
        self.advance();
        match token {
            Token::Error(err) => Some(Err(err.into())),
            token => Some(Ok((self.token_start, token, self.token_end))),
        }
    }

    fn advance(&mut self) {
        self.advance_start();
        self.token = self.tokenize();
    }

    #[inline]
    fn advance_start(&mut self) {
        let mut position: SourceIndex;
        loop {
            let (pos, c) = self.scanner.read();

            position = pos;

            if c == '\0' {
                self.eof = true;
                return;
            }

            if c.is_whitespace() {
                self.scanner.advance();
                continue;
            }

            break;
        }

        self.token_start = position;
    }

    #[inline]
    fn pop(&mut self) -> char {
        let (pos, c) = self.scanner.pop();
        self.token_end = pos + ByteOffset::from_char_len(c);
        c
    }

    #[inline]
    fn read(&mut self) -> char {
        let (_, c) = self.scanner.read();
        c
    }

    #[inline]
    fn skip(&mut self) {
        self.pop();
    }

    /// Get the span for the current token in `Source`.
    #[inline]
    pub fn span(&self) -> SourceSpan {
        SourceSpan::new(self.token_start, self.token_end)
    }

    /// Get a string slice of the current token.
    #[inline]
    fn slice(&self) -> &str {
        self.scanner.slice(self.span())
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        let mut c: char;
        loop {
            c = self.read();

            if !c.is_whitespace() {
                break;
            }

            self.skip();
        }
    }

    fn tokenize(&mut self) -> Token {
        let c = self.read();

        if c == '\0' {
            self.eof = true;
            return Token::EOF;
        }

        if c.is_whitespace() {
            self.skip_whitespace();
        }

        match self.read() {
            '*' => pop!(self, Token::Star),
            '+' => pop!(self, Token::Plus),
            '-' => pop!(self, Token::Minus),
            '/' => pop!(self, Token::Slash),
            '[' => pop!(self, Token::LBracket),
            ']' => pop!(self, Token::RBracket),
            '(' => pop!(self, Token::LParen),
            ')' => pop!(self, Token::RParen),
            '.' => pop!(self, Token::Dot),
            ':' => pop!(self, Token::Colon),
            '\'' => pop!(self, Token::Quote),
            '0'..='9' => self.lex_number(),
            'a'..='z' => self.lex_keyword_or_identifier(),
            'A'..='Z' => self.lex_identifier(),
            c => Token::Error(LexicalError::UnexpectedCharacter {
                start: self.span().start(),
                found: c,
            }),
        }
    }

    #[inline]
    fn lex_identifier(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c == '_' || c.is_ascii_alphabetic());

        loop {
            match self.read() {
                '_' | '!' | '?' => self.skip(),
                '0'..='9' => self.skip(),
                c if c.is_alphabetic() => self.skip(),
                _ => break,
            }
        }
        Token::Ident(Symbol::intern(self.slice()))
    }

    #[inline]
    fn lex_keyword_or_identifier(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c.is_ascii_lowercase());

        loop {
            match self.read() {
                '_' | '!' | '?' => self.skip(),
                '0'..='9' => self.skip(),
                c if c.is_alphabetic() => self.skip(),
                _ => break,
            }
        }

        match self.slice() {
            "let" => Token::Let,
            "lambda" => Token::Lambda,
            "module" => Token::Module,
            "def" => Token::Def,
            "hd" => Token::Hd,
            "tl" => Token::Tl,
            "if" => Token::If,
            other => Token::Ident(Symbol::intern(other)),
        }
    }

    #[inline]
    fn lex_number(&mut self) -> Token {
        let mut num = String::new();
        let mut c;

        // Expect the first character to be a base-10 digit
        c = self.read();
        debug_assert!(c.is_digit(10), "got {}", c);

        loop {
            match c {
                c if c.is_digit(10) => {
                    num.push(self.pop());
                }
                _ => break,
            }
            c = self.read();
        }

        match i64::from_str_radix(&num, 10) {
            Ok(i) => Token::Int(i),
            Err(err) => Token::Error(LexicalError::InvalidInt {
                span: self.span(),
                reason: err.kind().clone(),
            }),
        }
    }
}

impl<S> Iterator for Lexer<S>
where
    S: Source,
{
    type Item = Lexed;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.lex().map(|res| res.map_err(ParseError::from))
    }
}
