use std::ops::Range;

use miden_diagnostics::*;

use super::Source;

/// [Scanner] handles the low-level details of reading characters
/// from a raw input stream of bytes. It decodes those bytes into
/// UTF-8 characters, and associates each character with the [SourceIndex]
/// at which it occurs.
///
/// The [Scanner] is intended to be consumed by a lexer, which handles
/// converting the stream of characters into a token stream for use
/// by the parser.
///
/// ## Scanner Lifecycle
///
/// The following illustrates how content flows from the raw input
/// stream through the scanner.
///
/// ```ignore
/// lexer <- (peek) <- pending <- source
///       <- (pop) <- current <- pending <- source
/// ```
///
/// As shown above, the lexer is "pulling" characters from the scanner.
///
/// When "peeking" a character, we return the character currently in the
/// `pending` field, but if `pending` is empty, we read enough bytes from
/// the source to construct a UTF-8 character, and store it as `pending`,
/// as well as returning it to the lexer.
///
/// When "popping" a character (i.e. we are advancing the scanner in the
/// input), we are returing the character in the `current` field, and then
/// moving the character in `pending` into `current`. Accordingly, if any
/// of those fields is empty, we must pull from the next field in the chain,
/// reading bytes from the input as we go.
pub struct Scanner<S> {
    source: S,
    current: (SourceIndex, char),
    pending: (SourceIndex, char),
    start: SourceIndex,
    end: SourceIndex,
}
impl<S> Scanner<S>
where
    S: Source,
{
    /// Construct a new [Scanner] for the given `source`
    pub fn new(mut source: S) -> Self {
        let span = source.span();
        let start = span.start();
        let end = span.end();
        let current = source.read().unwrap_or((SourceIndex::UNKNOWN, '\0'));
        let pending = source.read().unwrap_or((SourceIndex::UNKNOWN, '\0'));
        Scanner {
            source,
            current,
            pending,
            start,
            end,
        }
    }

    /// Returns a [SourceIndex] representing the start of the source
    pub fn start(&self) -> SourceIndex {
        self.start
    }

    /// Advance scanner pipeline by a single character.
    ///
    /// `pending` becomes `current`, and bytes are read from the input
    /// to repopulate `pending`.
    #[inline]
    pub fn advance(&mut self) {
        self.current = self.pending;
        self.pending = match self.source.read() {
            None => (self.end, '\0'),
            Some(ic) => ic,
        };
    }

    /// Return the current character and advance our position in the source
    #[inline]
    pub fn pop(&mut self) -> (SourceIndex, char) {
        let current = self.current;
        self.advance();
        current
    }

    /// Return the next character in the input, but do not advance.
    #[inline]
    pub fn peek(&self) -> (SourceIndex, char) {
        self.pending
    }

    /// Return the character after the next character in the input, but do not advance.
    #[inline]
    pub fn peek_next(&mut self) -> (SourceIndex, char) {
        match self.source.peek() {
            None => (self.end, '\0'),
            Some((pos, c)) => (pos, c),
        }
    }

    /// Get current character in the input.
    #[inline]
    pub fn read(&self) -> (SourceIndex, char) {
        self.current
    }

    /// Get a string slice representing the given range in the underlying source
    #[inline]
    pub fn slice(&self, span: impl Into<Range<usize>>) -> &str {
        self.source.slice(span)
    }
}
