# miden-parsing

This crate provides additional infrastructure on top of [miden-diagnostics](https://github.com/0xPolygonMiden/miden-diagnostics) 
for parsing within a compiler frontend.

## Features

The following are features provided by this crate:

* An abstraction called `Source` that provides the core operations required by the scanner component of a parser,
along with a "standard" implementation called `FileMapSource` which builds on the `SourceFile` type provided by
`miden-diagnostics`
* `Scanner`, a low-level component intended for consumption from a lexer. At a high level, the scanner 
operates on the input source to allow a lexer to precisely control what characters in the source are part
of the current token, the ability to get a slice associated with the current token, as well as the ability
to control advancement of the underlying source stream (i.e. get the current char, pop the current char, peek
the next char, etc).
* `Parse<T>` a trait which describes what is needed to parse a `T`, and exposes the API for doing so
* `Parser<C>` represents a parser with a configuration type `C` that is capable of parsing any compatible `Parse` implementation.
This parser is designed for use with components provided by `miden-diagnostics`, specifically `CodeMap` and
`DiagnosticsHandler`. It abstracts away some of the boilerplate involved with common parsing tasks, e.g.
`parse_file` and `parse_string`.


This crate is known to work well with LALRPOP, and only requires that a compiler frontend provide a lexer
and token type specific to the language being parsed. See the `examples` folder for a complete end-to-end
demonstration of how to use this crate (and `miden-diagnostics`) with LALRPOP for a simple language.
