# just-mode.el - Emacs mode for justfiles

This Emacs mode does basic highlighting and indentation for justfiles, as used
by [just](https://github.com/casey/just).

NOTE: The Tree-sitter version of this package,
[just-ts-mode](https://github.com/leon-barrett/just-ts-mode.el), has more
reliable syntax highlighting, and you may prefer that.

# Installation

`just-mode` can be installed through [MELPA](https://melpa.org/#/just-mode).

# Usage

Edit a file named `justfile`. It will have highlighting and indentation.

# Comparison to `just-ts-mode`

There is another major mode for Justfiles based on tree-sitter,
called [just-ts-mode](https://stable.melpa.org/#/just-ts-mode). It has better
syntax highlighting, fixing issues like [poor highlighting of
comments](https://github.com/leon-barrett/just-mode.el/issues/11).

# License

`just-mode` is licensed under the GPL3 license. Please see file `LICENSE` for
its text.
