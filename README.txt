======================== calc.lisp =========================

A tui scientific/programmers calculator in common lisp.
Inspired by speedcrunch, but only with the features that I
like, plus much better support for embedded programmng with
bit bashing and overflow semantics.

Implemented as a single common lisp file with no
dependencies.

========================= features =========================

* Real-time results while typing
* Arbitrary size integer arithmetic
* Fixed size signed/unsigned integer arithmetic (u8, i16,
  etc)
* Real-time bitfield displayed while typing
* Overflow warnings when using fixed-size types
* Arbitrary bases
* SI engineer's shorthand (e.g. 5k2 = 5200)
* Double-float arithmetic
* Functions and variables
* Persistent sessions with saved histrory, functions, and
  variables
* Alternate "simple" terminal mode without real-time tui
  (standard REPL)

========================= building =========================

Since the entire project is a single lisp file with no
dependencies, it does not use asdf or quicklisp. The
makefile simply compile and loads the file, and then does
(save-lisp-and-die).

$ make fcalc

========================= running ==========================

The behaviour is selected based on the filename.
* ccalc: simple repl (no terminal escapes/realtime
  results)
* fcalc: full TUI repl with realtime results and bitfield

The full TUI repl depends on GNU Readline at runtime (not
build time). If GNU readline is not found when run, it will
fall back to the simple REPL.
