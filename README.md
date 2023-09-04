# the hestia programming language

**hestia** (pronounced *HES-ti-a* or *es-TI-a*) is a small, functional, dynamically
typed programming language designed for writing small scripts. It can be learned
in an afternoon, but is not especially fast or especially general-purpose.

See <site> for more details.

**hestia** aims to use as few (Rust) dependencies as possible, so expect lots of
yak-shaving.

## non-goals

* Static typing
* Speed
* Safety
* Being suitable for writing large, multi-file programs

## this repository

This is a mono-repo containing: code for the **hestia** "library" (`./lib`), which is
where all of the lexer, parser, and interpreter code lives; code for the **hestia**
REPL and binary (`./bin`), which is used to run **hestia** programs; and code for
**hestia's** website and docs (`./site`). Both `bin` and `site` consume `lib` for
the REPL/binary and WebAssembly REPL, respectively.

**To run the current hestia REPL, clone this repository, `cd bin`, and `cargo
run`.**

## inspiration

Languages that have influenced me over the years:

* Ruby
* Haskell
* Lua
* Clojure
* Racket
* Go
* Elm
* Bash
