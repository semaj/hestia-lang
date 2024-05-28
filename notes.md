TODO:

* Modules
* format string
* println
* Splat operator
* files
* http

## Grammar

I believe hestia has an almost-linear grammar. There are a few cases where we
need to back-track the lexer due to ambiguity (see maps and functions), but
generally it there is no ambiguity about what tokens it expects.
