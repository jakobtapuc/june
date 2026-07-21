# ⚠️ WORK IN PROGRESS
This is an attempt at teaching myself how to:
  - lex source code
  - parse tokens into an s-exp AST (using combinators)
  - evaluate the AST

All of this is done in SML and compiled using MLton (take a look at `./github/workflows/mlton.yaml` for further info).

I do use as many `SuccessorML` features using `"allowSuccessorML true"` in the MLB file as possible. This includes:
  - `do` declarations
  - Optional pattern bars
  - Optional semicolons
  - Record punning<super>1</super> 💖

1\. Hear me out, this is a small yet life-changing feature.

## This is not Scheme!
Even after I'm "finished" with the base implementation
my aim isn't to create a compliant Scheme implementation. Hence the name which isn't: Micro/Nano/Pico/Femto-Scheme.
## Roadmap
### Urgent
- [x] Lexer + Pretty print
- [x] Base AST + Pretty print
- [x] Error reporting with line positions<super>1</super>
- [x] Lists
- [x] Arithmetic operations
- [ ] Proper quoting
- [ ] Strings
- [ ] Floats
- [x] Displaying values
- [x] `define` for simple bindings
- [ ] Lambdas (Again, this is a WIP)
- [ ] Primitive `if`
- [ ] AST desugaring for `define`, `let`, and `cond`<super>2</super>

1\. This has been my goal from the very beginning, and it's something that most of the implementations my brain has been trying to hoard didn't include.

2\. It's kind of effing important, and AST manipulation is probably what I'm most curious about.

### May be implemented in the far future if I feel like it*
\*As in: don't get your hopes up
- [ ] A library system:
```scheme
(require "prelude") ; For the STD Prelude
(require "thread") ; For the STD green threads
(require "./math") ; For relative imports

; Scheme's naming conventions make it a breeze
(math/floor ...)
(thread/yield ...)
```
- [ ] Adding support for MLton's green threads

### Won't be implemented because I care about my mental health
- [ ] Macros in any form or shape
- [ ] `call/cc`
- [ ] Quasi-quotes
## ℹ️ Caveat
I **do not** intend to create a full-fledged general purpose programming language here, folks. It's a toy project that'll never become big lolz.
### Won't be implemented because I want a pretty pure language
- [ ] Mutation (no `set!`)