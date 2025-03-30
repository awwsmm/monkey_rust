# Writing an Interpreter in ~~Go~~ Rust

Before writing any code

- make sure Rust is installed (https://rustup.rs/)
- create a new binary crate with `cargo new monkey_rust --bin`
- install RustRover
- (optional) enable "format on save" in RustRover settings and restart
- (optional) add `rustfmt` pre-commit hook at `.git/hooks/pre-commit`
    - copy from https://eugene-babichenko.github.io/blog/2018/11/08/rustfmt-git-hook/
    - enable with `chmod +x .git/hooks/pre-commit`

This book uses a TDD style. Has anyone used that before?

### 1.1

**"lexer" vs. "tokenizer" vs. "scanner"?**

https://en.wikipedia.org/wiki/Lexical_analysis

"scanning" / "tokenizing" is the first stage of a lexer

"lexing" involves adding contextual information, like the type of the token, its position in the source code, etc.

**What makes an abstract syntax tree "abstract"?**

https://en.wikipedia.org/wiki/Abstract_syntax_tree

not every detail is captured as-is in the tree

e.g. parentheses are implied by the structure of the tree