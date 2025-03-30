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