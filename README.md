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

## Chapter 1

### 1.1

**"lexer" vs. "tokenizer" vs. "scanner"?**

https://en.wikipedia.org/wiki/Lexical_analysis

"scanning" / "tokenizing" is the first stage of a lexer

"lexing" involves adding contextual information, like the type of the token, its position in the source code, etc.

**What makes an abstract syntax tree "abstract"?**

https://en.wikipedia.org/wiki/Abstract_syntax_tree

not every detail is captured as-is in the tree

e.g. parentheses are implied by the structure of the tree

### 1.2

What token(s) would we need to add if we were writing a lexer for Python (or another whitespace-sensitive language)?

### 1.3

Parsing Unicode could be complex!

What issues might we encounter if we wanted our lexer to support not just ASCII, but the full Unicode range?

Java allows (letters, \$, _) as the first character of an identifier, but (letter, digit, \$, _) for subsequent characters. How would you implement this in a lexer?

### 1.4

Adding < and > to our tokens reminds me of HTML: https://stackoverflow.com/a/1732454

HTML is too complex to parse with only regular expressions, you need a parser, like the one we're writing: https://stackoverflow.com/a/1758162

## Chapter 2

### 2.2

yacc, bison, and ANTLR are tools that are commonly used in the programming language development community.

EBNF is all that is needed to automatically define a parser using these tools.

### 2.4

As you read this book and write Monkey code, compare it to the Go code we're using to write the interpreter. Go assignments are very different from Monkey ones (= vs. := operators).

Can anyone think of constructs which are expressions vs. statements in various languages?

Recursive descent is just one kind of parser. Did anyone look into other kinds?