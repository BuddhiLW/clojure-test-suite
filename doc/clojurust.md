# Running the Test Suite with clojurust

[clojurust](https://github.com/csm/clojurust) is a Rust-hosted Clojure dialect.
Its binary is `cljrs`; source files use `.cljrs` natively and `.cljc` for
cross-platform code.

## Pre-requisites

A `cljrs` binary. Either install from crates.io:

```bash
cargo install cljrs
```

or build one from a checkout:

```bash
cargo build --release --bin cljrs    # produces target/release/cljrs
```

## Running Tests

Tests can be run using the Babashka task:

```bash
bb test-rust
```

The task looks for `cljrs` on `PATH`. To test a binary you just built, point
`CLJRS_BIN` at it:

```bash
CLJRS_BIN=/path/to/clojurust/target/release/cljrs bb test-rust
```

`cljrs test --src-path test` discovers every test namespace under the source
path itself, so the task passes no namespace list.

## Reader conditionals

clojurust's platform feature key is `:rust`; it also honours `:default`. It
does not read `:clj`, so a `.cljc` file's JVM branch does not apply to it —
give it `:rust` where it differs and let it fall through to `:default`
otherwise:

```clojure
#?(:rust    (clojure.core/sleep ms)   ; no Thread/sleep — a core builtin
   :default (Thread/sleep ms))
```
