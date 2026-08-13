# Running the Test Suite with ClojureWasm

[ClojureWasm](https://github.com/BuddhiLW/ClojureWasm) is a JVM-free Clojure
runtime written in Zig, with a WebAssembly FFI. Its binary is `cljw`.

## Pre-requisites

A `cljw` binary. Either install a release:

```bash
brew install buddhilw/tap/cljw
```

or build one from a checkout (Zig 0.16.0):

```bash
zig build -Dwasm -Doptimize=ReleaseSafe   # produces zig-out/bin/cljw
```

## Running Tests

Tests can be run using the Babashka task:

```bash
bb test-cljw
```

The task looks for `cljw` on `PATH`. To test a binary you just built, point
`CLJW_BIN` at it:

```bash
CLJW_BIN=/path/to/ClojureWasm/zig-out/bin/cljw bb test-cljw
```

There is no build or compile step — `cljw` reads `.cljc` directly off the
source path (`-cp test`).

## Reader conditionals

ClojureWasm's platform feature set is `{:cljw, :clj, :default}`. It implements
Clojure rather than ClojureScript, so it reads the `:clj` branch of an ordinary
`.cljc` file. Branches are selected left to right, so `:cljw` only wins where
it is written *ahead* of `:clj`:

```clojure
#?(:cljw (is (= "BigInt" (str (type n))))    ; cljw-specific expectation
   :clj  (is (instance? clojure.lang.BigInt n)))
```

Use `:cljw` for cases where ClojureWasm genuinely differs from Clojure JVM
(it has no host classes, so anything naming `clojure.lang.*` or a Java class
needs one), and leave everything else on `:clj` / `:default`.
