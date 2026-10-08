# Running the Test Suite with jank

[jank](https://jank-lang.org) is a native Clojure dialect on LLVM with C++
interop. Its binary is `jank`; source files use `.jank` natively and `.cljc`
for cross-platform code. The suite is owned by the jank project, so many test
files already carry `:jank` reader branches.

## Pre-requisites

A `jank` binary. The [book](https://book.jank-lang.org) lists the supported
install routes: `brew install jank-lang/jank/jank` (macOS and Linux), the
`ppa.jank-lang.org` apt repository on Ubuntu 24.04+, or the AUR on Arch.

Without sudo, unpack the `.deb` at user level:

```bash
curl -O https://ppa.jank-lang.org/pool/main/j/jank/jank_0.1-questing_amd64.deb
dpkg-deb -x jank_0.1-questing_amd64.deb ~/.local/share/hive/dialects/jank/root
```

The binary bundles its own clang/LLVM but JIT-compiles against the host's
libstdc++ headers, and the `questing`/`resolute` build expects **GCC 16**
headers. On a host that only has GCC 15 the first run crashes with
`'stdlib.h' file not found` while building the precompiled header. Either
install `libstdc++-16-dev` or, without root, extract it and overlay it with
`bwrap` (this is what the wrapper used for the recorded sweep does):

```bash
apt-get download libstdc++-16-dev libgcc-16-dev
for d in lib*16-dev*.deb; do dpkg-deb -x "$d" ~/.local/share/hive/dialects/jank/gcc16; done
G=~/.local/share/hive/dialects/jank/gcc16
exec bwrap --bind / / --dev-bind /dev /dev \
  --overlay-src /usr/include/c++ --overlay-src $G/usr/include/c++ --ro-overlay /usr/include/c++ \
  --overlay-src /usr/include/x86_64-linux-gnu/c++ --overlay-src $G/usr/include/x86_64-linux-gnu/c++ --ro-overlay /usr/include/x86_64-linux-gnu/c++ \
  --overlay-src /usr/lib/gcc/x86_64-linux-gnu --overlay-src $G/usr/lib/gcc/x86_64-linux-gnu --ro-overlay /usr/lib/gcc/x86_64-linux-gnu \
  ~/.local/share/hive/dialects/jank/root/usr/local/bin/jank "$@"
```

The first run builds a precompiled header (about 15 s) and caches it under
`~/.cache/jank`.

## Running Tests

```bash
bb test-jank
```

The task looks for `jank` on `PATH`. To use another binary or wrapper, point
`JANK_BIN` at it:

```bash
JANK_BIN=~/.local/share/hive/dialects/jank/jank bb test-jank
```

jank has no `-e` flag, so the runner (`bb/native_test.clj`, dialect `:jank`)
runs each namespace in its own process through a driver file:

```bash
jank --module-path test run bb/jank_driver.jank -- clojure.core-test.abs
```

The driver requires the namespace, calls `clojure.test/run-tests` and prints
the `#counts <tests> <pass> <fail> <error>` marker line, like the cljw and
babashka runners. Results are written to `target/conformance/jank.edn` and
joined into `doc/conformance.md` by `bb conformance`. Each namespace takes
roughly 5-10 s because jank JIT-compiles the test code; raise
`NATIVE_TEST_TIMEOUT` if a namespace times out.

## Reader conditionals

jank's platform feature key is `:jank`; it also honours `:default`. Give it a
`:jank` branch where it differs:

```clojure
#?(:jank    (cpp/std.this_thread.sleep_for ...)
   :default (Thread/sleep ms))
```

## Known gaps

See the jank row of [conformance.md](conformance.md) for the per-namespace
status, and the "Known gaps" list at the end of this file.
