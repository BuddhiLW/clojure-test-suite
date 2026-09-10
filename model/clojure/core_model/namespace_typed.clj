(ns ^:typed.clojure clojure.core-model.namespace-typed
  "Static signatures for `namespace`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A recorded call result: the host's qualifier, nil, or the class it threw."
  (t/U nil t/Str '[t/Kw (t/Nilable t/Str)]))

(t/ann reference-namespace [(t/U t/Kw t/Sym) :-> (t/Nilable t/Str)])
(defn reference-namespace
  "`namespace` over the two types it accepts. The result is nilable by design:
  nil is the answer for an unqualified ident, and is distinct from the empty
  string an explicitly empty qualifier gives."
  [x]
  (namespace x))

(t/ann characterize-namespace [(t/U t/Kw t/Sym) :-> Outcome])
(defn characterize-namespace
  "The total outcome of one `namespace` call. A string is not Named, so a host
  without static types reaches the thrown arm on it as well as on nil."
  [x]
  (try (namespace x)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(t/ann nil-for-unqualified? Boolean)
(def nil-for-unqualified?
  "Whether an unqualified ident answers nil rather than the empty string.
  True, and the distinction is observable: `(keyword \"\" \"x\")` answers \"\"."
  true)
