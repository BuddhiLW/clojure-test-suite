(ns ^:typed.clojure clojure.core-model.keyword-typed
  "Static signatures for `keyword`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A recorded call result: the host's keyword, or the class name it threw."
  (t/U t/Kw '[t/Kw (t/Nilable t/Str)]))

(t/ann reference-keyword (t/IFn [(t/U t/Str t/Kw t/Sym) :-> t/Kw]
                                [(t/Nilable t/Str) t/Str :-> t/Kw]))
(defn reference-keyword
  "`keyword` at both its arities. The one-argument arity is nil-returning for
  anything it cannot name, which no static signature over its declared domain
  can express; the two-argument arity takes strings only."
  ([x] (keyword x))
  ([ns-part nm] (keyword ns-part nm)))

(t/ann characterize-keyword [t/Str (t/Nilable t/Str) :-> Outcome])
(defn characterize-keyword
  "The total outcome of one `keyword` call, the one-argument arity written as a
  nil name. The thrown arm is what a dynamically typed host reaches when the
  two-argument arity is handed a symbol, a keyword or a nil name."
  [ns-or-name nm]
  (try (if nm (keyword ns-or-name nm) (keyword ns-or-name))
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(t/ann nil-for-unnameable? Boolean)
(def nil-for-unnameable?
  "Whether the one-argument arity answers nil instead of throwing for an
  argument that is neither a string nor Named. True: `(keyword 1)` is nil."
  true)
