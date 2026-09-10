(ns ^:typed.clojure clojure.core-model.bigint-typed
  "Static signatures for `bigint`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias BigIntOutcome
  "A total observation of one call: the promoted integer, or a marker naming the
  class that was thrown."
  (t/U clojure.lang.BigInt (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-bigint [(t/U t/Num t/Str) :-> clojure.lang.BigInt])
(defn reference-bigint
  "`bigint` over its declared domain. Total in the type but not in fact: the
  non-finite doubles and text outside BigInteger's grammar still throw."
  [x]
  (bigint x))

(t/ann observed-bigint [(t/U t/Num t/Str) :-> BigIntOutcome])
(defn observed-bigint
  "The same call observed totally, so a throw is a value the snapshot can hold."
  [x]
  (try (bigint x) (catch Throwable e {:throws (.getName (class e))})))

(t/ann widening-is-lossless? Boolean)
(def widening-is-lossless?
  "Whether every argument reaches the result unchanged. False: a fractional
  argument is truncated toward zero, and a double carries only the precision a
  double had."
  false)

(t/ann declared-domain-is-total? Boolean)
(def declared-domain-is-total?
  "Whether the static domain `(U Num Str)` is where the function is actually
  defined. False: it is an over-approximation, and the run-time refusals inside
  it are exactly what the golden records."
  false)
