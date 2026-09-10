(ns ^:typed.clojure clojure.core-model.star-typed
  "Static signatures for `*`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-star [t/Num t/Num :-> t/Num])
(defn reference-star
  "`*` over the full numeric tower, binary — the shape a static host can state."
  [x y]
  (* x y))

(t/ann star-int [t/Int t/Int :-> t/Int])
(defn star-int
  "`*` restricted to the integer domain. The static signature is total; the
  run-time one is not, because a long product that leaves the range throws."
  [x y]
  (* x y))

(t/ann star-identity t/Int)
(def star-identity
  "The value `(*)` returns, and the identity element of the operation."
  1)

(t/ann zero-absorbs-everything? Boolean)
(def zero-absorbs-everything?
  "Whether a zero operand forces a zero product for every other operand. False:
  against an infinity or a NaN the product is NaN."
  false)
