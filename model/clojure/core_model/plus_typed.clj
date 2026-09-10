(ns ^:typed.clojure clojure.core-model.plus-typed
  "Static signatures for `+`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-plus [t/Num t/Num :-> t/Num])
(defn reference-plus
  "`+` over the full numeric tower, binary — the shape a static host can state."
  [x y]
  (+ x y))

(t/ann plus-int [t/Int t/Int :-> t/Int])
(defn plus-int
  "`+` restricted to the integer domain. The static signature is total; the
  run-time one is not, because a long sum that leaves the long range throws."
  [x y]
  (+ x y))

(t/ann plus-identity t/Int)
(def plus-identity
  "The value `(+)` returns, and the identity element of the operation."
  0)

(t/ann promotes-on-long-overflow? Boolean)
(def promotes-on-long-overflow?
  "Whether long addition widens rather than throwing when the sum leaves the long
  range. False: only an already-promoted operand widens the result."
  false)
