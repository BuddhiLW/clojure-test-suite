(ns ^:typed.clojure clojure.core-model.minus-typed
  "Static signatures for `-`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-minus [t/Num t/Num :-> t/Num])
(defn reference-minus
  "`-` over the full numeric tower, binary — the shape a static host can state."
  [x y]
  (- x y))

(t/ann negate-int [t/Int :-> t/Int])
(defn negate-int
  "Unary `-` over the integer domain. The static signature is total; the run-time
  one is not, because negating the most negative long overflows."
  [x]
  (- x))

(t/ann negation-total-over-longs? Boolean)
(def negation-total-over-longs?
  "Whether unary `-` lands in the longs for every long input. False: two's
  complement holds one more negative than positive, and the host throws there."
  false)
