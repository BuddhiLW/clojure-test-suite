(ns ^:typed.clojure clojure.core-model.slash-typed
  "Static signatures for `/`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-slash [t/Num t/Num :-> t/Num])
(defn reference-slash
  "`/` over the full numeric tower, binary — the shape a static host can state."
  [x y]
  (/ x y))

(t/ann reciprocal [t/Num :-> t/Num])
(defn reciprocal
  "Unary `/`. The static signature is total; the run-time one is not, because a
  zero argument throws."
  [x]
  (/ x))

(t/ann closed-over-integers? Boolean)
(def closed-over-integers?
  "Whether dividing two integers yields an integer. False: the result is an exact
  ratio, which is why a dialect without a rational tower diverges here."
  false)
