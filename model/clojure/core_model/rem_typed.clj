(ns ^:typed.clojure clojure.core-model.rem-typed
  "Static signatures for `rem`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-rem [t/Num t/Num :-> t/Num])
(defn reference-rem
  "`rem` over the full numeric tower — the shape a static host can state. The
  static signature is total; the run-time one is not, because a zero divisor
  throws and an infinite operand fails decimal conversion."
  [num div]
  (rem num div))

(t/ann rem-int [t/Int t/Int :-> t/Int])
(defn rem-int
  "`rem` restricted to the integer domain."
  [num div]
  (rem num div))

(t/ann follows-dividend-sign? Boolean)
(def follows-dividend-sign?
  "Whether the result carries the sign of the dividend rather than the divisor.
  True: that is the whole difference between `rem` and `mod`."
  true)
