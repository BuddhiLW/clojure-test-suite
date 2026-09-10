(ns ^:typed.clojure clojure.core-model.quot-typed
  "Static signatures for `quot`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-quot [t/Num t/Num :-> t/Num])
(defn reference-quot
  "`quot` over the full numeric tower — the shape a static host can state. The
  static signature is total; the run-time one is not, because a zero divisor
  throws and an infinite operand fails decimal conversion."
  [num div]
  (quot num div))

(t/ann quot-int [t/Int t/Int :-> t/Int])
(defn quot-int
  "`quot` restricted to the integer domain."
  [num div]
  (quot num div))

(t/ann rounds-toward-zero? Boolean)
(def rounds-toward-zero?
  "Whether `quot` truncates toward zero rather than flooring. True: that is what
  separates it from `mod`-style floored division on negative operands."
  true)

(t/ann guards-long-overflow? Boolean)
(def guards-long-overflow?
  "Whether `quot` throws when the exact quotient leaves the long range. False:
  the most negative long divided by -1 wraps back to itself, silently."
  false)
