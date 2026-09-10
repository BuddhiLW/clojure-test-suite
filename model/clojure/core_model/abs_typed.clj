(ns ^:typed.clojure clojure.core-model.abs-typed
  "Static signatures for `abs`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann abs-int [t/Int :-> t/Int])
(defn abs-int
  "`abs` restricted to the integer domain. Not total into the non-negatives:
  at the most negative value the negation overflows and returns itself."
  [x]
  (if (neg? x) (- x) x))

(t/ann abs-num [t/Num :-> t/Num])
(defn abs-num
  "`abs` over the full numeric tower."
  [x]
  (if (neg? x) (- x) x))

(t/ann total-into-non-negatives? Boolean)
(def total-into-non-negatives?
  "Whether `abs` over the integer domain lands in the non-negatives for every
  input. False: two's complement holds one more negative than positive."
  false)
