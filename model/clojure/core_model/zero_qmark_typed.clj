(ns ^:typed.clojure clojure.core-model.zero-qmark-typed
  "Static signatures for `zero?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-zero? [t/Num :-> Boolean])
(defn reference-zero?
  "`zero?` over the numeric tower, where it is total."
  [x]
  (zero? x))

(t/ann probe-zero? [t/Num :-> '[t/Num Boolean]])
(defn probe-zero?
  "One `zero?` call as data: the argument paired with its answer. The run-time
  model widens the argument to `t/Any` so it can record the refusals."
  [x]
  [x (zero? x)])

(t/ann negative-zero-is-zero? Boolean)
(def negative-zero-is-zero?
  "Whether -0.0 is zero. True: the sign bit does not survive numeric equality."
  true)

(t/ann nan-is-zero? Boolean)
(def nan-is-zero?
  "Whether ##NaN is zero. False: every comparison with a NaN is false, so a NaN
  is neither zero nor positive nor negative."
  false)
