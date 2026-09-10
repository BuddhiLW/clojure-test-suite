(ns ^:typed.clojure clojure.core-model.neg-qmark-typed
  "Static signatures for `neg?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-neg? [t/Num :-> Boolean])
(defn reference-neg?
  "`neg?` over the numeric tower, where it is total."
  [x]
  (neg? x))

(t/ann probe-neg? [t/Num :-> '[t/Num Boolean]])
(defn probe-neg?
  "One `neg?` call as data: the argument paired with its answer. The run-time
  model widens the argument to `t/Any` so it can record the refusals."
  [x]
  [x (neg? x)])

(t/ann negative-zero-is-negative? Boolean)
(def negative-zero-is-negative?
  "Whether -0.0 is negative. False: it carries a sign bit but compares equal to
  0.0, and `neg?` reads the comparison, not the bit."
  false)

(t/ann nan-is-negative? Boolean)
(def nan-is-negative?
  "Whether ##NaN is negative. False: a NaN is unordered, so it is neither
  negative nor positive nor zero."
  false)
