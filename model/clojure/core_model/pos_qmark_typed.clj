(ns ^:typed.clojure clojure.core-model.pos-qmark-typed
  "Static signatures for `pos?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-pos? [t/Num :-> Boolean])
(defn reference-pos?
  "`pos?` over the numeric tower, where it is total."
  [x]
  (pos? x))

(t/ann probe-pos? [t/Num :-> '[t/Num Boolean]])
(defn probe-pos?
  "One `pos?` call as data: the argument paired with its answer. The run-time
  model widens the argument to `t/Any` so it can record the refusals."
  [x]
  [x (pos? x)])

(t/ann nan-is-positive? Boolean)
(def nan-is-positive?
  "Whether ##NaN is positive. False: a NaN is unordered, so `(> x 0)` is false
  and so is `(< x 0)`."
  false)

(t/ann trichotomy-total? Boolean)
(def trichotomy-total?
  "Whether exactly one of `zero?`, `pos?` and `neg?` holds for every argument.
  False: ##NaN satisfies none of the three."
  false)
