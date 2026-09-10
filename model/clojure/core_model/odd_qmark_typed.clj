(ns ^:typed.clojure clojure.core-model.odd-qmark-typed
  "Static signatures for `odd?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-odd? [t/AnyInteger :-> Boolean])
(defn reference-odd?
  "`odd?` on the integral domain, the only domain on which it is total."
  [x]
  (odd? x))

(t/ann probe-odd? [t/AnyInteger :-> '[t/AnyInteger Boolean]])
(defn probe-odd?
  "One `odd?` call as data: the argument paired with its answer. The run-time
  model widens the argument to `t/Any` so it can record the refusals."
  [x]
  [x (odd? x)])

(t/ann complements-even? Boolean)
(def complements-even?
  "Whether `odd?` is the complement of `even?` on the integral domain. True:
  every integer is exactly one of the two."
  true)

(t/ann answers-non-integers? Boolean)
(def answers-non-integers?
  "Whether `odd?` answers a non-integral argument. False: 3.0, 1/2 and 0.2M
  are domain errors, not odd numbers."
  false)
