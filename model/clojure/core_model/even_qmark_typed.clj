(ns ^:typed.clojure clojure.core-model.even-qmark-typed
  "Static signatures for `even?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-even? [t/AnyInteger :-> Boolean])
(defn reference-even?
  "`even?` on the integral domain, the only domain on which it is total."
  [x]
  (even? x))

(t/ann probe-even? [t/AnyInteger :-> '[t/AnyInteger Boolean]])
(defn probe-even?
  "One `even?` call as data: the argument paired with its answer. The run-time
  model widens the argument to `t/Any` so it can record the refusals."
  [x]
  [x (even? x)])

(t/ann total-on-integers? Boolean)
(def total-on-integers?
  "Whether `even?` is total on the integral domain. True: parity is defined at
  every width, Long/MIN_VALUE included."
  true)

(t/ann answers-non-integers? Boolean)
(def answers-non-integers?
  "Whether `even?` answers a non-integral argument. False: 2.0, 1/2 and 0.2M
  are domain errors, not even numbers."
  false)
