(ns ^:typed.clojure clojure.core-model.nan-qmark-typed
  "Static signatures for `NaN?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-nan? [Double :-> Boolean])
(defn reference-nan?
  "`NaN?` on the double domain. The static domain is narrower than the run-time
  one: the host answers false for any other number in the tower."
  [x]
  (NaN? x))

(t/ann probe-nan? [Double :-> '[Double Boolean]])
(defn probe-nan?
  "One `NaN?` call as data: the argument paired with its answer. The run-time
  model widens the argument to `t/Any` so it can record the refusals."
  [x]
  [x (NaN? x)])

(t/ann infinities-are-nan? Boolean)
(def infinities-are-nan?
  "Whether ##Inf and ##-Inf are NaN. False: they are ordered doubles, equal to
  themselves."
  false)

(t/ann nan-equals-itself? Boolean)
(def nan-equals-itself?
  "Whether ##NaN is equal to itself. False: self-inequality is what identifies
  a NaN at all."
  false)
