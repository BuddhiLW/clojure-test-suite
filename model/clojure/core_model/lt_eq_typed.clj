(ns ^:typed.clojure clojure.core-model.lt-eq-typed
  "Static signatures for `<=`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the arguments, plus the value returned or the
  simple name of the class thrown. The throw branch is data, not an effect."
  (t/U '{:args (t/Vec t/Any) :value t/Any}
       '{:args (t/Vec t/Any) :throws t/Str}))

(t/ann reference-lt-eq [t/Num t/Num :-> Boolean])
(defn reference-lt-eq
  "`<=` over the numeric tower, binary — the shape a static host can state. The
  static domain is narrower than the run-time one, which admits any single
  argument and refuses a non-number from arity 2 up."
  [a b]
  (<= a b))

(t/ann lt-eq-outcome [t/Num t/Num :-> Outcome])
(defn lt-eq-outcome
  "`reference-lt-eq` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to arguments this signature
  excludes."
  [a b]
  {:args [a b] :value (reference-lt-eq a b)})

(t/ann lt-eq-int [t/Int t/Int :-> Boolean])
(defn lt-eq-int
  "`<=` restricted to the integer domain, where it IS a total order: no NaN
  lives there to break reflexivity."
  [a b]
  (<= a b))

(t/ann reflexive? Boolean)
(def reflexive?
  "Whether `(<= x x)` holds for every `x` in the run-time domain. False: NaN
  is unordered, so it is not `<=` even itself."
  false)

(t/ann induced-by-compare? Boolean)
(def induced-by-compare?
  "Whether `<=` is the ordering `compare` induces. False: `compare` answers 0
  for a pair involving NaN, where `<=` answers false."
  false)
