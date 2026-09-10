(ns ^:typed.clojure clojure.core-model.lt-typed
  "Static signatures for `<`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the arguments, plus the value returned or the
  simple name of the class thrown. The throw branch is data, not an effect."
  (t/U '{:args (t/Vec t/Any) :value t/Any}
       '{:args (t/Vec t/Any) :throws t/Str}))

(t/ann reference-lt [t/Num t/Num :-> Boolean])
(defn reference-lt
  "`<` over the numeric tower, binary — the shape a static host can state. The
  static domain is narrower than the run-time one, which admits any single
  argument and refuses a non-number from arity 2 up."
  [a b]
  (< a b))

(t/ann lt-outcome [t/Num t/Num :-> Outcome])
(defn lt-outcome
  "`reference-lt` as a total observation. Only the value branch is statically
  reachable — the throw branch belongs to arguments this signature excludes."
  [a b]
  {:args [a b] :value (reference-lt a b)})

(t/ann lt-int [t/Int t/Int :-> Boolean])
(defn lt-int
  "`<` restricted to the integer domain, where it IS a strict total order:
  no NaN lives there to leave two values unordered."
  [a b]
  (< a b))

(t/ann strict-total-order? Boolean)
(def strict-total-order?
  "Whether `<` is a strict total order over its whole run-time domain. False:
  NaN is unordered, so trichotomy fails for every pair involving it."
  false)

(t/ann arity-one-inspects-its-argument? Boolean)
(def arity-one-inspects-its-argument?
  "Whether `(< x)` checks that `x` is a number. False: it answers true for a
  string, a keyword or nil without looking."
  false)
