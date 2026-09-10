(ns ^:typed.clojure clojure.core-model.eq-typed
  "Static signatures for `=`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the arguments, plus the value returned or the
  simple name of the class thrown. The throw branch is data, not an effect."
  (t/U '{:args (t/Vec t/Any) :value t/Any}
       '{:args (t/Vec t/Any) :throws t/Str}))

(t/ann reference-eq [t/Any t/Any :-> Boolean])
(defn reference-eq
  "`=` binary — the shape a static host can state. Total over every pair of
  values, nil included, and lands in the booleans whatever they are."
  [a b]
  (= a b))

(t/ann eq-outcome [t/Any t/Any :-> Outcome])
(defn eq-outcome
  "`reference-eq` as a total observation. Only the value branch is statically
  reachable — the throw branch belongs to the zero-arity call, which this
  binary signature excludes."
  [a b]
  {:args [a b] :value (reference-eq a b)})

(t/ann eq-num [t/Num t/Num :-> Boolean])
(defn eq-num
  "`=` restricted to the numeric tower. Still total, and still not numeric
  equivalence: it partitions the tower into categories before comparing."
  [a b]
  (= a b))

(t/ann crosses-numeric-categories? Boolean)
(def crosses-numeric-categories?
  "Whether `=` holds between two numbers of different categories, such as a
  long and a double of the same magnitude. False: `==` does that, `=` does not."
  false)

(t/ann nan-equals-a-distinct-nan? Boolean)
(def nan-equals-a-distinct-nan?
  "Whether two NaN doubles are `=`. False — though a NaN is `=` to ITSELF,
  because `=` short-circuits on object identity first."
  false)
