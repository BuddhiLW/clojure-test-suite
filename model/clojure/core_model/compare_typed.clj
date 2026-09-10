(ns ^:typed.clojure clojure.core-model.compare-typed
  "Static signatures for `compare`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the arguments, plus the value returned or the
  simple name of the class thrown. The throw branch is data, not an effect."
  (t/U '{:args (t/Vec t/Any) :value t/Any}
       '{:args (t/Vec t/Any) :throws t/Str}))

(t/ann reference-compare [(t/Nilable t/Num) (t/Nilable t/Num) :-> t/AnyInteger])
(defn reference-compare
  "`compare` over nil and the numeric tower — the part of the domain a static
  host can state. nil precedes every number; otherwise the answer is the sign
  of the ordering, and 0 wherever neither operand is the smaller, so NaN
  compares 0 against everything."
  [a b]
  (cond (and (nil? a) (nil? b)) 0
        (nil? a)                -1
        (nil? b)                1
        (< a b)                 -1
        (> a b)                 1
        :else                   0))

(t/ann compare-outcome [(t/Nilable t/Num) (t/Nilable t/Num) :-> Outcome])
(defn compare-outcome
  "`reference-compare` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to the refused pairs and the
  wrong arities, which this signature excludes."
  [a b]
  {:args [a b] :value (reference-compare a b)})

(t/ann compare-num [t/Num t/Num :-> t/AnyInteger])
(defn compare-num
  "`compare` restricted to the numeric tower, where it never refuses a pair."
  [a b]
  (cond (< a b) -1 (> a b) 1 :else 0))

(t/ann sign-only? Boolean)
(def sign-only?
  "Whether the result is confined to -1, 0 and 1. False: strings and characters
  return a difference, so only the SIGN of the result is contractual."
  false)

(t/ann consistent-with-eq? Boolean)
(def consistent-with-eq?
  "Whether a 0 result means the arguments are `=`. False in both directions: a
  long and an equal double compare 0 without being `=`, and two `=` collections
  of different kinds are refused rather than compared."
  false)

(t/ann total-order? Boolean)
(def total-order?
  "Whether `compare` induces a total order. False: it answers 0 for every pair
  involving NaN, so its zero relation is not even transitive."
  false)
