(ns ^:typed.clojure clojure.core-model.gt-typed
  "Static signatures for `>`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the arguments, plus the value returned or the
  simple name of the class thrown. The throw branch is data, not an effect."
  (t/U '{:args (t/Vec t/Any) :value t/Any}
       '{:args (t/Vec t/Any) :throws t/Str}))

(t/ann reference-gt [t/Num t/Num :-> Boolean])
(defn reference-gt
  "`>` over the numeric tower, binary — the shape a static host can state. The
  static domain is narrower than the run-time one, which admits any single
  argument and refuses a non-number from arity 2 up."
  [a b]
  (> a b))

(t/ann gt-outcome [t/Num t/Num :-> Outcome])
(defn gt-outcome
  "`reference-gt` as a total observation. Only the value branch is statically
  reachable — the throw branch belongs to arguments this signature excludes."
  [a b]
  {:args [a b] :value (reference-gt a b)})

(t/ann gt-int [t/Int t/Int :-> Boolean])
(defn gt-int
  "`>` restricted to the integer domain, where it IS a strict total order:
  no NaN lives there to leave two values unordered."
  [a b]
  (> a b))

(t/ann dual-of-lt? Boolean)
(def dual-of-lt?
  "Whether `(> a b)` is `(< b a)`. True: `>` swaps its arguments. It is NOT
  the negation of `<`, which would make it true at every equal pair."
  true)

(t/ann negation-of-lt? Boolean)
(def negation-of-lt?
  "Whether `(> a b)` is `(not (< a b))`. False: both are false at an equal
  pair, and both are false at every pair involving NaN."
  false)
