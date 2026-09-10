(ns ^:typed.clojure clojure.core-model.boolean-typed
  "Static signatures for `boolean`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-boolean [t/Any :-> Boolean])
(defn reference-boolean
  "`boolean` over the whole value space. The one coercion in the family whose
  static and run-time signatures agree: total in, total out, no refusal."
  [x]
  (boolean x))

(t/ann boolean-of-boolean [Boolean :-> Boolean])
(defn boolean-of-boolean
  "`boolean` restricted to booleans, where it is the identity."
  [x]
  (boolean x))

(t/ann refuses-any-input? Boolean)
(def refuses-any-input?
  "Whether any argument is refused. False: `boolean` is total, the only
  coercion in the family that never throws."
  false)

(t/ann only-nil-and-false-are-falsey? Boolean)
(def only-nil-and-false-are-falsey?
  "Whether nil and false are the sole falsey inputs. True: zero, NaN, the empty
  string and every empty collection all map to true."
  true)

(t/ann loses-information? Boolean)
(def loses-information?
  "Whether the coercion discards information. True, and unrecoverably so: the
  whole argument collapses to one bit, so it is the family's only coercion that
  cannot be inverted at all."
  true)
