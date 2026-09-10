(ns ^:typed.clojure clojure.core-model.contains-qmark-typed
  "Static signatures for `contains?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-contains? (t/All [k v] [(t/Map k v) t/Any :-> t/Bool]))
(defn reference-contains?
  "`contains?` over a map. The key argument is unconstrained: a key of the wrong
  type is absent, not a type error."
  [m k]
  (contains? m k))

(t/defalias Throw
  "The recorded shape of a host exception: the simple class name, as data."
  (t/HMap :mandatory {:throws t/Str} :complete? true))

(t/ann contains-outcome (t/All [k v] [(t/Map k v) t/Any :-> (t/U t/Bool Throw)]))
(defn contains-outcome
  "`reference-contains?` as a total observation. Only the boolean branch is
  statically reachable — the throw branch belongs to the non-indexed seqs and
  scalars, which this signature excludes."
  [m k]
  (reference-contains? m k))

(t/ann answers-a-boolean? Boolean)
(def answers-a-boolean?
  "Whether the result type is Boolean rather than merely truthy. True: the
  answer is true or false, never the found value and never nil."
  true)

(t/ann key-type-constrains-the-question? Boolean)
(def key-type-constrains-the-question?
  "Whether the key must share the map's key type. False: any value may be
  asked about, and one of the wrong type simply answers false."
  false)

(t/ann indexed-target-asks-about-indices? Boolean)
(def indexed-target-asks-about-indices?
  "Whether an indexed target is asked about positions rather than elements.
  True: for a vector the key ranges over 0..count-1, so an element that is not
  also a valid index answers false."
  true)
