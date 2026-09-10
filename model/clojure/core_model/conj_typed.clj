(ns ^:typed.clojure clojure.core-model.conj-typed
  "Static signatures for `conj`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Throw
  "The recorded shape of a host exception: the simple class name, as data."
  (t/HMap :mandatory {:throws t/Str} :complete? true))

(t/ann reference-conj (t/All [x] [(t/Vec x) x :-> (t/Vec x)]))
(defn reference-conj
  "`conj` over a vector, the one target whose result type is closed under the
  element type. The end it grows at is invisible to the checker."
  [coll x]
  (conj coll x))

(t/ann conj-outcome (t/All [x] [(t/Vec x) x :-> (t/U (t/Vec x) Throw)]))
(defn conj-outcome
  "`reference-conj` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to the map and scalar
  targets, which this signature excludes."
  [coll x]
  (reference-conj coll x))

(t/ann target-type-decides-the-end? Boolean)
(def target-type-decides-the-end?
  "Whether the growth end is a property of the target's type. True: a vector
  grows at the back and a list at the front, and no arity-level signature
  distinguishes the two results, so the order law is a runtime obligation."
  true)

(t/ann map-item-must-be-a-pair? Boolean)
(def map-item-must-be-a-pair?
  "Whether a map target constrains the item beyond its type. True: the item
  must be a two-element entry, so a bare number is refused at runtime by a
  signature that admits it."
  true)

(t/ann nil-target-yields-a-list? Boolean)
(def nil-target-yields-a-list?
  "Whether nil is in the domain and produces a list. True: nil is treated as
  the empty list, so the result type is never nilable."
  true)
