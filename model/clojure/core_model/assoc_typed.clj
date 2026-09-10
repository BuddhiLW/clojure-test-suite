(ns ^:typed.clojure clojure.core-model.assoc-typed
  "Static signatures for `assoc`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-assoc (t/All [k v] [(t/Map k v) k v :-> (t/Map k v)]))
(defn reference-assoc
  "`assoc` over a homogeneous map, the one arity whose result type is closed
  under the argument types."
  [m k v]
  (assoc m k v))

(t/defalias Throw
  "The recorded shape of a host exception: the simple class name, as data."
  (t/HMap :mandatory {:throws t/Str} :complete? true))

(t/ann assoc-outcome (t/All [k v] [(t/Map k v) k v :-> (t/U (t/Map k v) Throw)]))
(defn assoc-outcome
  "`reference-assoc` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to the indexed and
  non-associative targets, which this signature excludes."
  [m k v]
  (reference-assoc m k v))

(t/ann widens-the-value-type? Boolean)
(def widens-the-value-type?
  "Whether a heterogeneous write widens the map's value type. True: writing a
  value of a new type yields a map over the union, so the signature above holds
  only for a value already in the map's value type."
  true)

(t/ann vector-index-must-be-in-range? Boolean)
(def vector-index-must-be-in-range?
  "Whether an indexed target constrains the key beyond its type. True: an
  integer index is still refused unless it is in range or exactly at the end,
  which no arity-level signature can express."
  true)

(t/ann nil-target-yields-a-map? Boolean)
(def nil-target-yields-a-map?
  "Whether nil is in the domain and produces a map. True: nil is treated as an
  empty map, so the result type is never nilable."
  true)
