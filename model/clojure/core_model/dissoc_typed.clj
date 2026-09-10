(ns ^:typed.clojure clojure.core-model.dissoc-typed
  "Static signatures for `dissoc`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-dissoc (t/All [k v] [(t/Map k v) t/Any :-> (t/Map k v)]))
(defn reference-dissoc
  "`dissoc` over a map. The key argument is unconstrained: a key of the wrong
  type is a no-op, not a type error."
  [m k]
  (dissoc m k))

(t/defalias Throw
  "The recorded shape of a host exception: the simple class name, as data."
  (t/HMap :mandatory {:throws t/Str} :complete? true))

(t/ann dissoc-outcome (t/All [k v] [(t/Map k v) t/Any :-> (t/U (t/Map k v) Throw)]))
(defn dissoc-outcome
  "`reference-dissoc` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to the indexed and
  non-associative targets, which this signature excludes."
  [m k]
  (reference-dissoc m k))

(t/ann narrows-the-map-type? Boolean)
(def narrows-the-map-type?
  "Whether removal narrows the key or value type. False: the result is a map
  over the same types, only with fewer entries."
  false)

(t/ann accepts-an-indexed-target? Boolean)
(def accepts-an-indexed-target?
  "Whether a vector is in the domain. False: unlike `assoc`, `dissoc` refuses
  every indexed target, so the two are not inverse over vectors."
  false)

(t/ann nil-target-stays-nil? Boolean)
(def nil-target-stays-nil?
  "Whether nil survives as nil. True: `dissoc` returns nil for a nil target,
  where `assoc` would have produced a map, so the result type is nilable."
  true)
