(ns ^:typed.clojure clojure.core-model.get-typed
  "Static signatures for `get`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-get (t/All [k v] (t/IFn [(t/Map k v) k :-> (t/Nilable v)]
                                         [(t/Map k v) k v :-> v])))
(defn reference-get
  "`get` over a homogeneous map. The arity-2 result is nilable whatever the
  value type, because a miss and a stored nil share one answer."
  ([m k] (get m k))
  ([m k not-found] (get m k not-found)))

(t/defalias Throw
  "The recorded shape of a host exception: the simple class name, as data."
  (t/HMap :mandatory {:throws t/Str} :complete? true))

(t/ann get-outcome (t/All [k v] (t/IFn [(t/Map k v) k :-> (t/U (t/Nilable v) Throw)]
                                       [(t/Map k v) k v :-> (t/U v Throw)])))
(defn get-outcome
  "`reference-get` as a total observation. Only the value branch is statically
  reachable — the throw branch belongs to targets outside the associative
  domain, which this signature excludes."
  ([m k] (reference-get m k))
  ([m k not-found] (reference-get m k not-found)))

(t/ann lookup-is-nilable-without-default? Boolean)
(def lookup-is-nilable-without-default?
  "Whether the arity-2 result type must admit nil for every value type. True:
  an absent key answers nil regardless of what the map stores."
  true)

(t/ann default-narrows-the-result? Boolean)
(def default-narrows-the-result?
  "Whether supplying a default removes nil from the result type. False: a key
  present with a nil value still answers nil, so the default cannot narrow it."
  false)
