(ns ^:typed.clojure clojure.core-model.empty-qmark-typed
  "Static signatures for `empty?`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/ann reference-empty? [(t/Seqable t/Any) :-> t/Bool])
(defn reference-empty?
  "`empty?` over anything seqable. Nilable, because nil is seqable and empty."
  [coll]
  (empty? coll))

(t/defalias Throw
  "The recorded shape of a host exception: the simple class name, as data."
  (t/HMap :mandatory {:throws t/Str} :complete? true))

(t/ann empty-outcome [(t/Seqable t/Any) :-> (t/U t/Bool Throw)])
(defn empty-outcome
  "`reference-empty?` as a total observation. Only the boolean branch is
  statically reachable — the throw branch belongs to the non-seqable scalars,
  which this signature excludes."
  [coll]
  (reference-empty? coll))

(t/ann accepts-nil? Boolean)
(def accepts-nil?
  "Whether nil is in the domain. True: nil is seqable and answers true."
  true)

(t/ann requires-a-counted-collection? Boolean)
(def requires-a-counted-collection?
  "Whether the argument must be counted. False: the answer is driven by `seq`,
  so an unrealized lazy seq that yields nothing is empty without being counted."
  false)

(t/ann accepts-a-scalar? Boolean)
(def accepts-a-scalar?
  "Whether a non-seqable scalar is in the domain. False: a number, a keyword or
  a boolean is refused rather than treated as an empty collection."
  false)
