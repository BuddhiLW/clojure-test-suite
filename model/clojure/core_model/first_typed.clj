(ns ^:typed.clojure clojure.core-model.first-typed
  "Static signatures for `first`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the value returned, or the simple name of the
  class thrown. The throw branch is data, not an effect."
  (t/U '{:value t/Any} '{:throw t/Str}))

(t/ann reference-first (t/All [x] [(t/Option (t/Seqable x)) :-> (t/Option x)]))
(defn reference-first
  "`first` over the seqable domain. nil-punning is in the type: a nil input is
  accepted and an absent head is nil."
  [coll]
  (first coll))

(t/ann observe-first (t/All [x] [(t/Option (t/Seqable x)) :-> Outcome]))
(defn observe-first
  "`reference-first` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to inputs outside the
  seqable domain, which this signature excludes."
  [coll]
  {:value (reference-first coll)})

(t/ann head-distinguishes-empty-from-nil-element? Boolean)
(def head-distinguishes-empty-from-nil-element?
  "Whether the return type separates 'no head' from 'a head that is nil'.
  False: both are `nil`, so `first` alone cannot decide emptiness."
  false)
