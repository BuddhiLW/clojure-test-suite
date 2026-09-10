(ns ^:typed.clojure clojure.core-model.last-typed
  "Static signatures for `last`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the value returned, or the simple name of the
  class thrown. The throw branch is data, not an effect."
  (t/U '{:value t/Any} '{:throw t/Str}))

(t/ann reference-last (t/All [x] [(t/Option (t/Seqable x)) :-> (t/Option x)]))
(defn reference-last
  "`last` over the seqable domain. Same shape as `first`: a nil input is
  accepted and an absent final element is nil."
  [coll]
  (last coll))

(t/ann observe-last (t/All [x] [(t/Option (t/Seqable x)) :-> Outcome]))
(defn observe-last
  "`reference-last` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to inputs outside the
  seqable domain, which this signature excludes."
  [coll]
  {:value (reference-last coll)})

(t/ann last-is-constant-time? Boolean)
(def last-is-constant-time?
  "Whether the signature promises anything about cost. False: `last` walks the
  whole seq, and no part of the type says so."
  false)
