(ns ^:typed.clojure clojure.core-model.next-typed
  "Static signatures for `next`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the value returned, or the simple name of the
  class thrown. The throw branch is data, not an effect."
  (t/U '{:value t/Any} '{:throw t/Str}))

(t/ann reference-next (t/All [x] [(t/Option (t/Seqable x)) :-> (t/Option (t/NonEmptyASeq x))]))
(defn reference-next
  "`next` over the seqable domain. The return type carries both facts that
  distinguish it from `rest`: it may be nil, and when it is not, it is
  non-empty."
  [coll]
  (next coll))

(t/ann observe-next (t/All [x] [(t/Option (t/Seqable x)) :-> Outcome]))
(defn observe-next
  "`reference-next` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to inputs outside the
  seqable domain, which this signature excludes."
  [coll]
  {:value (reference-next coll)})

(t/ann tail-may-be-nil? Boolean)
(def tail-may-be-nil?
  "Whether the return type admits nil. True: `next` collapses exhaustion to
  nil, which is what separates it from `rest`."
  true)
