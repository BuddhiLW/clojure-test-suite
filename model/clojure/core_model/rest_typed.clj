(ns ^:typed.clojure clojure.core-model.rest-typed
  "Static signatures for `rest`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the value returned, or the simple name of the
  class thrown. The throw branch is data, not an effect."
  (t/U '{:value t/Any} '{:throw t/Str}))

(t/ann reference-rest (t/All [x] [(t/Option (t/Seqable x)) :-> (t/ASeq x)]))
(defn reference-rest
  "`rest` over the seqable domain. The return type carries no nil: exhaustion
  is spelled as the empty seq."
  [coll]
  (rest coll))

(t/ann observe-rest (t/All [x] [(t/Option (t/Seqable x)) :-> Outcome]))
(defn observe-rest
  "`reference-rest` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to inputs outside the
  seqable domain, which this signature excludes."
  [coll]
  {:value (reference-rest coll)})

(t/ann tail-may-be-nil? Boolean)
(def tail-may-be-nil?
  "Whether the return type admits nil. False: `rest` is total into a seq, which
  is what separates it from `next`."
  false)
