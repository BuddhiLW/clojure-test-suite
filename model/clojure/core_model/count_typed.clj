(ns ^:typed.clojure clojure.core-model.count-typed
  "Static signatures for `count`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the value returned, or the simple name of the
  class thrown. The throw branch is data, not an effect."
  (t/U '{:value t/Any} '{:throw t/Str}))

(t/ann reference-count [(t/Option (t/Seqable t/Any)) :-> t/AnyInteger])
(defn reference-count
  "`count` over the seqable domain. nil is in the domain and counts zero."
  [coll]
  (count coll))

(t/ann observe-count [(t/Option (t/Seqable t/Any)) :-> Outcome])
(defn observe-count
  "`reference-count` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to inputs outside the
  seqable domain, which this signature excludes."
  [coll]
  {:value (reference-count coll)})

(t/ann non-negativity-is-in-the-type? Boolean)
(def non-negativity-is-in-the-type?
  "Whether the return type rules out a negative count. False: the static type
  is an integer, so the non-negativity law is a runtime obligation only."
  false)
