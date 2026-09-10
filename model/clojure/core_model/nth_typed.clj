(ns ^:typed.clojure clojure.core-model.nth-typed
  "Static signatures for `nth`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A total observation of a call: the value returned, or the simple name of the
  class thrown. The throw branch is data, not an effect."
  (t/U '{:value t/Any} '{:throw t/Str}))

(t/ann reference-nth (t/All [x y]
                       (t/IFn [(t/Vec x) t/AnyInteger :-> x]
                              [(t/Vec x) t/AnyInteger y :-> (t/U x y)])))
(defn reference-nth
  "`nth` over the indexed domain. The 2-arity promises an element and hides the
  out-of-range failure; the 3-arity widens the result to include the not-found
  value."
  ([coll i] (nth coll i))
  ([coll i not-found] (nth coll i not-found)))

(t/ann observe-nth (t/All [x y]
                     (t/IFn [(t/Vec x) t/AnyInteger :-> Outcome]
                            [(t/Vec x) t/AnyInteger y :-> Outcome])))
(defn observe-nth
  "`reference-nth` as a total observation. Only the value branch is
  statically reachable — the throw branch belongs to out-of-range indices and
  to collections outside the indexed domain, neither of which this signature
  excludes at the type level."
  ([coll i] {:value (reference-nth coll i)})
  ([coll i not-found] {:value (reference-nth coll i not-found)}))

(t/ann out-of-range-is-in-the-type? Boolean)
(def out-of-range-is-in-the-type?
  "Whether the 2-arity return type admits the failure. False: it claims an
  element unconditionally, so the IndexOutOfBounds edge is invisible to the
  checker and must be carried by the golden baseline instead."
  false)
