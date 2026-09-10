(ns ^:typed.clojure clojure.core-model.float-typed
  "Static signatures for `float`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias FloatOutcome
  "A total observation of one call: the narrowed value, or a marker naming the
  class that was thrown."
  (t/U t/Num (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-float [t/Num :-> t/Num])
(defn reference-float
  "`float` over the numeric tower. The static signature is total; the run-time
  one is not, because a magnitude beyond the float range is refused — including
  the infinities, which NaN does not join."
  [x]
  (float x))

(t/ann probe-float [t/Num :-> FloatOutcome])
(defn probe-float
  "The same call observed totally, so a refusal is a value the snapshot can hold."
  [x]
  (try (float x) (catch Throwable e {:throws (.getName (class e))})))

(t/ann refuses-out-of-range? Boolean)
(def refuses-out-of-range?
  "Whether a magnitude beyond the float range is refused. True, the infinities
  included — where `double` saturates the same input instead."
  true)

(t/ann refuses-underflow? Boolean)
(def refuses-underflow?
  "Whether a magnitude below the smallest subnormal is refused. False: it
  underflows to zero silently, so the loss is at the small end, not the large."
  false)

(t/ann accepts-nan? Boolean)
(def accepts-nan?
  "Whether NaN is a legal argument. True: it narrows to a float NaN, even though
  the infinities beside it are refused."
  true)
