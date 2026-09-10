(ns ^:typed.clojure clojure.core-model.long-typed
  "Static signatures for `long`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias LongOutcome
  "A total observation of one call: the narrowed value, or a marker naming the
  class that was thrown."
  (t/U t/Int (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-long [t/Num :-> t/Int])
(defn reference-long
  "`long` over the numeric tower — the shape a static host can state. The static
  signature is total; the run-time one is not, because a magnitude outside the
  64-bit range is refused. The static domain is also narrower than the run-time
  one, which admits characters."
  [x]
  (long x))

(t/ann probe-long [t/Num :-> LongOutcome])
(defn probe-long
  "The same call observed totally, so a refusal is a value the snapshot can hold."
  [x]
  (try (long x) (catch Throwable e {:throws (.getName (class e))})))

(t/ann truncates-toward-zero? Boolean)
(def truncates-toward-zero?
  "Whether `long` truncates toward zero rather than flooring. True: -3.7 narrows
  to -3, not to -4."
  true)

(t/ann wraps-out-of-range? Boolean)
(def wraps-out-of-range?
  "Whether a magnitude outside the 64-bit range wraps around. False: it is
  refused, which is what separates `long` from `unchecked-long`."
  false)

(t/ann range-check-is-exact? Boolean)
(def range-check-is-exact?
  "Whether the range check on a floating argument is exact. False: it runs in
  double precision, so the double nearest 2^63 passes it and then saturates to
  the largest long instead of being refused."
  false)
