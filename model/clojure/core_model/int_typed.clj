(ns ^:typed.clojure clojure.core-model.int-typed
  "Static signatures for `int`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias IntOutcome
  "A total observation of one call: the narrowed value, or a marker naming the
  class that was thrown."
  (t/U t/Int (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-int [t/Num :-> t/Int])
(defn reference-int
  "`int` over the numeric tower — the shape a static host can state. The static
  signature is total; the run-time one is not, because a magnitude outside the
  32-bit range is refused rather than narrowed. The static domain is also
  narrower than the run-time one, which admits characters."
  [x]
  (int x))

(t/ann probe-int [t/Num :-> IntOutcome])
(defn probe-int
  "The same call observed totally, so a refusal is a value the snapshot can hold."
  [x]
  (try (int x) (catch Throwable e {:throws (.getName (class e))})))

(t/ann truncates-toward-zero? Boolean)
(def truncates-toward-zero?
  "Whether `int` truncates toward zero rather than rounding. True: -3.7 narrows
  to -3, and 3.5 to 3."
  true)

(t/ann wraps-out-of-range? Boolean)
(def wraps-out-of-range?
  "Whether a magnitude outside the 32-bit range wraps around. False: it is
  refused, which is what separates `int` from `unchecked-int`."
  false)

(t/ann accepts-characters? Boolean)
(def accepts-characters?
  "Whether a character is a legal argument. True: it narrows through its code
  point, where `double` and `float` refuse the same input."
  true)
