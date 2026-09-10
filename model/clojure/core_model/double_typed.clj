(ns ^:typed.clojure clojure.core-model.double-typed
  "Static signatures for `double`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias DoubleOutcome
  "A total observation of one call: the widened value, or a marker naming the
  class that was thrown."
  (t/U t/Num (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-double [t/Num :-> t/Num])
(defn reference-double
  "`double` over the numeric tower. The static signature is total and so is the
  run-time one over that domain: every number widens, and a magnitude beyond the
  double range saturates to an infinity rather than being refused."
  [x]
  (double x))

(t/ann probe-double [t/Num :-> DoubleOutcome])
(defn probe-double
  "The same call observed totally, so a refusal is a value the snapshot can hold.
  Only a non-number reaches that branch."
  [x]
  (try (double x) (catch Throwable e {:throws (.getName (class e))})))

(t/ann refuses-out-of-range? Boolean)
(def refuses-out-of-range?
  "Whether a magnitude beyond the double range is refused. False: it saturates
  to an infinity, where `float` refuses the same input."
  false)

(t/ann accepts-characters? Boolean)
(def accepts-characters?
  "Whether a character is a legal argument. False: `double` takes numbers only,
  where `int` and `long` both coerce a character through its code point."
  false)

(t/ann preserves-long-precision? Boolean)
(def preserves-long-precision?
  "Whether widening a 64-bit integer preserves it. False: beyond 2^53 the low
  bits are dropped silently, so distinct longs land on one double."
  false)
