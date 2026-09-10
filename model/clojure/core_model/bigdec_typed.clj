(ns ^:typed.clojure clojure.core-model.bigdec-typed
  "Static signatures for `bigdec`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias BigDecOutcome
  "A total observation of one call: the arbitrary-precision decimal, or a marker
  naming the class that was thrown."
  (t/U java.math.BigDecimal (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-bigdec [(t/U t/Num t/Str) :-> java.math.BigDecimal])
(defn reference-bigdec
  "`bigdec` over its declared domain. Total in the type but not in fact: the
  non-finite doubles, a non-terminating ratio and text outside BigDecimal's
  grammar all throw."
  [x]
  (bigdec x))

(t/ann observed-bigdec [(t/U t/Num t/Str) :-> BigDecOutcome])
(defn observed-bigdec
  "The same call observed totally, so a throw is a value the snapshot can hold."
  [x]
  (try (bigdec x) (catch Throwable e {:throws (.getName (class e))})))

(t/ann scale-is-part-of-the-value? Boolean)
(def scale-is-part-of-the-value?
  "Whether the result carries a scale the type does not mention. True: 1M and
  1.0M compare equal yet print differently, so equality alone cannot witness a
  conforming implementation."
  true)

(t/ann double-conversion-is-exact? Boolean)
(def double-conversion-is-exact?
  "Whether a double converts to its exact binary value. False: it goes through
  the double's own shortest rendering, so 0.1 becomes 0.1M and not the full
  binary expansion."
  false)
