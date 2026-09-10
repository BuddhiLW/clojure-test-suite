(ns ^:typed.clojure clojure.core-model.parse-long-typed
  "Static signatures for `parse-long`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias LongOutcome
  "A total observation of one call: the parsed long, nil, or a marker naming the
  class that was thrown."
  (t/U nil t/Int (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-parse-long [t/Str :-> (t/Nilable t/Int)])
(defn reference-parse-long
  "`parse-long` over its declared domain. Partial into the longs: text outside
  the grammar, and text outside the long range, are both nil rather than throws."
  [s]
  (parse-long s))

(t/ann observed-parse-long [t/Str :-> LongOutcome])
(defn observed-parse-long
  "The same call observed totally, so a throw is a value the snapshot can hold."
  [s]
  (try (parse-long s) (catch Throwable e {:throws (.getName (class e))})))

(t/ann total-into-longs? Boolean)
(def total-into-longs?
  "Whether every string in the domain yields a long. False: nil is in the
  codomain, and it is the answer for overflow as well as for bad grammar."
  false)

(t/ann accepts-non-strings? Boolean)
(def accepts-non-strings?
  "Whether the argument type may widen past String. False: a non-string argument
  is rejected at run time rather than coerced."
  false)
