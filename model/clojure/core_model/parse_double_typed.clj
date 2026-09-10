(ns ^:typed.clojure clojure.core-model.parse-double-typed
  "Static signatures for `parse-double`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias DoubleOutcome
  "A total observation of one call: the parsed double, nil, or a marker naming
  the class that was thrown."
  (t/U nil Double (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-parse-double [t/Str :-> (t/Nilable Double)])
(defn reference-parse-double
  "`parse-double` over its declared domain. The codomain is the whole double
  line: out-of-range magnitudes saturate to an infinity instead of answering nil."
  [s]
  (parse-double s))

(t/ann observed-parse-double [t/Str :-> DoubleOutcome])
(defn observed-parse-double
  "The same call observed totally, so a throw is a value the snapshot can hold."
  [s]
  (try (parse-double s) (catch Throwable e {:throws (.getName (class e))})))

(t/ann codomain-excludes-non-finite? Boolean)
(def codomain-excludes-non-finite?
  "Whether the result is always finite. False: the infinities and NaN are all
  reachable, from `Infinity`, `-Infinity`, `NaN` and from overflowing exponents."
  false)

(t/ann trims-surrounding-whitespace? Boolean)
(def trims-surrounding-whitespace?
  "Whether leading and trailing whitespace is stripped before parsing. True,
  which is the opposite of `parse-long`."
  true)
