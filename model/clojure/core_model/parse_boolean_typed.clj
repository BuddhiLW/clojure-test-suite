(ns ^:typed.clojure clojure.core-model.parse-boolean-typed
  "Static signatures for `parse-boolean`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias BooleanOutcome
  "A total observation of one call: the parsed boolean, nil, or a marker naming
  the class that was thrown."
  (t/U nil Boolean (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-parse-boolean [t/Str :-> (t/Nilable Boolean)])
(defn reference-parse-boolean
  "`parse-boolean` over its declared domain. The nil arm is not a third truth
  value: it means unrecognized, and it is distinct from `false`."
  [s]
  (parse-boolean s))

(t/ann observed-parse-boolean [t/Str :-> BooleanOutcome])
(defn observed-parse-boolean
  "The same call observed totally, so a throw is a value the snapshot can hold."
  [s]
  (try (parse-boolean s) (catch Throwable e {:throws (.getName (class e))})))

(t/ann recognized-literals (t/Vec t/Str))
(def recognized-literals
  "The complete set of strings that parse. Matching is exact: case-sensitive and
  untrimmed."
  ["true" "false"])

(t/ann unrecognized-collapses-to-false? Boolean)
(def unrecognized-collapses-to-false?
  "Whether an unrecognized string answers `false`. False: it answers nil, so a
  caller that only tests truthiness cannot tell the two apart."
  false)
