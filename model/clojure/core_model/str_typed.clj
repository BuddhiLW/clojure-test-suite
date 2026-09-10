(ns ^:typed.clojure clojure.core-model.str-typed
  "Static signatures for `str`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A recorded call result: the host's string, or the class name it threw."
  (t/U t/Str '[t/Kw (t/Nilable t/Str)]))

(t/ann reference-str (t/IFn [:-> t/Str]
                            [t/Any :-> t/Str]
                            [t/Any t/Any :-> t/Str]))
(defn reference-str
  "`str` at the arities a static host can name. The argument type is
  unconstrained and the result is always a string: nil, a scalar and a
  collection are all accepted, each with its own printed form."
  ([] (str))
  ([x] (str x))
  ([x y] (str x y)))

(t/ann characterize-str [t/Any :-> Outcome])
(defn characterize-str
  "The total outcome of one `str` call. The thrown arm is unreachable on this
  host — `str` is total into strings — and is named so a dialect that does
  throw is recorded rather than crashing the emit."
  [x]
  (try (str x)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(t/ann nil-erases? Boolean)
(def nil-erases?
  "Whether a nil argument contributes nothing to the result. True: `(str)`,
  `(str nil)` and `(str nil nil)` are all the empty string."
  true)
