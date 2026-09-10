(ns ^:typed.clojure clojure.core-model.subs-typed
  "Static signatures for `subs`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A recorded call result: the host's substring, or the class name it threw."
  (t/U t/Str '[t/Kw (t/Nilable t/Str)]))

(t/ann reference-subs (t/IFn [t/Str t/Int :-> t/Str]
                             [t/Str t/Int t/Int :-> t/Str]))
(defn reference-subs
  "`subs` at both its arities. The index type is static; the index RANGE is
  not, so a well-typed call can still leave the string."
  ([s start] (subs s start))
  ([s start end] (subs s start end)))

(t/ann characterize-subs [t/Str t/Int (t/Nilable t/Int) :-> Outcome])
(defn characterize-subs
  "The total outcome of one `subs` call, the two-argument arity written as a
  nil end index. The thrown arm is reachable for every index outside
  [0, (count s)] — bounds are checked, never clamped."
  [s start end]
  (try (if end (subs s start end) (subs s start))
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(t/ann bounds-checked? Boolean)
(def bounds-checked?
  "Whether an out-of-range index is refused rather than clamped. True: no
  static signature makes `subs` total over its declared index type."
  true)
