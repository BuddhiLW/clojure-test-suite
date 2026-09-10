(ns ^:typed.clojure clojure.core-model.format-typed
  "Static signatures for `format`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A recorded call result: the host's formatted string, or the class it threw."
  (t/U t/Str '[t/Kw (t/Nilable t/Str)]))

(t/ann reference-format (t/IFn [t/Str :-> t/Str]
                               [t/Str t/Any :-> t/Str]
                               [t/Str t/Any t/Any :-> t/Str]))
(defn reference-format
  "`format` at the arities a static host can name. The argument types are
  unconstrained because the conversion, not the signature, decides what each
  argument may be — the agreement between the two is a run-time check."
  ([fmt] (format fmt))
  ([fmt a] (format fmt a))
  ([fmt a b] (format fmt a b)))

(t/ann characterize-format [t/Str t/Any :-> Outcome])
(defn characterize-format
  "The total outcome of one `format` call. The thrown arm is reachable from a
  perfectly well-typed call: a conversion that does not match its argument, a
  missing argument and an unknown conversion all fail at run time."
  [fmt a]
  (try (format fmt a)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(t/ann conversions-checked-at-run-time? Boolean)
(def conversions-checked-at-run-time?
  "Whether the format string is opaque to the type system. True: no signature
  relates the conversions in the string to the types of the arguments."
  true)
