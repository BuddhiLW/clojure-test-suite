(ns ^:typed.clojure clojure.core-model.name-typed
  "Static signatures for `name`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias Outcome
  "A recorded call result: the host's name string, or the class name it threw."
  (t/U t/Str '[t/Kw (t/Nilable t/Str)]))

(t/ann reference-name [(t/U t/Str t/Kw t/Sym) :-> t/Str])
(defn reference-name
  "`name` over the three types it accepts. A string is its own name; a
  qualified keyword or symbol yields the name half only."
  [x]
  (name x))

(t/ann characterize-name [(t/U t/Str t/Kw t/Sym) :-> Outcome])
(defn characterize-name
  "The total outcome of one `name` call. The thrown arm is unreachable inside
  the declared domain and is what a dynamically typed host reaches on nil or
  on any value that is not Named."
  [x]
  (try (name x)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(t/ann total-into-strings? Boolean)
(def total-into-strings?
  "Whether `name` lands in the strings for every argument in its domain. True:
  the empty name is the empty string, never nil."
  true)
