(ns ^:typed.clojure clojure.core-model.parse-uuid-typed
  "Static signatures for `parse-uuid`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias UuidOutcome
  "A total observation of one call: the parsed UUID, nil, or a marker naming the
  class that was thrown."
  (t/U nil java.util.UUID (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-parse-uuid [t/Str :-> (t/Nilable java.util.UUID)])
(defn reference-parse-uuid
  "`parse-uuid` over its declared domain. The accepted grammar is the host's, not
  the canonical 8-4-4-4-12: five hyphen-separated hex groups of any length parse."
  [s]
  (parse-uuid s))

(t/ann observed-parse-uuid [t/Str :-> UuidOutcome])
(defn observed-parse-uuid
  "The same call observed totally, so a throw is a value the snapshot can hold."
  [s]
  (try (parse-uuid s) (catch Throwable e {:throws (.getName (class e))})))

(t/ann canonical-rendering [java.util.UUID :-> t/Str])
(defn canonical-rendering
  "The rendering a parsed UUID prints as: always lower-case 8-4-4-4-12, whatever
  the accepted input looked like."
  [u]
  (str u))

(t/ann accepts-only-canonical-form? Boolean)
(def accepts-only-canonical-form?
  "Whether the input must already be 8-4-4-4-12. False on the JVM: short groups
  are left-padded and an oversized group is truncated to its low bits."
  false)

(t/ann off-domain-throw-is-shared-with-parse-long? Boolean)
(def off-domain-throw-is-shared-with-parse-long?
  "Whether a non-string argument fails the way the rest of the parse family
  fails. False: this one reaches a cast and a null deref instead of the
  family's explicit argument check."
  false)
