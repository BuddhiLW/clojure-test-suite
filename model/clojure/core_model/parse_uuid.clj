(ns clojure.core-model.parse-uuid
  "Behaviour model for `clojure.core/parse-uuid`. Emits `model/golden/parse-uuid.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def UuidText
  "The domain `parse-uuid` is defined on. Every other argument throws."
  :string)

(def Outcome
  "A total observation of one call: the parsed UUID, nil, or a marker naming the
  class that was thrown."
  [:or :nil :uuid [:map [:throws :string]]])

(defn reference-parse-uuid
  "`parse-uuid`, delegated to the host so the model characterizes rather than restates."
  [s]
  (parse-uuid s))

(m/=> reference-parse-uuid [:=> [:cat UuidText] [:maybe :uuid]])

(defn- observe
  "The value `thunk` yields, or a marker naming the class it threw."
  [thunk]
  (try (thunk) (catch Throwable t {:throws (.getName (class t))})))

(defn observed-parse-uuid
  "`reference-parse-uuid` over any argument at all: an off-domain argument is
  recorded as a throw marker instead of aborting the snapshot."
  [x]
  (observe #(reference-parse-uuid x)))

(m/=> observed-parse-uuid [:=> [:cat :any] Outcome])

(defn- throw-marker?
  "True when the outcome records a throw rather than a returned value."
  [o]
  (and (map? o) (contains? o :throws)))

(defn- canonical-round-trip?
  "Every UUID `parse-uuid` returns re-parses from its own canonical rendering to
  the same value. nil and a throw marker are accepted outcomes."
  [o]
  (cond
    (nil? o)          true
    (throw-marker? o) true
    (uuid? o)         (= o (reference-parse-uuid (str o)))
    :else             false))

(def ^:private uuid-text-gen
  (gen/one-of [(gen/fmap str gen/uuid)
               (gen/fmap #(str/upper-case (str %)) gen/uuid)
               gen/string-alphanumeric
               (gen/elements ["" "0" "df0993" "0-0-0-0-0" "12-34-56-78-9"
                              "0-0-0-0" "0-0-0-0-0-0" "-1-1-1-1-1"
                              "b6883c0a-0342-4007-9966-bc2dfa6b109e"
                              "b6883c0a-0342-4007-9966-bc2dfa6b109"
                              "g6883c0a-0342-4007-9966-bc2dfa6b109e"])
               gen/simple-type]))

(deftrifecta parse-uuid-behaviour
  clojure.core-model.parse-uuid/observed-parse-uuid
  {:golden-path "model/golden/parse-uuid.edn"
   :cases       {:empty               ""
                 :zero-text           "0"
                 :fragment            "df0993"
                 :canonical           "b6883c0a-0342-4007-9966-bc2dfa6b109e"
                 :canonical-upper     "B6883C0A-0342-4007-9966-BC2DFA6B109E"
                 :canonical-mixed     "B6883C0A-0342-4007-9966-BC2dfa6b109E"
                 :nil-uuid            "00000000-0000-0000-0000-000000000000"
                 :short-groups        "0-0-0-0-0"
                 :ragged-groups       "12-34-56-78-9"
                 :overlong-group      "5-4-3-DEADBEEF0002-9000000001"
                 :short-last-group    "b6883c0a-0342-4007-9966-bc2dfa6b109"
                 :four-groups         "0-0-0-0"
                 :six-groups          "0-0-0-0-0-0"
                 :empty-first-group   "-1-1-1-1-1"
                 :non-hex-digit       "g6883c0a-0342-4007-9966-bc2dfa6b109e"
                 :extra-leading-char  "ab6883c0a-0342-4007-9966-bc2dfa6b109e"
                 :extra-trailing-char "b6883c0a-0342-4007-9966-bc2dfa6b109eb"
                 :leading-space       " b6883c0a-0342-4007-9966-bc2dfa6b109e"
                 :trailing-space      "b6883c0a-0342-4007-9966-bc2dfa6b109e "
                 :braced              "{b6883c0a-0342-4007-9966-bc2dfa6b109e}"
                 :urn                 "urn:uuid:b6883c0a-0342-4007-9966-bc2dfa6b109e"
                 :non-string-long     1000
                 :non-string-double   0.0
                 :non-string-char     \a
                 :non-string-keyword  :key
                 :non-string-map      {}
                 :non-string-vector   []
                 :nil                 nil}
   :xf          pr-str
   :gen         uuid-text-gen
   :pred        canonical-round-trip?
   :num-tests   500
   :mutations   [["identity"        (fn [x] x)]
                 ["always-nil"      (fn [_] nil)]
                 ["never-throws"    (fn [x] (try (parse-uuid x) (catch Throwable _ nil)))]
                 ["case-sensitive"  (fn [x] (observe #(when (= x (str/lower-case x)) (parse-uuid x))))]
                 ["strict-length"   (fn [x] (observe #(when (= 36 (count x)) (parse-uuid x))))]
                 ["tolerates-space" (fn [x] (observe #(parse-uuid (str/trim x))))]]})

(deftest parse-uuid-laws
  (testing "hex case is folded, so an upper-case rendering parses to the same value"
    (is (= (reference-parse-uuid "b6883c0a-0342-4007-9966-bc2dfa6b109e")
           (reference-parse-uuid "B6883C0A-0342-4007-9966-BC2dfa6b109E"))))
  (testing "the JVM accepts short groups and left-pads them"
    (is (= (reference-parse-uuid "00000000-0000-0000-0000-000000000000")
           (reference-parse-uuid "0-0-0-0-0")))
    (is (= (reference-parse-uuid "00000012-0034-0056-0078-000000000009")
           (reference-parse-uuid "12-34-56-78-9")))
    (is (= (reference-parse-uuid "b6883c0a-0342-4007-9966-0bc2dfa6b109")
           (reference-parse-uuid "b6883c0a-0342-4007-9966-bc2dfa6b109"))))
  (testing "an oversized group is truncated to its low 64 bits rather than rejected"
    (is (= (reference-parse-uuid "00000005-0004-0003-0002-009000000001")
           (reference-parse-uuid "5-4-3-DEADBEEF0002-9000000001"))))
  (testing "group count, stray characters and surrounding space are rejected as nil"
    (is (every? #(nil? (reference-parse-uuid %))
                ["" "0" "df0993" "0-0-0-0" "0-0-0-0-0-0" "-1-1-1-1-1"
                 "g6883c0a-0342-4007-9966-bc2dfa6b109e"
                 "ab6883c0a-0342-4007-9966-bc2dfa6b109e"
                 "b6883c0a-0342-4007-9966-bc2dfa6b109eb"
                 " b6883c0a-0342-4007-9966-bc2dfa6b109e"
                 "{b6883c0a-0342-4007-9966-bc2dfa6b109e}"])))
  (testing "the off-domain throw is NOT the parse family's IllegalArgumentException"
    (is (= {:throws "java.lang.NullPointerException"} (observed-parse-uuid nil)))
    (is (= {:throws "java.lang.ClassCastException"} (observed-parse-uuid 1000)))))
