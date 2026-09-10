(ns clojure.core-model.parse-long
  "Behaviour model for `clojure.core/parse-long`. Emits `model/golden/parse-long.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def LongText
  "The domain `parse-long` is defined on. Every other argument throws."
  :string)

(def Outcome
  "A total observation of one call: the parsed long, nil, or a marker naming the
  class that was thrown."
  [:or :nil :int [:map [:throws :string]]])

(defn reference-parse-long
  "`parse-long`, delegated to the host so the model characterizes rather than restates."
  [s]
  (parse-long s))

(m/=> reference-parse-long [:=> [:cat LongText] [:maybe :int]])

(defn- observe
  "The value `thunk` yields, or a marker naming the class it threw."
  [thunk]
  (try (thunk) (catch Throwable t {:throws (.getName (class t))})))

(defn observed-parse-long
  "`reference-parse-long` over any argument at all: an off-domain argument is
  recorded as a throw marker instead of aborting the snapshot."
  [x]
  (observe #(reference-parse-long x)))

(m/=> observed-parse-long [:=> [:cat :any] Outcome])

(defn- throw-marker?
  "True when the outcome records a throw rather than a returned value."
  [o]
  (and (map? o) (contains? o :throws)))

(defn- canonical-round-trip?
  "Every long `parse-long` returns is reproduced by parsing that long's own
  rendering. nil and a throw marker are accepted outcomes."
  [o]
  (cond
    (nil? o)          true
    (throw-marker? o) true
    (int? o)          (= o (reference-parse-long (str o)))
    :else             false))

(def ^:private long-text-gen
  (gen/one-of [(gen/fmap str gen/large-integer)
               (gen/fmap #(str "+" %) gen/nat)
               gen/string-alphanumeric
               (gen/elements ["" "  " " 1" "1 " "+1" "-0" "007" "0x10" "1e3" "1L"
                              "١٢٣" "9223372036854775807"
                              "9223372036854775808" "-9223372036854775809"])
               gen/simple-type]))

(deftrifecta parse-long-behaviour
  clojure.core-model.parse-long/observed-parse-long
  {:golden-path "model/golden/parse-long.edn"
   :cases       {:empty              ""
                 :blank              "  "
                 :zero               "0"
                 :simple             "42"
                 :explicit-plus      "+12"
                 :negative           "-1000"
                 :negative-zero      "-0"
                 :leading-zeros      "007"
                 :leading-space      " 42"
                 :trailing-space     "42 "
                 :leading-tab        "\t42"
                 :hex                "0x1F"
                 :long-suffix        "1L"
                 :exponent           "1e3"
                 :decimal            "0.0"
                 :double-sign        "+-5"
                 :bare-plus          "+"
                 :underscored        "1_000"
                 :arabic-indic       "١٢٣"
                 :fullwidth          "＋４２"
                 :long-max           "9223372036854775807"
                 :long-max-inc       "9223372036854775808"
                 :long-min           "-9223372036854775808"
                 :long-min-dec       "-9223372036854775809"
                 :non-string-long    1000
                 :non-string-double  0.0
                 :non-string-char    \a
                 :non-string-keyword :key
                 :non-string-vector  []
                 :nil                nil}
   :xf          pr-str
   :gen         long-text-gen
   :pred        canonical-round-trip?
   :num-tests   500
   :mutations   [["identity"         (fn [x] x)]
                 ["always-nil"       (fn [_] nil)]
                 ["never-throws"     (fn [x] (try (parse-long x) (catch Throwable _ nil)))]
                 ["trims-whitespace" (fn [x] (observe #(parse-long (str/trim x))))]
                 ["drops-sign"       (fn [x] (observe #(parse-long (str/replace x #"^[+-]" ""))))]
                 ["radix-16"         (fn [x] (observe #(Long/parseLong ^String x 16)))]]})

(deftest parse-long-laws
  (testing "unparseable text is nil, never a throw"
    (is (every? #(nil? (reference-parse-long %))
                ["" "  " "foo" "1L" "0.0" "1e3" "+-5" "0x1F" "1_000" "+"])))
  (testing "surrounding whitespace is not trimmed"
    (is (nil? (reference-parse-long " 42")))
    (is (nil? (reference-parse-long "42 ")))
    (is (nil? (reference-parse-long "\t42"))))
  (testing "the long range is the boundary; overflow is nil, not a promotion"
    (is (= Long/MAX_VALUE (reference-parse-long "9223372036854775807")))
    (is (nil? (reference-parse-long "9223372036854775808")))
    (is (= Long/MIN_VALUE (reference-parse-long "-9223372036854775808")))
    (is (nil? (reference-parse-long "-9223372036854775809"))))
  (testing "a non-string argument throws instead of answering nil"
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-long 1000)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-long nil)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-long \a))))
  (testing "non-ASCII decimal digits are accepted"
    (is (= 123 (reference-parse-long "١٢٣")))))
