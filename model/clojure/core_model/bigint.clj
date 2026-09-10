(ns clojure.core-model.bigint
  "Behaviour model for `clojure.core/bigint`. Emits `model/golden/bigint.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Coercible
  "The domain `bigint` is defined on: any number, or a string of digits. Anything
  else reaches the BigInteger constructor and throws."
  [:or number? :string])

(def Promoted
  "What `bigint` answers with: an arbitrary-precision integer."
  [:fn #(instance? clojure.lang.BigInt %)])

(def Outcome
  "A total observation of one call: the promoted integer, or a marker naming the
  class that was thrown."
  [:or Promoted [:map [:throws :string]]])

(defn reference-bigint
  "`bigint`, delegated to the host so the model characterizes rather than restates."
  [x]
  (bigint x))

(m/=> reference-bigint [:=> [:cat Coercible] Promoted])

(defn- observe
  "The value `thunk` yields, or a marker naming the class it threw."
  [thunk]
  (try (thunk) (catch Throwable t {:throws (.getName (class t))})))

(defn observed-bigint
  "`reference-bigint` over any argument at all: an off-domain argument, and an
  in-domain argument the host still refuses, are recorded as throw markers."
  [x]
  (observe #(reference-bigint x)))

(m/=> observed-bigint [:=> [:cat :any] Outcome])

(defn- throw-marker?
  "True when the outcome records a throw rather than a returned value."
  [o]
  (and (map? o) (contains? o :throws)))

(defn- canonical-round-trip?
  "Every promoted integer re-parses from its own decimal rendering to the same
  value, so no precision is lost on the way out. A throw marker is accepted."
  [o]
  (cond
    (throw-marker? o) true
    (instance? clojure.lang.BigInt o) (= o (reference-bigint (str o)))
    :else false))

(def ^:private coercible-gen
  (gen/one-of [gen/large-integer
               gen/double
               gen/ratio
               (gen/fmap str gen/large-integer)
               gen/string-alphanumeric
               (gen/elements [0.5 -0.5 1/2 -3/2 1M 1.5M 1N ##Inf ##NaN
                              Long/MAX_VALUE Long/MIN_VALUE Double/MAX_VALUE
                              "1" "-1" "" "abc" "0x1F" "1.0"])
               gen/simple-type]))

(deftrifecta bigint-behaviour
  clojure.core-model.bigint/observed-bigint
  {:golden-path "model/golden/bigint.edn"
   :cases       {:zero               0
                 :one                1
                 :minus-one          -1
                 :double-one         1.0
                 :double-minus-one   -1.0
                 :negative-zero      -0.0
                 :half               0.5
                 :minus-half         -0.5
                 :nearly-two         1.9
                 :nearly-minus-two   -1.9
                 :ratio              1/2
                 :ratio-negative     -3/2
                 :ratio-improper     3/2
                 :text-one           "1"
                 :text-plus-one      "+1"
                 :text-minus-one     "-1"
                 :text-decimal       "1.0"
                 :text-hex           "0x1F"
                 :text-empty         ""
                 :text-blank         "  1  "
                 :text-word          "abc"
                 :text-huge          "123456789012345678901234567890"
                 :long-max           9223372036854775807
                 :long-min           -9223372036854775808
                 :promoted-long-max  9223372036854775808N
                 :big-double         1.0E30
                 :max-double         1.7976931348623157E308
                 :infinity           ##Inf
                 :negative-infinity  ##-Inf
                 :nan                ##NaN
                 :bigdec             1M
                 :bigdec-fraction    1.5M
                 :bigint             1N
                 :big-integer        (biginteger 7)
                 :boolean            true
                 :char               \a
                 :keyword            :key
                 :vector             []
                 :nil                nil}
   :xf          pr-str
   :gen         coercible-gen
   :pred        canonical-round-trip?
   :num-tests   500
   :mutations   [["identity"                   (fn [x] x)]
                 ["always-zero"                (fn [_] 0N)]
                 ["never-throws"               (fn [x] (try (bigint x) (catch Throwable _ nil)))]
                 ["rounds-instead-of-truncating"
                  (fn [x] (observe #(bigint (if (number? x) (Math/round (double x)) x))))]
                 ["string-radix-16"
                  (fn [x] (observe #(if (string? x)
                                      (bigint (BigInteger. ^String x 16))
                                      (bigint x))))]
                 ["long-truncating"            (fn [x] (observe #(bigint (long (reference-bigint x)))))]]})

(deftest bigint-laws
  (testing "fractional input truncates toward zero, it does not round"
    (is (= 0N (reference-bigint 0.5)))
    (is (= 0N (reference-bigint -0.5)))
    (is (= 1N (reference-bigint 1.9)))
    (is (= -1N (reference-bigint -1.9)))
    (is (= 0N (reference-bigint 1/2)))
    (is (= -1N (reference-bigint -3/2)))
    (is (= 1N (reference-bigint 1.5M))))
  (testing "the string path is BigInteger's grammar, not the reader's"
    (is (= 1N (reference-bigint "1")))
    (is (= 1N (reference-bigint "+1")))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigint "1.0")))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigint "0x1F")))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigint "  1  ")))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigint ""))))
  (testing "promotion past the long range is exact, in both directions"
    (is (= 9223372036854775808N (reference-bigint (inc' Long/MAX_VALUE))))
    (is (= 9223372036854775808N (inc (reference-bigint Long/MAX_VALUE))))
    (is (= -9223372036854775809N (dec (reference-bigint Long/MIN_VALUE)))))
  (testing "a double carries only the precision it had; it is not a decimal-text parse"
    (is (= (reference-bigint 1.0E30)
           (reference-bigint "1000000000000000000000000000000"))))
  (testing "the non-finite doubles and the non-numbers throw, with different classes"
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigint ##Inf)))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigint ##NaN)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-bigint true)))
    (is (= {:throws "java.lang.NullPointerException"} (observed-bigint nil))))
  (testing "the result is a BigInt, not a boxed long"
    (is (instance? clojure.lang.BigInt (reference-bigint 0)))
    (is (instance? clojure.lang.BigInt (reference-bigint 0.0)))
    (is (instance? clojure.lang.BigInt (reference-bigint (biginteger 7))))))
