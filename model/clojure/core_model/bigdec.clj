(ns clojure.core-model.bigdec
  "Behaviour model for `clojure.core/bigdec`. Emits `model/golden/bigdec.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Coercible
  "The domain `bigdec` is defined on: any number, or a string in BigDecimal's
  grammar. Anything else reaches the BigDecimal constructor and throws."
  [:or number? :string])

(def Outcome
  "A total observation of one call: the arbitrary-precision decimal, or a marker
  naming the class that was thrown."
  [:or decimal? [:map [:throws :string]]])

(defn reference-bigdec
  "`bigdec`, delegated to the host so the model characterizes rather than restates."
  [x]
  (bigdec x))

(m/=> reference-bigdec [:=> [:cat Coercible] decimal?])

(defn- observe
  "The value `thunk` yields, or a marker naming the class it threw."
  [thunk]
  (try (thunk) (catch Throwable t {:throws (.getName (class t))})))

(defn observed-bigdec
  "`reference-bigdec` over any argument at all: an off-domain argument, and an
  in-domain argument the host still refuses, are recorded as throw markers."
  [x]
  (observe #(reference-bigdec x)))

(m/=> observed-bigdec [:=> [:cat :any] Outcome])

(defn- throw-marker?
  "True when the outcome records a throw rather than a returned value."
  [o]
  (and (map? o) (contains? o :throws)))

(defn- canonical-round-trip?
  "Every decimal re-parses from its own rendering to an equal value, scale and
  exponent included. A throw marker is an accepted outcome."
  [o]
  (cond
    (throw-marker? o) true
    (decimal? o)      (= o (reference-bigdec (str o)))
    :else             false))

(def ^:private coercible-gen
  (gen/one-of [gen/large-integer
               gen/double
               gen/ratio
               (gen/fmap str gen/large-integer)
               gen/string-alphanumeric
               (gen/elements [0.1 -0.0 1/2 -1/2 1/3 1M 0M 1.5M 1N ##Inf ##NaN
                              Long/MAX_VALUE Double/MAX_VALUE
                              "0" "1" "+1" "-1" "0.5" "1e10" "1E-10" "+1e+10"
                              "" "abc" " 1" "Infinity"])
               gen/simple-type]))

(deftrifecta bigdec-behaviour
  clojure.core-model.bigdec/observed-bigdec
  {:golden-path "model/golden/bigdec.edn"
   :cases       {:zero                0
                 :one                 1
                 :minus-one           -1
                 :bigint-one          1N
                 :double-one          1.0
                 :double-zero         0.0
                 :negative-zero       -0.0
                 :double-tenth        0.1
                 :long-max            9223372036854775807
                 :text-zero           "0"
                 :text-one            "1"
                 :text-explicit-plus  "+1"
                 :text-minus-one      "-1"
                 :text-half           "0.5"
                 :text-minus-half     "-0.5"
                 :text-exp-lower      "1e10"
                 :text-exp-upper      "1E10"
                 :text-signed-exp     "+1e+10"
                 :text-negative-exp   "1E-10"
                 :text-minus-exp      "-1e-10"
                 :text-empty          ""
                 :text-word           "abc"
                 :text-leading-space  " 1"
                 :text-infinity       "Infinity"
                 :ratio-half          1/2
                 :ratio-minus-half    -1/2
                 :ratio-non-terminating 1/3
                 :ratio-improper      -3/2
                 :bigdec-identity     1.0M
                 :bigdec-zero         0M
                 :bigdec-fraction     1.5M
                 :big-integer         (biginteger 7)
                 :huge-bigint         123456789012345678901234567890N
                 :infinity            ##Inf
                 :negative-infinity   ##-Inf
                 :nan                 ##NaN
                 :boolean-true        true
                 :boolean-false       false
                 :char                \a
                 :keyword             :key
                 :nil                 nil}
   :xf          pr-str
   :gen         coercible-gen
   :pred        canonical-round-trip?
   :num-tests   500
   :mutations   [["identity"     (fn [x] x)]
                 ["always-zero"  (fn [_] 0M)]
                 ["never-throws" (fn [x] (try (bigdec x) (catch Throwable _ nil)))]
                 ["double-ctor"
                  (fn [x] (observe #(if (double? x)
                                      (java.math.BigDecimal. (double x))
                                      (bigdec x))))]
                 ["drops-scale"
                  (fn [x] (observe #(.setScale ^java.math.BigDecimal (bigdec x)
                                               0 java.math.RoundingMode/HALF_UP)))]
                 ["via-bigint"   (fn [x] (observe #(bigdec (bigint x))))]]})

(deftest bigdec-laws
  (testing "a double is converted through its own shortest rendering, not its exact binary value"
    (is (= (reference-bigdec 0.1) (reference-bigdec "0.1")))
    (is (not= (reference-bigdec 0.1) (java.math.BigDecimal. 0.1)))
    (is (= "0.1M" (pr-str (reference-bigdec 0.1)))))
  (testing "scale and exponent survive the coercion, even where = does not see them"
    (is (= "1.0M" (pr-str (reference-bigdec 1.0))))
    (is (= "1M" (pr-str (reference-bigdec 1))))
    (is (= (reference-bigdec 1.0) (reference-bigdec 1)))
    (is (= "1E+10M" (pr-str (reference-bigdec "1e10"))))
    (is (= "1E-10M" (pr-str (reference-bigdec "1E-10")))))
  (testing "negative zero is flattened to zero"
    (is (= "0.0M" (pr-str (reference-bigdec -0.0)))))
  (testing "a ratio is exact when it terminates and throws when it does not"
    (is (= 0.5M (reference-bigdec 1/2)))
    (is (= -0.5M (reference-bigdec -1/2)))
    (is (= -1.5M (reference-bigdec -3/2)))
    (is (= {:throws "java.lang.ArithmeticException"} (observed-bigdec 1/3))))
  (testing "the string grammar admits a sign and an exponent but no surrounding space"
    (is (= 1M (reference-bigdec "+1")))
    (is (= (reference-bigdec "1e10") (reference-bigdec "+1E+10")))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigdec " 1")))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigdec "")))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigdec "Infinity"))))
  (testing "the non-finite doubles and the non-numbers throw, with different classes"
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigdec ##Inf)))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigdec ##-Inf)))
    (is (= {:throws "java.lang.NumberFormatException"} (observed-bigdec ##NaN)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-bigdec true)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-bigdec false)))
    (is (= {:throws "java.lang.NullPointerException"} (observed-bigdec nil))))
  (testing "the result satisfies decimal?"
    (is (decimal? (reference-bigdec 1)))
    (is (decimal? (reference-bigdec 1N)))
    (is (decimal? (reference-bigdec "1")))))
