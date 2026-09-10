(ns clojure.core-model.zero-qmark
  "Behaviour model for `clojure.core/zero?`. Emits `model/golden/zero_qmark.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `zero?` accepts: any number in the tower."
  [:fn number?])

(def Outcome
  "One call recorded totally: the boolean the host answered, or the simple name
  of the class it threw."
  [:or :boolean [:map [:throws :string]]])

(def Probe
  "An argument paired with the outcome of calling `zero?` on it."
  [:tuple :any Outcome])

(defn reference-zero?
  "`zero?`, delegated to the host so the model characterizes rather than
  restates. Throws for an argument outside `Numeric`."
  [x]
  (zero? x))

(m/=> reference-zero? [:=> [:cat Numeric] :boolean])

(defn- outcome
  "`(f x)`, or `{:throws \"<simple class name>\"}` when the call throws."
  [f x]
  (try (f x)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn probe-zero?
  "`zero?` as a total function: `[x outcome]`. A refusal is recorded rather than
  propagated, so the baseline carries the whole domain."
  [x]
  [x (outcome reference-zero? x)])

(m/=> probe-zero? [:=> [:cat :any] Probe])

(defn- agrees-with-numeric-equality?
  "`zero?` answers exactly what `(== x 0)` answers, refusal included: -0.0 and
  0.0M are zero, ##NaN is not, and a non-number is refused by both."
  [[x out]]
  (= out (outcome #(== % 0) x)))

(defn- refuses?
  "Whether the host refuses `x` rather than answering."
  [x]
  (map? (outcome reference-zero? x)))

(deftrifecta zero-qmark-behaviour
  clojure.core-model.zero-qmark/probe-zero?
  {:golden-path "model/golden/zero_qmark.edn"
   :cases       {:zero              0
                 :zero-double       0.0
                 :negative-zero     -0.0
                 :zero-bigint       0N
                 :zero-bigdec       0.0M
                 :zero-ratio        (/ 0 2)
                 :one               1
                 :minus-one         -1
                 :one-double        1.0
                 :minus-one-double  -1.0
                 :tiny              0.0000001
                 :min-double        Double/MIN_VALUE
                 :max-double        Double/MAX_VALUE
                 :min-int           Long/MIN_VALUE
                 :max-int           Long/MAX_VALUE
                 :infinity          ##Inf
                 :negative-infinity ##-Inf
                 :nan               ##NaN
                 :one-bigint        1N
                 :one-bigdec        1.0M
                 :ratio             1/2
                 :negative-ratio    -1/2
                 :nil               nil
                 :false             false
                 :true              true
                 :string            "0"}
   :xf          pr-str
   :gen         (gen/one-of [gen/small-integer
                             gen/large-integer
                             gen/size-bounded-bigint
                             gen/double
                             gen/ratio
                             (gen/fmap bigdec gen/large-integer)
                             (gen/return nil)
                             gen/string
                             gen/keyword
                             gen/boolean])
   :pred        agrees-with-numeric-equality?
   :num-tests   500
   ;; Each mutant must be REJECTED by the facets above; a survivor means the
   ;; model asserts nothing.
   :mutations   [["always-true"       (fn [x] [x true])]
                 ["always-false"      (fn [x] [x false])]
                 ["identity-equality" (fn [x] [x (outcome #(= 0 %) x)])]
                 ["sign-bit-strict"   (fn [x] [x (outcome #(if (double? %)
                                                             (zero? (Double/doubleToRawLongBits %))
                                                             (== % 0))
                                                          x)])]
                 ["nan-is-zero"       (fn [x] [x (outcome #(not (or (pos? %) (neg? %))) x)])]
                 ["never-refuses"     (fn [x] [x (try (zero? x) (catch Throwable _ false))])]]})

(deftest zero-qmark-laws
  (testing "every zero in the tower is zero, negative zero included"
    (is (every? #(true? (second (probe-zero? %)))
                [0 0.0 -0.0 0N 0.0M (/ 0 2)])))
  (testing "##NaN is not zero, and is neither positive nor negative either"
    (is (false? (second (probe-zero? ##NaN))))
    (is (not (pos? ##NaN)))
    (is (not (neg? ##NaN))))
  (testing "the trichotomy holds for every number except ##NaN"
    (is (every? #(= 1 (count (filter true? [(zero? %) (pos? %) (neg? %)])))
                [0 0.0 -0.0 1 -1 1.0 -1.0 1/2 -1/2 1N -1N 1.0M -1.0M
                 Double/MIN_VALUE Double/MAX_VALUE Long/MIN_VALUE Long/MAX_VALUE
                 ##Inf ##-Inf])))
  (testing "a non-number is refused, not answered false"
    (is (every? refuses? [nil false true "0" :zero]))))
