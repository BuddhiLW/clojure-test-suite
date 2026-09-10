(ns clojure.core-model.nan-qmark
  "Behaviour model for `clojure.core/NaN?`. Emits `model/golden/nan_qmark.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `NaN?` accepts: any number in the tower."
  [:fn number?])

(def Outcome
  "One call recorded totally: the boolean the host answered, or the simple name
  of the class it threw."
  [:or :boolean [:map [:throws :string]]])

(def Probe
  "An argument paired with the outcome of calling `NaN?` on it."
  [:tuple :any Outcome])

(defn reference-nan?
  "`NaN?`, delegated to the host so the model characterizes rather than
  restates. Throws for an argument outside `Numeric`."
  [x]
  (NaN? x))

(m/=> reference-nan? [:=> [:cat Numeric] :boolean])

(defn- outcome
  "`(f x)`, or `{:throws \"<simple class name>\"}` when the call throws."
  [f x]
  (try (f x)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn probe-nan?
  "`NaN?` as a total function: `[x outcome]`. A refusal is recorded rather than
  propagated, so the baseline carries the whole domain."
  [x]
  [x (outcome reference-nan? x)])

(m/=> probe-nan? [:=> [:cat :any] Probe])

(defn- agrees-with-self-inequality?
  "`NaN?` answers exactly what `(not (== x x))` answers, refusal included: a NaN
  is the only number that is not numerically equal to itself."
  [[x out]]
  (= out (outcome #(not (== % %)) x)))

(defn- refuses?
  "Whether the host refuses `x` rather than answering."
  [x]
  (map? (outcome reference-nan? x)))

(deftrifecta nan-qmark-behaviour
  clojure.core-model.nan-qmark/probe-nan?
  {:golden-path "model/golden/nan_qmark.edn"
   :cases       {:nan               ##NaN
                 :infinity          ##Inf
                 :negative-infinity ##-Inf
                 :zero              0
                 :zero-double       0.0
                 :negative-zero     -0.0
                 :one-double        1.0
                 :minus-one-double  -1.0
                 :min-double        Double/MIN_VALUE
                 :max-double        Double/MAX_VALUE
                 :min-int           Long/MIN_VALUE
                 :max-int           Long/MAX_VALUE
                 :one-bigint        1N
                 :one-bigdec        1.0M
                 :ratio             1/2
                 :nil               nil
                 :false             false
                 :true              true
                 :nan-string        "##NaN"
                 :keyword           :nan}
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
   :pred        agrees-with-self-inequality?
   :num-tests   500
   ;; Each mutant must be REJECTED by the facets above; a survivor means the
   ;; model asserts nothing.
   :mutations   [["always-true"           (fn [x] [x true])]
                 ["always-false"          (fn [x] [x false])]
                 ["infinities-are-nan"    (fn [x] [x (outcome #(and (double? %)
                                                                    (or (Double/isNaN %)
                                                                        (Double/isInfinite %)))
                                                              x)])]
                 ["non-numbers-are-nan"   (fn [x] [x (outcome #(not (number? %)) x)])]
                 ["never-refuses"         (fn [x] [x (try (NaN? x) (catch Throwable _ false))])]
                 ["echoes-wrong-input"    (fn [_] [##NaN true])]]})

(deftest nan-qmark-laws
  (testing "##NaN is the only number that is not numerically equal to itself"
    (is (true? (second (probe-nan? ##NaN))))
    (is (not (== ##NaN ##NaN)))
    (is (not (= ##NaN ##NaN)))
    (is (double? ##NaN)))
  (testing "the infinities are ordinary doubles, not NaN"
    (is (false? (second (probe-nan? ##Inf))))
    (is (false? (second (probe-nan? ##-Inf)))))
  (testing "##NaN is neither positive nor negative nor zero"
    (is (not (pos? ##NaN)))
    (is (not (neg? ##NaN)))
    (is (not (zero? ##NaN))))
  (testing "self-inequality decides the whole tower"
    (is (every? #(agrees-with-self-inequality? (probe-nan? %))
                [0 0.0 -0.0 1.0 -1.0 1N 1.0M 1/2 ##Inf ##-Inf ##NaN
                 Long/MIN_VALUE Long/MAX_VALUE Double/MIN_VALUE Double/MAX_VALUE])))
  (testing "a non-number is refused, not answered false"
    (is (every? refuses? [nil false true "##NaN" :nan]))))
