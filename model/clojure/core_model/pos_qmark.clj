(ns clojure.core-model.pos-qmark
  "Behaviour model for `clojure.core/pos?`. Emits `model/golden/pos_qmark.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `pos?` accepts: any number in the tower."
  [:fn number?])

(def Outcome
  "One call recorded totally: the boolean the host answered, or the simple name
  of the class it threw."
  [:or :boolean [:map [:throws :string]]])

(def Probe
  "An argument paired with the outcome of calling `pos?` on it."
  [:tuple :any Outcome])

(defn reference-pos?
  "`pos?`, delegated to the host so the model characterizes rather than
  restates. Throws for an argument outside `Numeric`."
  [x]
  (pos? x))

(m/=> reference-pos? [:=> [:cat Numeric] :boolean])

(defn- outcome
  "`(f x)`, or `{:throws \"<simple class name>\"}` when the call throws."
  [f x]
  (try (f x)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn probe-pos?
  "`pos?` as a total function: `[x outcome]`. A refusal is recorded rather than
  propagated, so the baseline carries the whole domain."
  [x]
  [x (outcome reference-pos? x)])

(m/=> probe-pos? [:=> [:cat :any] Probe])

(defn- agrees-with-ordering?
  "`pos?` answers exactly what `(> x 0)` answers, refusal included: ##NaN is
  unordered so it is not positive, and neither zero is."
  [[x out]]
  (= out (outcome #(> % 0) x)))

(defn- refuses?
  "Whether the host refuses `x` rather than answering."
  [x]
  (map? (outcome reference-pos? x)))

(deftrifecta pos-qmark-behaviour
  clojure.core-model.pos-qmark/probe-pos?
  {:golden-path "model/golden/pos_qmark.edn"
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
                 :min-double        Double/MIN_VALUE
                 :max-double        Double/MAX_VALUE
                 :min-int           Long/MIN_VALUE
                 :max-int           Long/MAX_VALUE
                 :infinity          ##Inf
                 :negative-infinity ##-Inf
                 :nan               ##NaN
                 :one-bigint        1N
                 :minus-one-bigint  -1N
                 :one-bigdec        1.0M
                 :minus-one-bigdec  -1.0M
                 :ratio             1/2
                 :negative-ratio    -1/2
                 :nil               nil
                 :false             false
                 :true              true
                 :string            "1"}
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
   :pred        agrees-with-ordering?
   :num-tests   500
   ;; Each mutant must be REJECTED by the facets above; a survivor means the
   ;; model asserts nothing.
   :mutations   [["always-true"      (fn [x] [x true])]
                 ["always-false"     (fn [x] [x false])]
                 ["includes-zero"    (fn [x] [x (outcome #(>= % 0) x)])]
                 ["nan-is-positive"  (fn [x] [x (outcome #(not (or (neg? %) (zero? %))) x)])]
                 ["sign-bit"         (fn [x] [x (outcome #(if (double? %)
                                                            (not (neg? (Double/doubleToRawLongBits %)))
                                                            (> % 0))
                                                         x)])]
                 ["never-refuses"    (fn [x] [x (try (pos? x) (catch Throwable _ false))])]]})

(deftest pos-qmark-laws
  (testing "no zero in the tower is positive, negative zero included"
    (is (every? #(false? (second (probe-pos? %)))
                [0 0.0 -0.0 0N 0.0M (/ 0 2)])))
  (testing "##NaN is unordered, so it is neither positive nor negative"
    (is (false? (second (probe-pos? ##NaN))))
    (is (not (neg? ##NaN))))
  (testing "the smallest positive double is still positive"
    (is (true? (second (probe-pos? Double/MIN_VALUE))))
    (is (false? (second (probe-pos? (- Double/MIN_VALUE))))))
  (testing "pos? is the strict half of the ordering, at every width"
    (is (every? #(agrees-with-ordering? (probe-pos? %))
                [0 1 -1 1.0 -1.0 1N -1N 1.0M -1.0M 1/2 -1/2
                 Long/MIN_VALUE Long/MAX_VALUE ##Inf ##-Inf ##NaN])))
  (testing "a non-number is refused, not answered false"
    (is (every? refuses? [nil false true "1" :one]))))
