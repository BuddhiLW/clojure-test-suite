(ns clojure.core-model.odd-qmark
  "Behaviour model for `clojure.core/odd?`. Emits `model/golden/odd_qmark.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Integral
  "The domain `odd?` accepts: an integer of any width."
  [:fn integer?])

(def Outcome
  "One call recorded totally: the boolean the host answered, or the simple name
  of the class it threw."
  [:or :boolean [:map [:throws :string]]])

(def Probe
  "An argument paired with the outcome of calling `odd?` on it."
  [:tuple :any Outcome])

(defn reference-odd?
  "`odd?`, delegated to the host so the model characterizes rather than
  restates. Throws for an argument outside `Integral`."
  [x]
  (odd? x))

(m/=> reference-odd? [:=> [:cat Integral] :boolean])

(defn- outcome
  "`(f x)`, or `{:throws \"<simple class name>\"}` when the call throws."
  [f x]
  (try (f x)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn probe-odd?
  "`odd?` as a total function: `[x outcome]`. A refusal is recorded rather than
  propagated, so the baseline carries the whole domain."
  [x]
  [x (outcome reference-odd? x)])

(m/=> probe-odd? [:=> [:cat :any] Probe])

(defn- disagrees-with-remainder?
  "On the integral domain `odd?` is `(not (zero? (mod x 2)))`; off it every
  argument is refused with an argument error rather than answered false."
  [[x out]]
  (if (integer? x)
    (= out (not (zero? (mod x 2))))
    (= out {:throws "IllegalArgumentException"})))

(defn- refuses?
  "Whether the host refuses `x` rather than answering."
  [x]
  (map? (outcome reference-odd? x)))

(deftrifecta odd-qmark-behaviour
  clojure.core-model.odd-qmark/probe-odd?
  {:golden-path "model/golden/odd_qmark.edn"
   :cases       {:zero                 0
                 :positive-even        12
                 :positive-odd         17
                 :negative-even        -118
                 :negative-odd         -119
                 :bigint-even          122N
                 :bigint-odd           123N
                 :bigint-negative-even -120N
                 :bigint-negative-odd  -121N
                 :long-min             Long/MIN_VALUE
                 :long-max             Long/MAX_VALUE
                 :integral-double      3.0
                 :fractional-double    1.5
                 :negative-zero        -0.0
                 :infinity             ##Inf
                 :negative-infinity    ##-Inf
                 :nan                  ##NaN
                 :ratio                1/2
                 :bigdec               0.2M
                 :nil                  nil
                 :string               "3"
                 :boolean              true}
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
   :pred        disagrees-with-remainder?
   :num-tests   500
   ;; Each mutant must be REJECTED by the facets above; a survivor means the
   ;; model asserts nothing.
   :mutations   [["always-true"        (fn [x] [x true])]
                 ["always-false"       (fn [x] [x false])]
                 ["even-instead"       (fn [x] [x (outcome even? x)])]
                 ["never-refuses"      (fn [x] [x (outcome #(not (zero? (mod % 2))) x)])]
                 ["coerces-doubles"    (fn [x] [x (outcome #(odd? (long %)) x)])]
                 ["echoes-wrong-input" (fn [_] [1 true])]]})

(deftest odd-qmark-laws
  (testing "parity decides the integral domain, at any width"
    (is (every? #(disagrees-with-remainder? (probe-odd? %))
                [0 12 17 -118 -119 122N 123N -120N -121N
                 Long/MIN_VALUE Long/MAX_VALUE])))
  (testing "a double is refused even when it is mathematically odd"
    (is (refuses? 3.0))
    (is (refuses? -0.0))
    (is (refuses? 1.5)))
  (testing "every non-integral argument is refused, not answered false"
    (is (every? refuses? [##Inf ##-Inf ##NaN 1/2 0.2M nil "3" true])))
  (testing "odd? is the complement of even? on the integral domain"
    (is (every? #(not= (odd? %) (even? %))
                [0 12 17 -118 -119 122N 123N Long/MIN_VALUE Long/MAX_VALUE]))))
