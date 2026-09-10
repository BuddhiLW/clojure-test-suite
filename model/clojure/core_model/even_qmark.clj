(ns clojure.core-model.even-qmark
  "Behaviour model for `clojure.core/even?`. Emits `model/golden/even_qmark.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Integral
  "The domain `even?` accepts: an integer of any width."
  [:fn integer?])

(def Outcome
  "One call recorded totally: the boolean the host answered, or the simple name
  of the class it threw."
  [:or :boolean [:map [:throws :string]]])

(def Probe
  "An argument paired with the outcome of calling `even?` on it."
  [:tuple :any Outcome])

(defn reference-even?
  "`even?`, delegated to the host so the model characterizes rather than
  restates. Throws for an argument outside `Integral`."
  [x]
  (even? x))

(m/=> reference-even? [:=> [:cat Integral] :boolean])

(defn- outcome
  "`(f x)`, or `{:throws \"<simple class name>\"}` when the call throws."
  [f x]
  (try (f x)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn probe-even?
  "`even?` as a total function: `[x outcome]`. A refusal is recorded rather than
  propagated, so the baseline carries the whole domain."
  [x]
  [x (outcome reference-even? x)])

(m/=> probe-even? [:=> [:cat :any] Probe])

(defn- agrees-with-remainder?
  "On the integral domain `even?` is `(zero? (mod x 2))`; off it every argument
  is refused with an argument error rather than answered false."
  [[x out]]
  (if (integer? x)
    (= out (zero? (mod x 2)))
    (= out {:throws "IllegalArgumentException"})))

(defn- refuses?
  "Whether the host refuses `x` rather than answering."
  [x]
  (map? (outcome reference-even? x)))

(deftrifecta even-qmark-behaviour
  clojure.core-model.even-qmark/probe-even?
  {:golden-path "model/golden/even_qmark.edn"
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
                 :integral-double      2.0
                 :fractional-double    1.5
                 :negative-zero        -0.0
                 :infinity             ##Inf
                 :negative-infinity    ##-Inf
                 :nan                  ##NaN
                 :ratio                1/2
                 :bigdec               0.2M
                 :nil                  nil
                 :string               "2"
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
   :pred        agrees-with-remainder?
   :num-tests   500
   ;; Each mutant must be REJECTED by the facets above; a survivor means the
   ;; model asserts nothing.
   :mutations   [["always-true"       (fn [x] [x true])]
                 ["always-false"      (fn [x] [x false])]
                 ["odd-instead"       (fn [x] [x (outcome odd? x)])]
                 ["never-refuses"     (fn [x] [x (outcome #(zero? (mod % 2)) x)])]
                 ["coerces-doubles"   (fn [x] [x (outcome #(even? (long %)) x)])]
                 ["echoes-wrong-input" (fn [_] [0 true])]]})

(deftest even-qmark-laws
  (testing "parity decides the integral domain, at any width"
    (is (every? #(agrees-with-remainder? (probe-even? %))
                [0 12 17 -118 -119 122N 123N -120N -121N
                 Long/MIN_VALUE Long/MAX_VALUE])))
  (testing "a double is refused even when it is mathematically even"
    (is (refuses? 2.0))
    (is (refuses? -0.0))
    (is (refuses? 1.5)))
  (testing "every non-integral argument is refused, not answered false"
    (is (every? refuses? [##Inf ##-Inf ##NaN 1/2 0.2M nil "2" true])))
  (testing "even? and odd? partition the integral domain"
    (is (every? #(not= (even? %) (odd? %))
                [0 12 17 -118 -119 122N 123N Long/MIN_VALUE Long/MAX_VALUE]))))
