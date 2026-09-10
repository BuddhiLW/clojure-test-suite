(ns clojure.core-model.quot
  "Behaviour model for `clojure.core/quot`. Emits `model/golden/quot.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The numeric tower `quot` accepts."
  [:or [:fn integer?] [:fn float?] [:fn ratio?] [:fn decimal?]])

(def Thrown
  "A host throw recorded as a value: class simple name and message."
  [:tuple [:= :throws] :string [:maybe :string]])

(def Outcome
  "What a reference call yields: a number, or a recorded throw."
  [:or Numeric Thrown])

(defn- attempt
  "`(apply f args)`, or `[:throws class message]` when the host throws. Total, so
  a divergent edge is recorded rather than crashing the emit."
  [f args]
  (try (apply f args)
       (catch Throwable t [:throws (.getSimpleName (class t)) (.getMessage t)])))

(defn reference-quot
  "`quot`, delegated to the host so the model characterizes rather than restates.
  Returns the host's answer, or a recorded throw."
  [num div]
  (attempt quot [num div]))

(m/=> reference-quot [:=> [:cat Numeric Numeric] Outcome])

(defn- outcome?
  "Whether `o` is a number in the tower or a recorded throw — never nil, never an
  escaped exception."
  [o]
  (m/validate Outcome o))

(defn- truncates-toward-zero?
  "`(quot x y)` rounds the exact quotient toward zero, so it never exceeds it in
  magnitude and carries the sign of the exact quotient."
  [x y]
  (let [q (reference-quot x y)]
    (and (<= (abs q) (abs (/ x y)))
         (or (zero? q) (= (pos? q) (pos? (/ x y)))))))

(defn- division-identity?
  "`x` = `(+ (* (quot x y) y) (rem x y))` — the defining relation between the two."
  [x y]
  (= x (+ (* (reference-quot x y) y) (rem x y))))

(def ^:private gen-operand
  "Operands spanning the tower a dialect must get right: small and full-range
  longs, doubles (NaN and the infinities included), and ratios."
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap (fn [[a b]] (/ a (if (zero? b) 1 b)))
                         (gen/tuple gen/small-integer gen/small-integer))]))

(def ^:private gen-nonzero-integer
  "Integers excluding zero, the divisors on which `quot` is total."
  (gen/such-that (complement zero?) gen/small-integer 100))

(deftrifecta quot-behaviour
  clojure.core-model.quot/reference-quot
  {:golden-path "model/golden/quot.edn"
   :apply?      true
   :cases       {:pos-pos             [10 3]
                 :neg-pos             [-10 3]
                 :pos-neg             [10 -3]
                 :neg-neg             [-10 -3]
                 :exact               [10 5]
                 :magnitude-below-one [5 10]
                 :neg-magnitude-below-one [-5 10]
                 :zero-numerator      [0 3]
                 :zero-denominator    [10 0]
                 :zero-denominator-double [10 0.0]
                 :double-numerator    [10.0 3]
                 :double-denominator  [10 3.0]
                 :double-double       [10.0 3.0]
                 :double-fraction     [7.5 2]
                 :double-negative     [-10.0 3]
                 :bigint              [10N 3]
                 :bigdec              [10.0M 3.0M]
                 :bigdec-downcasts    [10.0M 3.0]
                 :ratio-below-one     [1/2 3/4]
                 :ratio-promotes      [3 1/2]
                 :ratio-mixed         [37/2 15]
                 :by-infinity         [1 ##Inf]
                 :by-negative-infinity [1 ##-Inf]
                 :infinite-numerator  [##Inf 1]
                 :nan-numerator       [##NaN 1]
                 :nan-denominator     [1 ##NaN]
                 :long-min-by-neg-one [Long/MIN_VALUE -1]}
   :xf          pr-str
   :gen         (gen/tuple gen-operand gen-operand)
   :pred        outcome?
   :num-tests   500
   :mutations   [["always-zero"    (fn [_ _] 0)]
                 ["floor-division" (fn [x y]
                                     (attempt (fn [a b] (long (Math/floor (/ (double a) (double b)))))
                                              [x y]))]
                 ["uses-rem"       (fn [x y] (attempt rem [x y]))]
                 ["swapped"        (fn [x y] (attempt quot [y x]))]
                 ["loses-throw"    (fn [x y] (try (quot x y) (catch Throwable _ nil)))]
                 ["guards-overflow"
                  (fn [x y]
                    (if (and (= x Long/MIN_VALUE) (= y -1))
                      [:throws "ArithmeticException" "long overflow"]
                      (attempt quot [x y])))]]})

(deftest quot-laws
  (testing "quot and rem satisfy the division identity over the integers"
    (is (:pass? (tc/quick-check 300 (prop/for-all [x gen/small-integer
                                                   y gen-nonzero-integer]
                                      (division-identity? x y))))))
  (testing "quot truncates toward zero, never away from it"
    (is (:pass? (tc/quick-check 300 (prop/for-all [x gen/small-integer
                                                   y gen-nonzero-integer]
                                      (truncates-toward-zero? x y)))))
    (is (= -3 (reference-quot -10 3)))
    (is (= -4 (long (Math/floor (/ -10.0 3.0)))))
    (is (= 2 (mod -10 3))))
  (testing "quot overflows silently at Long/MIN_VALUE, unlike + - and *"
    (is (= Long/MIN_VALUE (reference-quot Long/MIN_VALUE -1)))
    (is (neg? (reference-quot Long/MIN_VALUE -1))))
  (testing "a zero divisor throws even when it is a double"
    (is (= [:throws "ArithmeticException" "Divide by zero"] (reference-quot 10 0)))
    (is (= [:throws "ArithmeticException" "Divide by zero"] (reference-quot 10 0.0))))
  (testing "an infinite or NaN operand fails BigDecimal conversion, not division"
    (is (= [:throws "NumberFormatException" "Infinite or NaN"] (reference-quot ##Inf 1)))
    (is (= [:throws "NumberFormatException" "Infinite or NaN"] (reference-quot ##NaN 1)))
    (is (= 0.0 (reference-quot 1 ##Inf))))
  (testing "quot takes exactly two arguments"
    (is (= [:throws "ArityException"] (subvec (attempt quot [10]) 0 2)))
    (is (= [:throws "ArityException"] (subvec (attempt quot [10 3 2]) 0 2))))
  (testing "every case outcome satisfies the declared contract"
    (is (every? outcome? [(reference-quot 10 3) (reference-quot 10 0)
                          (reference-quot 1/2 3/4) (reference-quot ##Inf 1)]))))
