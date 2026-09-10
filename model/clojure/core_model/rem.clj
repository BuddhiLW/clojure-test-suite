(ns clojure.core-model.rem
  "Behaviour model for `clojure.core/rem`. Emits `model/golden/rem.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The numeric tower `rem` accepts."
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

(defn reference-rem
  "`rem`, delegated to the host so the model characterizes rather than restates.
  Returns the host's answer, or a recorded throw."
  [num div]
  (attempt rem [num div]))

(m/=> reference-rem [:=> [:cat Numeric Numeric] Outcome])

(defn- outcome?
  "Whether `o` is a number in the tower or a recorded throw — never nil, never an
  escaped exception."
  [o]
  (m/validate Outcome o))

(defn- follows-dividend-sign?
  "`(rem x y)` is zero or carries the sign of the dividend, whatever the divisor's
  sign — the convention that separates it from `mod`."
  [x y]
  (let [r (reference-rem x y)]
    (or (zero? r) (= (neg? r) (neg? x)))))

(defn- smaller-than-divisor?
  "`(abs (rem x y))` is strictly below `(abs y)`."
  [x y]
  (< (abs (reference-rem x y)) (abs y)))

(defn- division-identity?
  "`x` = `(+ (* (quot x y) y) (rem x y))` — the defining relation between the two."
  [x y]
  (= x (+ (* (quot x y) y) (reference-rem x y))))

(def ^:private gen-operand
  "Operands spanning the tower a dialect must get right: small and full-range
  longs, doubles (NaN and the infinities included), and ratios."
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap (fn [[a b]] (/ a (if (zero? b) 1 b)))
                         (gen/tuple gen/small-integer gen/small-integer))]))

(def ^:private gen-nonzero-integer
  "Integers excluding zero, the divisors on which `rem` is total."
  (gen/such-that (complement zero?) gen/small-integer 100))

(deftrifecta rem-behaviour
  clojure.core-model.rem/reference-rem
  {:golden-path "model/golden/rem.edn"
   :apply?      true
   :cases       {:pos-pos                 [10 3]
                 :neg-pos                 [-10 3]
                 :pos-neg                 [10 -3]
                 :neg-neg                 [-10 -3]
                 :exact                   [10 5]
                 :magnitude-below-divisor [5 10]
                 :zero-numerator          [0 3]
                 :negative-zero-numerator [-0.0 3]
                 :zero-denominator        [10 0]
                 :zero-denominator-double [10 0.0]
                 :double-numerator        [10.0 3]
                 :double-denominator      [10 3.0]
                 :double-double           [10.0 3.0]
                 :double-fraction         [7.5 2]
                 :double-negative         [-7.5 2]
                 :bigint                  [10N 3]
                 :bigdec                  [10.0M 3.0M]
                 :bigdec-downcasts        [10.0M 3.0]
                 :ratio-below-one         [1/2 3/4]
                 :ratio-promotes          [3 1/2]
                 :ratio-mixed             [37/2 15]
                 :by-infinity             [1 ##Inf]
                 :infinite-numerator      [##Inf 1]
                 :nan-numerator           [##NaN 1]
                 :nan-denominator         [1 ##NaN]
                 :long-min-by-neg-one     [Long/MIN_VALUE -1]}
   :xf          pr-str
   :gen         (gen/tuple gen-operand gen-operand)
   :pred        outcome?
   :num-tests   500
   :mutations   [["always-zero"    (fn [_ _] 0)]
                 ["mod"            (fn [x y] (attempt mod [x y]))]
                 ["abs-remainder"  (fn [x y] (attempt (fn [a b] (abs (rem a b))) [x y]))]
                 ["uses-quot"      (fn [x y] (attempt quot [x y]))]
                 ["swapped"        (fn [x y] (attempt rem [y x]))]
                 ["loses-throw"    (fn [x y] (try (rem x y) (catch Throwable _ nil)))]]})

(deftest rem-laws
  (testing "quot and rem satisfy the division identity over the integers"
    (is (:pass? (tc/quick-check 300 (prop/for-all [x gen/small-integer
                                                   y gen-nonzero-integer]
                                      (division-identity? x y))))))
  (testing "rem carries the sign of the dividend and stays below the divisor"
    (is (:pass? (tc/quick-check 300 (prop/for-all [x gen/small-integer
                                                   y gen-nonzero-integer]
                                      (and (follows-dividend-sign? x y)
                                           (smaller-than-divisor? x y))))))
    (is (= -1 (reference-rem -10 3)))
    (is (= 1 (reference-rem 10 -3))))
  (testing "mod takes the sign of the divisor instead, so the two differ"
    (is (= 2 (mod -10 3)))
    (is (= -2 (mod 10 -3)))
    (is (not= (mod -10 3) (reference-rem -10 3))))
  (testing "the sign of a negative zero dividend survives"
    (is (= -0.0 (reference-rem -0.0 3)))
    (is (= 0.0 (reference-rem 0.0 3))))
  (testing "rem does not overflow where quot silently does"
    (is (= 0 (reference-rem Long/MIN_VALUE -1))))
  (testing "a zero divisor throws even when it is a double"
    (is (= [:throws "ArithmeticException" "Divide by zero"] (reference-rem 10 0)))
    (is (= [:throws "ArithmeticException" "Divide by zero"] (reference-rem 10 0.0))))
  (testing "an infinite or NaN operand fails BigDecimal conversion, not division"
    (is (= [:throws "NumberFormatException" "Infinite or NaN"] (reference-rem ##Inf 1)))
    (is (= [:throws "NumberFormatException" "Infinite or NaN"] (reference-rem ##NaN 1)))
    (is (Double/isNaN (reference-rem 1 ##Inf))))
  (testing "rem takes exactly two arguments"
    (is (= [:throws "ArityException"] (subvec (attempt rem [10]) 0 2)))
    (is (= [:throws "ArityException"] (subvec (attempt rem [10 3 2]) 0 2))))
  (testing "every case outcome satisfies the declared contract"
    (is (every? outcome? [(reference-rem 10 3) (reference-rem 10 0)
                          (reference-rem 1/2 3/4) (reference-rem ##Inf 1)]))))
