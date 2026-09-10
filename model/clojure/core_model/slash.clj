(ns clojure.core-model.slash
  "Behaviour model for `clojure.core//`. Emits `model/golden/slash.edn`, the
  baseline each dialect is graded against.

  The golden characterizes `/` AS A VALUE — the boxed arity every higher-order
  use reaches. A literal `(/ x y)` is compiler-inlined to a different overload
  with different zero behaviour; `slash-laws` records that split."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The numeric tower `/` accepts."
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

(defn reference-slash
  "`/` as a value, delegated to the host so the model characterizes rather than
  restates. Returns the host's answer, or a recorded throw. Unary reciprocates."
  [& args]
  (attempt / args))

(m/=> reference-slash [:=> [:cat [:* Numeric]] Outcome])

(defn- outcome?
  "Whether `o` is a number in the tower or a recorded throw — never nil, never an
  escaped exception."
  [o]
  (m/validate Outcome o))

(defn- nan?
  "Whether `x` is the double NaN, which is unequal to itself."
  [x]
  (and (double? x) (Double/isNaN x)))

(defn- exact-quotient-inverts?
  "`(* (/ a b) b)` = a exactly for integers with b non-zero — the rational tower
  loses nothing."
  [a b]
  (= a (* (reference-slash a b) b)))

(def ^:private gen-operand
  "Operands spanning the tower a dialect must get right: small and full-range
  longs, doubles (NaN and the infinities included), and ratios."
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap (fn [[a b]] (/ a (if (zero? b) 1 b)))
                         (gen/tuple gen/small-integer gen/small-integer))]))

(def ^:private gen-nonzero-integer
  "Integers excluding zero, the denominators on which exact division is total."
  (gen/such-that (complement zero?) gen/small-integer 100))

(deftrifecta slash-behaviour
  clojure.core-model.slash/reference-slash
  {:golden-path "model/golden/slash.edn"
   :apply?      true
   :cases       {:no-args               []
                 :unary-long            [2]
                 :unary-double          [2.0]
                 :unary-ratio           [1/2]
                 :unary-zero            [0]
                 :exact-long            [15 5]
                 :ratio-result          [1 2]
                 :negative-ratio        [-7 2]
                 :sign-normalized       [1 -3]
                 :sign-both-negative    [-1 -3]
                 :long-double           [15 2.0]
                 :double-double         [15.0 2.0]
                 :variadic              [1 2 3 4 5 6 7 8 9]
                 :bigint                [15N 5N]
                 :bigdec                [1.0M 2]
                 :bigdec-by-ratio       [2.0M 1/2]
                 :ratio-by-ratio        [1/2 1/3]
                 :long-min-by-neg-one   [Long/MIN_VALUE -1]
                 :one-over-inf          [1 ##Inf]
                 :neg-one-over-inf      [-1 ##Inf]
                 :nan-denominator       [1.0 ##NaN]
                 :nan-over-zero         [##NaN 0]
                 :inf-over-zero         [##Inf 0]
                 :divide-by-zero-long   [1 0]
                 :double-by-zero-double [1.0 0.0]
                 :double-by-zero-long   [1.0 0]
                 :zero-by-zero-double   [0.0 0.0]}
   :xf          pr-str
   :gen         (gen/vector gen-operand 1 4)
   :pred        outcome?
   :num-tests   500
   :mutations   [["always-zero"           (fn [& _] 0)]
                 ["swapped"               (fn [& args] (attempt / (reverse args)))]
                 ["double-division"       (fn [& args] (attempt / (map double args)))]
                 ["truncating"            (fn [& args] (attempt quot args))]
                 ["loses-throw"           (fn [& args] (try (apply / args) (catch Throwable _ nil)))]
                 ["zero-checked-before-nan"
                  (fn [& args]
                    (if (and (next args)
                             (some #(and (number? %) (not (nan? %)) (zero? %)) (rest args)))
                      [:throws "ArithmeticException" "Divide by zero"]
                      (attempt / args)))]]})

(deftest slash-laws
  (testing "exact division inverts under multiplication over the integers"
    (is (:pass? (tc/quick-check 300 (prop/for-all [a gen/small-integer
                                                   b gen-nonzero-integer]
                                      (exact-quotient-inverts? a b))))))
  (testing "integer division is exact and rational, never truncating or floating"
    (is (= 1/3 (reference-slash 1 3)))
    (is (= 1/2 (reference-slash 2)))
    (is (= 1/362880 (reference-slash 1 2 3 4 5 6 7 8 9)))
    (is (= 9223372036854775808N (reference-slash Long/MIN_VALUE -1))))
  (testing "the sign is normalized onto the numerator"
    (is (= (reference-slash 1 -3) (reference-slash -1 3)))
    (is (= 1/3 (reference-slash -1 -3))))
  (testing "boxed `/` checks NaN before it checks the zero denominator"
    (is (nan? (reference-slash ##NaN 0)))
    (is (nan? (reference-slash 1.0 ##NaN)))
    (is (= [:throws "ArithmeticException" "Divide by zero"] (reference-slash ##Inf 0))))
  ;; The compiler inlines a literal (/ x y) over primitive doubles to an overload
  ;; with no zero check; the var's boxed arity always checks. Both are `/`.
  (testing "inlined `/` yields infinities where the boxed arity throws"
    (is (= ##Inf (/ 1.0 0.0)))
    (is (= ##Inf (/ 1.0 0)))
    (is (= ##-Inf (/ -1.0 0.0)))
    (is (nan? (/ 0.0 0.0)))
    (is (= ##Inf (/ ##Inf 0)))
    (is (= [:throws "ArithmeticException" "Divide by zero"] (reference-slash 1.0 0.0)))
    (is (= [:throws "ArithmeticException" "Divide by zero"] (reference-slash 0.0 0.0)))
    (is (thrown? ArithmeticException (apply / [1.0 0.0]))))
  (testing "a non-numeric operand is rejected at run time, not coerced"
    (is (= [:throws "NullPointerException"] (subvec (reference-slash nil 1) 0 2)))
    (is (= [:throws "NullPointerException"] (subvec (reference-slash 1 nil) 0 2))))
  (testing "every case outcome satisfies the declared contract"
    (is (every? outcome? [(reference-slash 2) (reference-slash 1 3)
                          (reference-slash 1 0) (reference-slash 1.0M 2)]))))
