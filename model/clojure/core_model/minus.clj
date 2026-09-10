(ns clojure.core-model.minus
  "Behaviour model for `clojure.core/-`. Emits `model/golden/minus.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The numeric tower `-` accepts."
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

(defn reference-minus
  "`-` as a value, delegated to the host so the model characterizes rather than
  restates. Returns the host's answer, or a recorded throw. Unary negates."
  [& args]
  (attempt - args))

(m/=> reference-minus [:=> [:cat [:* Numeric]] Outcome])

(defn- outcome?
  "Whether `o` is a number in the tower or a recorded throw — never nil, never an
  escaped exception."
  [o]
  (m/validate Outcome o))

(defn- nan?
  "Whether `x` is the double NaN, which is unequal to itself."
  [x]
  (and (double? x) (Double/isNaN x)))

(defn- subtraction-is-negated-addition?
  "`(- a b)` = `(+ a (- b))`, with NaN excluded and the negation overflow at
  Long/MIN_VALUE excluded — there the two sides throw for different reasons."
  [a b]
  (if (= b Long/MIN_VALUE)
    true
    (let [l (reference-minus a b)
          r (attempt + [a (- b)])]
      (or (nan? l) (= l r)))))

(def ^:private gen-operand
  "Operands spanning the tower a dialect must get right: small and full-range
  longs, doubles (NaN and the infinities included), and ratios."
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap (fn [[a b]] (/ a (if (zero? b) 1 b)))
                         (gen/tuple gen/small-integer gen/small-integer))]))

(deftrifecta minus-behaviour
  clojure.core-model.minus/reference-minus
  {:golden-path "model/golden/minus.edn"
   :apply?      true
   :cases       {:no-args            []
                 :negate-long        [7]
                 :negate-double      [1.5]
                 :negate-zero        [0.0]
                 :negate-long-min    [Long/MIN_VALUE]
                 :negate-ratio       [1/3]
                 :long-long          [1 2]
                 :long-double        [1 2.5]
                 :double-double      [0.3 0.1]
                 :variadic           [10 1 2 3]
                 :underflow-min      [Long/MIN_VALUE 1]
                 :overflow-max       [Long/MAX_VALUE -1]
                 :min-minus-zero     [Long/MIN_VALUE 0]
                 :bigint-promotes    [Long/MIN_VALUE 1N]
                 :ratio-difference   [1/2 1/3]
                 :bigdec             [1.0M 2]
                 :nan                [1.0 ##NaN]
                 :inf-minus-inf      [##Inf ##Inf]
                 :zero-minus-zero    [0.0 0.0]
                 :neg-zero-minus-zero [-0.0 0.0]
                 :self-cancels       [7 7]}
   :xf          pr-str
   :gen         (gen/vector gen-operand 1 4)
   :pred        outcome?
   :num-tests   500
   :mutations   [["always-zero"        (fn [& _] 0)]
                 ["swapped"            (fn [& args] (attempt - (reverse args)))]
                 ["adds"               (fn [& args] (attempt + args))]
                 ["unchecked-subtract" (fn [x & more]
                                         (if (seq more)
                                           (reduce unchecked-subtract x more)
                                           (unchecked-negate x)))]
                 ["loses-throw"        (fn [& args] (try (apply - args) (catch Throwable _ nil)))]]})

(deftest minus-laws
  (testing "subtraction is addition of the negation, where negation is defined"
    (is (:pass? (tc/quick-check 300 (prop/for-all [a gen-operand b gen-operand]
                                      (subtraction-is-negated-addition? a b))))))
  (testing "unary negation of Long/MIN_VALUE throws, unlike abs, which returns it"
    (is (= [:throws "ArithmeticException" "long overflow"]
           (reference-minus Long/MIN_VALUE)))
    (is (= Long/MIN_VALUE (abs Long/MIN_VALUE)))
    (is (= Long/MIN_VALUE (unchecked-negate Long/MIN_VALUE))))
  (testing "negation carries the sign onto zero, subtraction does not"
    (is (= -0.0 (reference-minus 0.0)))
    (is (= 0.0 (reference-minus 0.0 0.0)))
    (is (= -0.0 (reference-minus -0.0 0.0))))
  (testing "long arithmetic throws on overflow rather than wrapping"
    (is (= [:throws "ArithmeticException" "long overflow"]
           (reference-minus Long/MIN_VALUE 1)))
    (is (= Long/MAX_VALUE (unchecked-subtract Long/MIN_VALUE 1))))
  (testing "a bigint operand promotes instead of overflowing"
    (is (= -9223372036854775809N (reference-minus Long/MIN_VALUE 1N))))
  (testing "every case outcome satisfies the declared contract"
    (is (every? outcome? [(reference-minus) (reference-minus 7)
                          (reference-minus Long/MIN_VALUE 1) (reference-minus 1/2 1/3)]))))
