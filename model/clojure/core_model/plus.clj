(ns clojure.core-model.plus
  "Behaviour model for `clojure.core/+`. Emits `model/golden/plus.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The numeric tower `+` accepts."
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

(defn reference-plus
  "`+` as a value, delegated to the host so the model characterizes rather than
  restates. Returns the host's answer, or a recorded throw."
  [& args]
  (attempt + args))

(m/=> reference-plus [:=> [:cat [:* Numeric]] Outcome])

(defn- outcome?
  "Whether `o` is a number in the tower or a recorded throw — never nil, never an
  escaped exception."
  [o]
  (m/validate Outcome o))

(defn- nan?
  "Whether `x` is the double NaN, which is unequal to itself."
  [x]
  (and (double? x) (Double/isNaN x)))

(defn- commutative?
  "`(+ a b)` = `(+ b a)`, with NaN excluded as unordered."
  [a b]
  (let [l (reference-plus a b)]
    (or (nan? l) (= l (reference-plus b a)))))

(def ^:private gen-operand
  "Operands spanning the tower a dialect must get right: small and full-range
  longs, doubles (NaN and the infinities included), and ratios."
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap (fn [[a b]] (/ a (if (zero? b) 1 b)))
                         (gen/tuple gen/small-integer gen/small-integer))]))

(deftrifecta plus-behaviour
  clojure.core-model.plus/reference-plus
  {:golden-path "model/golden/plus.edn"
   :apply?      true
   :cases       {:no-args           []
                 :unary-long        [7]
                 :unary-double      [1.5]
                 :long-long         [1 2]
                 :long-double       [1 2.5]
                 :double-double     [0.1 0.2]
                 :variadic          [1 2 3 4]
                 :overflow-max      [Long/MAX_VALUE 1]
                 :overflow-min      [Long/MIN_VALUE -1]
                 :max-plus-zero     [Long/MAX_VALUE 0]
                 :max-plus-double   [Long/MAX_VALUE 1.0]
                 :bigint-promotes   [Long/MAX_VALUE 1N]
                 :ratio-collapses   [1/2 1/2]
                 :ratio-stays-ratio [1/3 1/6]
                 :bigdec            [1.0M 2]
                 :nan               [1.0 ##NaN]
                 :inf-minus-inf     [##Inf ##-Inf]
                 :inf-plus-inf      [##Inf ##Inf]
                 :negative-zeros    [-0.0 -0.0]
                 :zero-absorbs-sign [-0.0 0.0]}
   :xf          pr-str
   :gen         (gen/vector gen-operand 0 4)
   :pred        outcome?
   :num-tests   500
   :mutations   [["always-zero"   (fn [& _] 0)]
                 ["first-arg"     (fn [& args] (first args))]
                 ["multiplies"    (fn [& args] (attempt * args))]
                 ["unchecked-add" (fn [& args] (reduce unchecked-add 0 args))]
                 ["loses-throw"   (fn [& args] (try (apply + args) (catch Throwable _ nil)))]]})

(deftest plus-laws
  (testing "commutativity holds across the whole generated domain"
    (is (:pass? (tc/quick-check 300 (prop/for-all [a gen-operand b gen-operand]
                                      (commutative? a b))))))
  (testing "0 is the identity, except that it coerces -0.0 to 0.0"
    (is (= 7 (reference-plus 7 0)))
    (is (= 1.5 (reference-plus 1.5 0)))
    (is (= 0.0 (reference-plus -0.0 0)))
    (is (= -0.0 (reference-plus -0.0 -0.0))))
  (testing "long arithmetic throws on overflow rather than wrapping"
    (is (= [:throws "ArithmeticException" "long overflow"]
           (reference-plus Long/MAX_VALUE 1)))
    (is (= Long/MIN_VALUE (unchecked-add Long/MAX_VALUE 1))))
  (testing "a bigint operand promotes instead of overflowing"
    (is (= 9223372036854775808N (reference-plus Long/MAX_VALUE 1N))))
  (testing "every case outcome satisfies the declared contract"
    (is (every? outcome? [(reference-plus) (reference-plus 1 2)
                          (reference-plus Long/MAX_VALUE 1) (reference-plus 1/3 1/6)]))))
