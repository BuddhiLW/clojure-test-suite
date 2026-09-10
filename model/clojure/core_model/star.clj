(ns clojure.core-model.star
  "Behaviour model for `clojure.core/*`. Emits `model/golden/star.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The numeric tower `*` accepts."
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

(defn reference-star
  "`*` as a value, delegated to the host so the model characterizes rather than
  restates. Returns the host's answer, or a recorded throw."
  [& args]
  (attempt * args))

(m/=> reference-star [:=> [:cat [:* Numeric]] Outcome])

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
  "`(* a b)` = `(* b a)`, with NaN excluded as unordered."
  [a b]
  (let [l (reference-star a b)]
    (or (nan? l) (= l (reference-star b a)))))

(def ^:private gen-operand
  "Operands spanning the tower a dialect must get right: small and full-range
  longs, doubles (NaN and the infinities included), and ratios."
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap (fn [[a b]] (/ a (if (zero? b) 1 b)))
                         (gen/tuple gen/small-integer gen/small-integer))]))

(deftrifecta star-behaviour
  clojure.core-model.star/reference-star
  {:golden-path "model/golden/star.edn"
   :apply?      true
   :cases       {:no-args           []
                 :unary-long        [7]
                 :unary-double      [1.5]
                 :long-long         [3 4]
                 :long-double       [3 1.5]
                 :double-double     [0.1 0.3]
                 :variadic          [2 3 4]
                 :overflow-max      [Long/MAX_VALUE 2]
                 :overflow-min-neg  [Long/MIN_VALUE -1]
                 :max-times-one     [Long/MAX_VALUE 1]
                 :bigint-promotes   [Long/MAX_VALUE 2N]
                 :ratio-collapses   [1/2 4]
                 :ratio-stays-ratio [1/2 1/3]
                 :bigdec            [1.0M 2]
                 :nan               [2.0 ##NaN]
                 :zero-times-inf    [0 ##Inf]
                 :zero-times-nan    [0 ##NaN]
                 :sign-onto-zero    [-1 0.0]
                 :negative-zeros    [0.0 -0.0]
                 :inf-times-inf     [##Inf ##-Inf]}
   :xf          pr-str
   :gen         (gen/vector gen-operand 0 4)
   :pred        outcome?
   :num-tests   500
   :mutations   [["always-one"          (fn [& _] 1)]
                 ["adds"                (fn [& args] (attempt + args))]
                 ["unchecked-multiply"  (fn [& args] (reduce unchecked-multiply 1 args))]
                 ["zero-short-circuits" (fn [& args]
                                          (if (some #(and (number? %) (not (nan? %)) (zero? %)) args)
                                            0
                                            (attempt * args)))]
                 ["loses-throw"         (fn [& args] (try (apply * args) (catch Throwable _ nil)))]]})

(deftest star-laws
  (testing "commutativity holds across the whole generated domain"
    (is (:pass? (tc/quick-check 300 (prop/for-all [a gen-operand b gen-operand]
                                      (commutative? a b))))))
  (testing "1 is the identity and the empty product"
    (is (= 1 (reference-star)))
    (is (= 7 (reference-star 7 1)))
    (is (= 1.5 (reference-star 1.5 1))))
  (testing "zero absorbs only in the finite domain"
    (is (= 0 (reference-star 0 Long/MAX_VALUE)))
    (is (nan? (reference-star 0 ##Inf)))
    (is (nan? (reference-star 0 ##NaN))))
  (testing "long arithmetic throws on overflow rather than wrapping"
    (is (= [:throws "ArithmeticException" "long overflow"]
           (reference-star Long/MAX_VALUE 2)))
    (is (= [:throws "ArithmeticException" "long overflow"]
           (reference-star Long/MIN_VALUE -1)))
    (is (= -2 (unchecked-multiply Long/MAX_VALUE 2))))
  (testing "a bigint operand promotes instead of overflowing"
    (is (= 18446744073709551614N (reference-star Long/MAX_VALUE 2N))))
  (testing "every case outcome satisfies the declared contract"
    (is (every? outcome? [(reference-star) (reference-star 3 4)
                          (reference-star Long/MAX_VALUE 2) (reference-star 1/2 1/3)]))))
