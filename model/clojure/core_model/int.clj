(ns clojure.core-model.int
  "Behaviour model for `clojure.core/int`. Emits `model/golden/int.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def ^:private int-max 2147483647)
(def ^:private int-min -2147483648)

(defn- coercible?
  "Whether the host maps `x` to an int instead of refusing. A boxed argument is
  truncated to a long first, so a fractional double inside the 32-bit range
  coerces; the refusal comes from the 64-bit narrowing or from the 32-bit one.
  A character coerces through its code point and NaN truncates to zero."
  [x]
  (cond
    (char? x)                                  true
    (not (number? x))                          false
    (Double/isNaN (double x))                  true
    (Double/isInfinite (double x))             false
    (not (<= Long/MIN_VALUE x Long/MAX_VALUE)) false
    :else                                      (<= int-min (bigint x) int-max)))

(def Coercible
  "The domain `int` maps to a value rather than refusing."
  [:and [:or number? char?] [:fn coercible?]])

(def Refusal
  "A host refusal recorded as data: the simple class name of the throw."
  [:map [::throws :string]])

(defn reference-int
  "`int`, delegated to the host so the model characterizes rather than restates."
  [x]
  (int x))

(m/=> reference-int [:=> [:cat Coercible] :int])

(defn- recording
  "Apply `f` to `x`, returning the host's refusal as a `Refusal` value instead of
  propagating it."
  [f x]
  (try (f x) (catch Throwable t {::throws (.getSimpleName (class t))})))

(defn probe-int
  "`reference-int` with a refusal recorded, so a golden case on a refused input
  is an outcome rather than an aborted run."
  [x]
  (recording reference-int x))

(m/=> probe-int [:=> [:cat :any] [:or :int Refusal]])

(defn- refusal?
  "Whether `r` is a recorded host refusal rather than a coerced value."
  [r]
  (and (map? r) (contains? r ::throws)))

(defn- encode
  "Total encoding of one outcome: the printed value with its host type, `nil`, or
  the refusal's exception class."
  [r]
  (cond
    (refusal? r) (str "throws " (::throws r))
    (nil? r)     "nil"
    :else        (str (pr-str r) " " (.getSimpleName (class r)))))

(defn- int-outcome?
  "Every `int` outcome is an integer inside the 32-bit range, or a named refusal."
  [r]
  (or (refusal? r)
      (and (integer? r) (<= int-min r int-max))))

(def ^:private inputs
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap #(/ % 7) gen/large-integer)
               gen/char
               (gen/return nil)
               gen/string
               gen/boolean
               gen/keyword]))

(deftrifecta int-behaviour
  clojure.core-model.int/probe-int
  {:golden-path "model/golden/int.edn"
   :cases       {:zero               0
                 :positive-long      7
                 :negative-long      -7
                 :truncates-up       3.7
                 :truncates-down     -3.7
                 :truncates-half     3.5
                 :truncates-neg-half -3.5
                 :ratio              7/2
                 :negative-ratio     -7/2
                 :small-ratio        1/10
                 :bigint             1N
                 :bigdec             1.9M
                 :char               \a
                 :int-max            2147483647
                 :int-min            -2147483648
                 :long-over-int      2147483648
                 :long-max           9223372036854775807
                 :double-over-int    2.5e10
                 :double-just-over   2147483647.4
                 :nan                ##NaN
                 :infinity           ##Inf
                 :negative-infinity  ##-Inf
                 :nil                nil
                 :string             "1"
                 :boolean            true
                 :keyword            :0}
   :xf          encode
   :gen         inputs
   :pred        int-outcome?
   :num-tests   500
   :mutations   [["identity"          (fn [x] x)]
                 ["widened-to-long"   (fn [x] (recording long x))]
                 ["rounds-half-up"    (fn [x] (recording #(int (Math/round (double %))) x))]
                 ["drops-sign"        (fn [x] (recording #(int (Math/abs (double %))) x))]
                 ["wraps-on-overflow" (fn [x] (recording #(unchecked-int (long %)) x))]
                 ["nan-refused"       (fn [x] (if (and (number? x) (Double/isNaN (double x)))
                                                {::throws "IllegalArgumentException"}
                                                (recording reference-int x)))]]})

(deftest int-laws
  (testing "the model's domain predicate agrees with the host"
    (is (every? (fn [x] (= (coercible? x) (not (refusal? (probe-int x)))))
                (gen/sample inputs 300))))
  (testing "truncation is toward zero, never rounding"
    (is (= [3 -3 3 -3 0 0] (map reference-int [3.7 -3.7 3.5 -3.5 1/2 -1/2]))))
  (testing "a character coerces through its code point"
    (is (= 97 (reference-int \a))))
  (testing "NaN truncates to zero while the infinities are refused"
    (is (= 0 (reference-int ##NaN)))
    (is (refusal? (probe-int ##Inf)))
    (is (refusal? (probe-int ##-Inf))))
  (testing "narrowing out of the 32-bit range refuses rather than wrapping"
    (is (= int-max (reference-int int-max)))
    (is (= int-min (reference-int int-min)))
    (is (refusal? (probe-int (inc (long int-max)))))
    (is (refusal? (probe-int (dec (long int-min)))))
    (is (= int-min (unchecked-int (inc (long int-max))))))
  (testing "a boxed argument narrows through 64 bits first, so a fractional
            double inside the 32-bit range coerces and the out-of-range refusal
            is an arithmetic overflow rather than an argument error"
    (is (= 2147483647 (reference-int 2147483647.4)))
    (is (= "throws ArithmeticException" (encode (probe-int 2147483648))))
    (is (= "throws ArithmeticException" (encode (probe-int 2.5e10))))
    (is (= "throws IllegalArgumentException" (encode (probe-int 1e19)))))
  (testing "the host boxes the result to a 32-bit integer"
    (is (instance? Integer (reference-int 0)))))
