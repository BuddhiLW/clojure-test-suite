(ns clojure.core-model.float
  "Behaviour model for `clojure.core/float`. Emits `model/golden/float.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def ^:private float-max Float/MAX_VALUE)

(defn- coercible?
  "Whether the host narrows `x` to a float instead of refusing. NaN does; the
  infinities and every magnitude beyond the float range do not. A magnitude
  below the smallest subnormal underflows to zero rather than being refused."
  [x]
  (cond
    (not (number? x))         false
    (Double/isNaN (double x)) true
    :else                     (<= (- float-max) x float-max)))

(def Coercible
  "The domain `float` maps to a value rather than refusing."
  [:and [:fn number?] [:fn coercible?]])

(def Refusal
  "A host refusal recorded as data: the simple class name of the throw."
  [:map [::throws :string]])

(defn reference-float
  "`float`, delegated to the host so the model characterizes rather than
  restates."
  [x]
  (float x))

(m/=> reference-float [:=> [:cat Coercible] :double])

(defn- recording
  "Apply `f` to `x`, returning the host's refusal as a `Refusal` value instead of
  propagating it."
  [f x]
  (try (f x) (catch Throwable t {::throws (.getSimpleName (class t))})))

(defn probe-float
  "`reference-float` with a refusal recorded, so a golden case on a refused input
  is an outcome rather than an aborted run."
  [x]
  (recording reference-float x))

(m/=> probe-float [:=> [:cat :any] [:or :double Refusal]])

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

(defn- float-outcome?
  "Every `float` outcome is a 32-bit floating value or a named refusal."
  [r]
  (or (refusal? r) (instance? Float r)))

(def ^:private inputs
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap #(/ % 7) gen/large-integer)
               (gen/fmap #(*' % % % %) gen/large-integer)
               (gen/fmap bigdec gen/large-integer)
               gen/char
               (gen/return nil)
               gen/string
               gen/boolean
               gen/keyword]))

(deftrifecta float-behaviour
  clojure.core-model.float/probe-float
  {:golden-path "model/golden/float.edn"
   :cases       {:zero                0
                 :one                 1
                 :negative-one        -1
                 :tenth               0.1
                 :ratio               1/3
                 :bigint              1N
                 :bigdec              1.0M
                 :long-max            9223372036854775807
                 :float-max           (double Float/MAX_VALUE)
                 :over-float          1.0E39
                 :max-double          1.7976931348623157E308
                 :min-double          4.9E-324
                 :under-float         1.0E-46
                 :min-subnormal-float 1.4E-45
                 :nan                 ##NaN
                 :infinity            ##Inf
                 :negative-infinity   ##-Inf
                 :negative-zero       -0.0
                 :char                \a
                 :nil                 nil
                 :string              "1"
                 :boolean             true
                 :keyword             :0}
   :xf          encode
   :gen         inputs
   :pred        float-outcome?
   :num-tests   500
   :mutations   [["identity"             (fn [x] x)]
                 ["widened-to-double"    (fn [x] (recording double x))]
                 ["rounds-to-nearest"    (fn [x] (recording #(float (Math/rint (double %))) x))]
                 ["accepts-char"         (fn [x] (if (char? x)
                                                   (float (long x))
                                                   (recording reference-float x)))]
                 ["saturates-on-overflow" (fn [x] (let [r (recording reference-float x)]
                                                    (if (and (refusal? r) (number? x))
                                                      Float/POSITIVE_INFINITY
                                                      r)))]
                 ["refuses-nan"          (fn [x] (if (and (number? x) (Double/isNaN (double x)))
                                                   {::throws "IllegalArgumentException"}
                                                   (recording reference-float x)))]
                 ["refuses-underflow"    (fn [x] (let [r (recording reference-float x)]
                                                   (if (and (instance? Float r)
                                                            (zero? r)
                                                            (number? x)
                                                            (not (zero? x)))
                                                     {::throws "IllegalArgumentException"}
                                                     r)))]]})

(deftest float-laws
  (testing "the model's domain predicate agrees with the host"
    (is (every? (fn [x] (= (coercible? x) (not (refusal? (probe-float x)))))
                (gen/sample inputs 300))))
  (testing "narrowing to 32 bits loses precision silently — the value still
            prints short, and only widening it back reveals the loss"
    (is (= "0.1" (pr-str (reference-float 0.1))))
    (is (not= 0.1 (double (reference-float 0.1))))
    (is (= 0.10000000149011612 (double (reference-float 0.1))))
    (is (= "0.33333334" (pr-str (reference-float 1/3))))
    (is (not= 0.33333334 (reference-float 1/3)))
    (is (= 0.3333333432674408 (double (reference-float 1/3)))))
  (testing "a magnitude beyond the float range is refused, where `double`
            saturates the same input to an infinity"
    (is (refusal? (probe-float 1.0E39)))
    (is (refusal? (probe-float 1.7976931348623157E308)))
    (is (= ##Inf (double 1e400M))))
  (testing "NaN narrows but the infinities are refused"
    (is (Double/isNaN (double (reference-float ##NaN))))
    (is (refusal? (probe-float ##Inf)))
    (is (refusal? (probe-float ##-Inf))))
  (testing "underflow below the smallest subnormal is silent: zero, not a refusal"
    (is (= 0.0 (double (reference-float 4.9E-324))))
    (is (= 0.0 (double (reference-float 1.0E-46))))
    (is (not (zero? 1.0E-46)))
    (is (not (zero? (reference-float 1.4E-45)))))
  (testing "a character is refused, unlike `int` and `long` which coerce it"
    (is (refusal? (probe-float \a)))
    (is (= 97 (long \a))))
  (testing "the host boxes the result to a 32-bit floating value"
    (is (instance? Float (reference-float 0)))
    (is (instance? Float (reference-float 0.0M)))))
