(ns clojure.core-model.double
  "Behaviour model for `clojure.core/double`. Emits `model/golden/double.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(defn- coercible?
  "Whether the host widens `x` to a double instead of refusing. Every number
  does — magnitudes beyond the double range saturate to an infinity rather than
  being refused. Nothing else does, characters included."
  [x]
  (number? x))

(def Coercible
  "The domain `double` maps to a value rather than refusing."
  [:fn coercible?])

(def Refusal
  "A host refusal recorded as data: the simple class name of the throw."
  [:map [::throws :string]])

(defn reference-double
  "`double`, delegated to the host so the model characterizes rather than
  restates."
  [x]
  (double x))

(m/=> reference-double [:=> [:cat Coercible] :double])

(defn- recording
  "Apply `f` to `x`, returning the host's refusal as a `Refusal` value instead of
  propagating it."
  [f x]
  (try (f x) (catch Throwable t {::throws (.getSimpleName (class t))})))

(defn probe-double
  "`reference-double` with a refusal recorded, so a golden case on a refused
  input is an outcome rather than an aborted run."
  [x]
  (recording reference-double x))

(m/=> probe-double [:=> [:cat :any] [:or :double Refusal]])

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

(defn- double-outcome?
  "Every `double` outcome is a 64-bit floating value or a named refusal."
  [r]
  (or (refusal? r) (instance? Double r)))

(def ^:private inputs
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap #(/ % 7) gen/large-integer)
               (gen/fmap #(*' % % %) gen/large-integer)
               (gen/fmap bigdec gen/large-integer)
               gen/char
               (gen/return nil)
               gen/string
               gen/boolean
               gen/keyword]))

(deftrifecta double-behaviour
  clojure.core-model.double/probe-double
  {:golden-path "model/golden/double.edn"
   :cases       {:zero              0
                 :one               1
                 :negative-one      -1
                 :ratio             1/3
                 :bigint            1N
                 :bigdec            1.0M
                 :bigdec-overflow   1e400M
                 :bigint-huge       10000000000000000000000000000000000000000N
                 :long-max          9223372036854775807
                 :long-max-minus-one 9223372036854775806
                 :float-tenth       (float 0.1)
                 :double-tenth      0.1
                 :nan               ##NaN
                 :infinity          ##Inf
                 :negative-infinity ##-Inf
                 :negative-zero     -0.0
                 :char              \a
                 :nil               nil
                 :string            "1"
                 :boolean           true
                 :keyword           :0}
   :xf          encode
   :gen         inputs
   :pred        double-outcome?
   :num-tests   500
   :mutations   [["identity"         (fn [x] x)]
                 ["narrowed-to-float" (fn [x] (recording #(double (float %)) x))]
                 ["truncated-to-long" (fn [x] (recording #(double (long %)) x))]
                 ["rounds-to-nearest" (fn [x] (recording #(Math/rint (double %)) x))]
                 ["accepts-char"     (fn [x] (if (char? x)
                                               (double (long x))
                                               (recording reference-double x)))]
                 ["refuses-bigdec"   (fn [x] (if (decimal? x)
                                               {::throws "ClassCastException"}
                                               (recording reference-double x)))]]})

(deftest double-laws
  (testing "the model's domain predicate agrees with the host"
    (is (every? (fn [x] (= (coercible? x) (not (refusal? (probe-double x)))))
                (gen/sample inputs 300))))
  (testing "every number widens, but a character does not — the asymmetry with
            `int` and `long`, which both coerce a character"
    (is (every? (comp not refusal? probe-double) [0 1 -1 1/3 1N 1.0M ##NaN ##Inf]))
    (is (refusal? (probe-double \a)))
    (is (= 97 (long \a))))
  (testing "widening a 64-bit integer loses the low bits silently: two distinct
            longs land on the same double"
    (is (not= 9223372036854775807 9223372036854775806))
    (is (= (reference-double 9223372036854775807)
           (reference-double 9223372036854775806))))
  (testing "a magnitude beyond the double range saturates to an infinity rather
            than being refused — `float` refuses the same input"
    (is (= ##Inf (reference-double 1e400M)))
    (is (refusal? (recording float 1e400M))))
  (testing "NaN and the infinities pass through"
    (is (Double/isNaN (reference-double ##NaN)))
    (is (= ##Inf (reference-double ##Inf)))
    (is (= ##-Inf (reference-double ##-Inf))))
  (testing "negative zero is preserved, prints apart from zero and compares equal"
    (is (= "-0.0" (pr-str (reference-double -0.0))))
    (is (= 0.0 (reference-double -0.0)))
    (is (not= (pr-str 0.0) (pr-str (reference-double -0.0)))))
  (testing "the host boxes the result to a 64-bit floating value"
    (is (instance? Double (reference-double 0)))
    (is (instance? Double (reference-double 0.0M)))))
