(ns clojure.core-model.long
  "Behaviour model for `clojure.core/long`. Emits `model/golden/long.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def ^:private long-max Long/MAX_VALUE)
(def ^:private long-min Long/MIN_VALUE)

(defn- coercible?
  "Whether the host maps `x` to a long instead of refusing. A character coerces
  through its code point; NaN truncates to zero; the infinities are refused, as
  is any magnitude the 64-bit range cannot hold."
  [x]
  (cond
    (char? x)                      true
    (not (number? x))              false
    (Double/isNaN (double x))      true
    (Double/isInfinite (double x)) false
    :else                          (<= long-min x long-max)))

(def Coercible
  "The domain `long` maps to a value rather than refusing."
  [:and [:or number? char?] [:fn coercible?]])

(def Refusal
  "A host refusal recorded as data: the simple class name of the throw."
  [:map [::throws :string]])

(defn reference-long
  "`long`, delegated to the host so the model characterizes rather than restates."
  [x]
  (long x))

(m/=> reference-long [:=> [:cat Coercible] :int])

(defn- recording
  "Apply `f` to `x`, returning the host's refusal as a `Refusal` value instead of
  propagating it."
  [f x]
  (try (f x) (catch Throwable t {::throws (.getSimpleName (class t))})))

(defn probe-long
  "`reference-long` with a refusal recorded, so a golden case on a refused input
  is an outcome rather than an aborted run."
  [x]
  (recording reference-long x))

(m/=> probe-long [:=> [:cat :any] [:or :int Refusal]])

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

(defn- long-outcome?
  "Every `long` outcome is an integer inside the 64-bit range, or a named
  refusal."
  [r]
  (or (refusal? r)
      (and (integer? r) (<= long-min r long-max))))

(def ^:private inputs
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap #(/ % 7) gen/large-integer)
               (gen/fmap #(*' % %) gen/large-integer)
               gen/char
               (gen/return nil)
               gen/string
               gen/boolean
               gen/keyword]))

(deftrifecta long-behaviour
  clojure.core-model.long/probe-long
  {:golden-path "model/golden/long.edn"
   :cases       {:zero               0
                 :positive           7
                 :negative           -7
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
                 :int-value          2147483648
                 :long-max           9223372036854775807
                 :long-min           -9223372036854775808
                 :bigint-over-long   9223372036854775808N
                 :double-at-long-max 9.223372036854776E18
                 :double-over-long   1.0E19
                 :nan                ##NaN
                 :infinity           ##Inf
                 :negative-infinity  ##-Inf
                 :nil                nil
                 :string             "1"
                 :boolean            true
                 :keyword            :0}
   :xf          encode
   :gen         inputs
   :pred        long-outcome?
   :num-tests   500
   :mutations   [["identity"          (fn [x] x)]
                 ["narrowed-to-int"   (fn [x] (recording int x))]
                 ["rounds-half-up"    (fn [x] (recording #(Math/round (double %)) x))]
                 ["drops-sign"        (fn [x] (recording #(long (Math/abs (double %))) x))]
                 ["floors"            (fn [x] (recording #(long (Math/floor (double %))) x))]
                 ["nan-refused"       (fn [x] (if (and (number? x) (Double/isNaN (double x)))
                                                {::throws "IllegalArgumentException"}
                                                (recording reference-long x)))]]})

(deftest long-laws
  (testing "the model's domain predicate agrees with the host"
    (is (every? (fn [x] (= (coercible? x) (not (refusal? (probe-long x)))))
                (gen/sample inputs 300))))
  (testing "truncation is toward zero, never rounding or flooring"
    (is (= [3 -3 3 -3 0 0] (map reference-long [3.7 -3.7 3.5 -3.5 1/2 -1/2]))))
  (testing "a character coerces through its code point"
    (is (= 97 (reference-long \a))))
  (testing "NaN truncates to zero while the infinities are refused"
    (is (= 0 (reference-long ##NaN)))
    (is (refusal? (probe-long ##Inf)))
    (is (refusal? (probe-long ##-Inf))))
  (testing "an integer outside the 64-bit range is refused rather than wrapped"
    (is (= long-max (reference-long long-max)))
    (is (= long-min (reference-long long-min)))
    (is (refusal? (probe-long (inc' long-max))))
    (is (refusal? (probe-long (dec' long-min)))))
  (testing "the range check runs in double precision, so the double nearest
            2^63 saturates to Long/MAX_VALUE instead of being refused"
    (is (= long-max (reference-long (double long-max))))
    (is (= (double long-max) (double (dec long-max))))
    (is (refusal? (probe-long 1.0E19))))
  (testing "widening a long to a double loses the low bits, so the round trip is
            not the identity"
    (is (not= (dec long-max) (reference-long (double (dec long-max))))))
  (testing "the host boxes the result to a 64-bit integer"
    (is (instance? Long (reference-long 0)))))
