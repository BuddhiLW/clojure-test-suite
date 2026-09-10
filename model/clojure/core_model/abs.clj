(ns clojure.core-model.abs
  "Behaviour model for `clojure.core/abs`. Emits `model/golden/abs.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `abs` accepts."
  [:or :int :double])

(defn reference-abs
  "`abs`, delegated to the host so the model characterizes rather than restates."
  [x]
  (abs x))

(m/=> reference-abs [:=> [:cat Numeric] Numeric])

(def ^:private long-min-value Long/MIN_VALUE)

(defn non-negative-except-long-min?
  "`abs` is non-negative except at Long/MIN_VALUE, where negation overflows to
  itself. NaN is unordered and excluded."
  [x]
  (let [r (reference-abs x)]
    (cond
      (= x long-min-value)               (= r long-min-value)
      (and (double? r) (Double/isNaN r)) true
      :else                              (not (neg? r)))))

(defn idempotent?
  "`(abs (abs x))` = `(abs x)`."
  [x]
  (let [once (reference-abs x)]
    (or (and (double? once) (Double/isNaN once))
        (= once (reference-abs once)))))

(defn sign-symmetric?
  "`(abs x)` = `(abs (- x))`, except where negation itself overflows."
  [x]
  (if (= x long-min-value)
    true
    (let [a (reference-abs x)
          b (reference-abs (- x))]
      (or (and (double? a) (Double/isNaN a))
          (= a b)))))

(deftrifecta abs-behaviour
  clojure.core-model.abs/reference-abs
  {:golden-path "model/golden/abs.edn"
   :cases       {:zero            0
                 :positive-long   7
                 :negative-long   -7
                 :positive-double 1.5
                 :negative-double -1.5
                 :negative-zero   -0.0
                 :long-min        Long/MIN_VALUE
                 :long-max        Long/MAX_VALUE
                 :infinity        ##Inf
                 :negative-inf    ##-Inf
                 :nan             ##NaN}
   :xf          pr-str
   :gen         (gen/one-of [gen/small-integer gen/large-integer gen/double])
   :pred        non-negative-except-long-min?
   :num-tests   500
   ;; Each mutant must be REJECTED by the facets above; a survivor means the
   ;; model asserts nothing.
   :mutations   [["identity"    (fn [x] x)]
                 ["always-zero" (fn [_] 0)]
                 ["negated"     (fn [x] (- (abs x)))]
                 ["off-by-one"  (fn [x] (inc (abs x)))]]})

(deftest abs-laws
  (testing "idempotence"
    (is (every? idempotent? [0 7 -7 1.5 -1.5 ##Inf ##-Inf Long/MAX_VALUE])))
  (testing "sign symmetry"
    (is (every? sign-symmetric? [0 7 -7 1.5 -1.5 ##Inf ##-Inf])))
  (testing "the Long/MIN_VALUE overflow is characterized, not hidden"
    (is (= Long/MIN_VALUE (reference-abs Long/MIN_VALUE)))
    (is (neg? (reference-abs Long/MIN_VALUE)))))
