(ns clojure.core-model.num
  "Behaviour model for `clojure.core/num`. Emits `model/golden/num.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(defn- coercible?
  "Whether the host returns from `num` instead of refusing: every number, and
  nil, which passes straight through. Nothing else."
  [x]
  (or (nil? x) (number? x)))

(def Coercible
  "The domain `num` returns from rather than refusing."
  [:fn coercible?])

(def Refusal
  "A host refusal recorded as data: the simple class name of the throw."
  [:map [::throws :string]])

(defn reference-num
  "`num`, delegated to the host so the model characterizes rather than restates."
  [x]
  (num x))

(m/=> reference-num [:=> [:cat Coercible] [:maybe [:fn number?]]])

(defn- recording
  "Apply `f` to `x`, returning the host's refusal as a `Refusal` value instead of
  propagating it."
  [f x]
  (try (f x) (catch Throwable t {::throws (.getSimpleName (class t))})))

(defn probe-num
  "`reference-num` with a refusal recorded, so a golden case on a refused input
  is an outcome rather than an aborted run."
  [x]
  (recording reference-num x))

(m/=> probe-num [:=> [:cat :any] [:or [:maybe [:fn number?]] Refusal]])

(defn- refusal?
  "Whether `r` is a recorded host refusal rather than a returned value."
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

(defn- num-outcome?
  "Every `num` outcome is a number, nil, or a named refusal."
  [r]
  (or (refusal? r) (nil? r) (number? r)))

(def ^:private inputs
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap #(/ % 7) gen/large-integer)
               (gen/fmap bigdec gen/large-integer)
               (gen/fmap int gen/small-integer)
               (gen/fmap float gen/small-integer)
               gen/char
               (gen/return nil)
               gen/string
               gen/boolean
               gen/keyword]))

(deftrifecta num-behaviour
  clojure.core-model.num/probe-num
  {:golden-path "model/golden/num.edn"
   :cases       {:zero          0
                 :long          1
                 :double        0.1
                 :ratio         1/2
                 :bigint        1N
                 :bigdec        1.0M
                 :int           (int 1)
                 :byte          (byte 1)
                 :short         (short 1)
                 :float         (float 1.0)
                 :nan           ##NaN
                 :infinity      ##Inf
                 :negative-zero -0.0
                 :nil           nil
                 :char          \a
                 :string        "1"
                 :boolean       true
                 :keyword       :0
                 :vector        []
                 :map           {}
                 :set           #{}
                 :symbol        'a
                 :regex         #""
                 :function      inc}
   :xf          encode
   :gen         inputs
   :pred        num-outcome?
   :num-tests   500
   :mutations   [["identity"         (fn [x] x)]
                 ["refuses-nil"      (fn [x] (if (nil? x)
                                               {::throws "NullPointerException"}
                                               (recording reference-num x)))]
                 ["nil-becomes-zero" (fn [x] (if (nil? x) 0 (recording reference-num x)))]
                 ["widens-to-long"   (fn [x] (recording #(if (integer? %) (long %) (num %)) x))]
                 ["coerces-to-double" (fn [x] (recording double x))]
                 ["accepts-char"     (fn [x] (if (char? x)
                                               (long x)
                                               (recording reference-num x)))]
                 ["accepts-string"   (fn [x] (if (string? x)
                                               (Long/parseLong x)
                                               (recording reference-num x)))]]})

(deftest num-laws
  (testing "the model's domain predicate agrees with the host"
    (is (every? (fn [x] (= (coercible? x) (not (refusal? (probe-num x)))))
                (gen/sample inputs 300))))
  (testing "reached through a dynamic call `num` is the identity on numbers: it
            returns the very same object, so nothing is converted and nothing is
            lost"
    (is (every? (fn [x] (or (not (number? x)) (identical? x (reference-num x))))
                (gen/sample inputs 300)))
    (is (instance? Integer (reference-num (int 1))))
    (is (instance? Byte (reference-num (byte 1))))
    (is (instance? Float (reference-num (float 1.0)))))
  (testing "reached with a primitive argument the compiler picks an overload, and
            only the long and double ones exist: a narrow integral primitive
            boxes up to Long, a float has no overload and boxes as a Float"
    (is (instance? Long (num (int 1))))
    (is (instance? Long (num (byte 1))))
    (is (instance? Double (num 1.0)))
    (is (not (instance? Long (reference-num (int 1)))))
    (is (instance? Float (num (float 1.0)))))
  (testing "nil passes straight through — the one input `num` neither converts
            nor refuses"
    (is (nil? (reference-num nil)))
    (is (not (refusal? (probe-num nil)))))
  (testing "a non-number is refused with a cast error, characters included"
    (is (every? (comp refusal? probe-num) [\a "1" true :0 [] {} #{} 'a #"" inc]))
    (is (= "throws ClassCastException" (encode (probe-num \a)))))
  (testing "NaN, the infinities and negative zero pass through unchanged"
    (is (Double/isNaN (reference-num ##NaN)))
    (is (= ##Inf (reference-num ##Inf)))
    (is (= "-0.0" (pr-str (reference-num -0.0))))))
