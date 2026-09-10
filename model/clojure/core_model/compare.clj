(ns clojure.core-model.compare
  "Behaviour model for `clojure.core/compare`. Emits `model/golden/compare.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Orderable
  "The domain `compare` accepts: nil, plus any two values of one comparable
  kind. Mixing kinds is in the domain only in that it is refused."
  :any)

(def Outcome
  "One call recorded as data: the arguments, plus either the value returned or
  the simple name of the class thrown."
  [:map
   [:args [:vector Orderable]]
   [:value {:optional true} :any]
   [:throws {:optional true} :string]])

(defn reference-compare
  "`compare`, delegated to the host so the model characterizes rather than
  restates."
  [a b]
  (compare a b))

(m/=> reference-compare [:=> [:cat Orderable Orderable] :int])

(defn compare-outcome
  "`compare` over `args`, encoded totally: a throwing edge — a refused pair or
  a wrong arity — is recorded as data rather than propagated, so a golden case
  records the outcome instead of crashing."
  [& args]
  (let [args (vec args)]
    (try {:args args :value (apply reference-compare args)}
         (catch Throwable t {:args args :throws (.getSimpleName (class t))}))))

(m/=> compare-outcome [:=> [:cat [:* Orderable]] Outcome])

(defn- attempt
  "`compare` on one pair, as data. Independent of the trifecta subject so the
  laws still hold while that subject is mutated."
  [a b]
  (try {:value (reference-compare a b)}
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn- sign
  "-1, 0 or 1. `compare` promises a sign, not a magnitude."
  [n]
  (cond (neg? n) -1 (pos? n) 1 :else 0))

(defn- zero-on-identity?
  "A value compares 0 against ITSELF whatever its type, because `compare`
  short-circuits on object identity before it reaches Comparable — so a hash
  set, which refuses comparison with any other set, compares 0 with itself."
  [x]
  (= {:value 0} (attempt x x)))

(defn- nil-sorts-first?
  "nil precedes every non-nil value, and follows none."
  [x]
  (and (= {:value -1} (attempt nil x))
       (= {:value 1} (attempt x nil))))

(defn- sign-antisymmetric?
  "Swapping the arguments negates the SIGN of the answer, not the answer."
  [[a b]]
  (let [forward (attempt a b)
        backward (attempt b a)]
    (if (:throws forward)
      (some? (:throws backward))
      (and (some? (:value backward))
           (= (sign (:value forward)) (- (sign (:value backward))))))))

(defn- refusal-symmetric?
  "A pair `compare` refuses in one order is refused in the other."
  [[a b]]
  (= (some? (:throws (attempt a b)))
     (some? (:throws (attempt b a)))))

(defn- compare-laws-hold?
  "Laws `compare` obeys over generated pairs: it answers an integer or refuses,
  it refuses symmetrically, and swapping the arguments negates the sign."
  [{:keys [args value] :as outcome}]
  (and (or (some? (:throws outcome)) (int? value))
       (refusal-symmetric? args)
       (sign-antisymmetric? args)))

(def ^:private value-gen
  (gen/one-of [gen/small-integer
               gen/double
               gen/boolean
               gen/keyword
               gen/symbol
               gen/string-ascii
               gen/char-ascii
               (gen/return nil)
               (gen/fmap bigint gen/small-integer)
               (gen/fmap bigdec gen/small-integer)
               (gen/fmap #(/ % 7) gen/small-integer)
               (gen/vector gen/small-integer 0 3)
               (gen/fmap set (gen/vector gen/small-integer 0 2))
               (gen/map gen/keyword gen/small-integer {:max-elements 2})
               (gen/elements [##NaN ##Inf ##-Inf 0 0.0 -0.0 1 1.0 1N 1M 1/2])]))

(def ^:private args-gen
  "Pairs, half of them the same object twice so the identity short-circuit is
  reached as often as the Comparable path."
  (gen/one-of [(gen/vector value-gen 2 2)
               (gen/fmap (fn [x] [x x]) value-gen)]))

(deftrifecta compare-behaviour
  clojure.core-model.compare/compare-outcome
  {:golden-path "model/golden/compare.edn"
   :apply?      true
   :cases       {:arity-0                  []
                 :arity-1                  [1]
                 :arity-3                  [1 2 3]
                 :long-ascending           [0 10]
                 :long-equal               [0 0]
                 :long-vs-bigint           [0 -100N]
                 :long-vs-equal-double     [1 1.0]
                 :long-vs-ratio            [1 100/3]
                 :bigdec-differing-scale   [1M 1.0M]
                 :long-vs-nil              [1 nil]
                 :nil-vs-long              [nil 1]
                 :nil-vs-nil               [nil nil]
                 :nan-vs-long              [##NaN 1]
                 :long-vs-nan              [1 ##NaN]
                 :nan-vs-nan               [##NaN ##NaN]
                 :signed-zero              [0.0 -0.0]
                 :inf-vs-long              [##Inf 1]
                 :neg-inf-vs-inf           [##-Inf ##Inf]
                 :long-min-vs-long-max     [Long/MIN_VALUE Long/MAX_VALUE]
                 :char-ascending           [\a \b]
                 :char-equal               [\0 \0]
                 :char-descending          [\z \a]
                 :string-ascending         ["cat" "dog"]
                 :string-descending        ["b" "a"]
                 :string-magnitude         ["c" "a"]
                 :string-magnitude-deep    ["abcd" "abZ"]
                 :symbol-ascending         ['cat 'dog]
                 :symbol-vs-nil            ['a nil]
                 :keyword-ascending        [:cat :dog]
                 :keyword-equal            [:dog :dog]
                 :keyword-bare-vs-qualified [:cat :animal/cat]
                 :boolean-descending       [true false]
                 :boolean-ascending        [false true]
                 :empty-vectors            [[] []]
                 :vector-by-element        [[3] [1]]
                 :vector-by-length         [[] [1 2]]
                 :vector-nested-empty      [[] [[]]]
                 :vector-length-beats-element [[2] [1 1]]
                 :vector-vs-nil            [[] nil]
                 :vector-mixed-element     [[1] [[]]]
                 :vector-vs-list           [[] (list)]
                 :number-vs-vector         [1 []]
                 :string-vs-vector         ["a" []]
                 :string-vs-seq-of-chars   ["cat" (list \c \a \t)]
                 :set-vs-equal-set         [#{1} #{1}]
                 :map-vs-equal-map         [{1 2} {1 2}]
                 :seq-vs-equal-seq         [(range 5) (range 5)]
                 :identical-set            (let [s #{1}] [s s])
                 :identical-map            (let [m {1 2}] [m m])}
   :xf          pr-str
   :gen         args-gen
   :pred        compare-laws-hold?
   :num-tests   500
   :mutations   [["always-zero"
                  (fn [& args] {:args (vec args) :value 0})]
                 ["always-minus-one"
                  (fn [& args] {:args (vec args) :value -1})]
                 ["flipped"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (- (apply compare args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["sign-normalized"
                  (fn [& args]
                    (let [args (vec args)]
                      (try (let [c (apply compare args)]
                             {:args args :value (cond (neg? c) -1 (pos? c) 1 :else 0)})
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["zero-only-when-equal"
                  (fn [& args]
                    (let [args (vec args)]
                      (try (let [c (apply compare args)]
                             {:args args
                              :value (if (and (zero? c) (not (apply = args))) 1 c)})
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["nil-sorts-last"
                  (fn [& args]
                    (let [args (vec args)
                          [a b] args]
                      (try {:args args
                            :value (cond (and (nil? a) (nil? b)) 0
                                         (nil? a)                1
                                         (nil? b)                -1
                                         :else                   (compare a b))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["nan-gets-a-total-order"
                  (fn [& args]
                    (let [args (vec args)
                          [a b] args]
                      (try {:args args
                            :value (if (or (double? a) (double? b))
                                     (let [x (double a) y (double b)]
                                       (cond (< x y) -1 (> x y) 1 (= x y) 0 :else 1))
                                     (compare a b))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["strings-by-length"
                  (fn [& args]
                    (let [args (vec args)
                          [a b] args]
                      (try {:args args
                            :value (if (and (string? a) (string? b))
                                     (compare (count a) (count b))
                                     (compare a b))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]]})

(deftest compare-laws
  (testing "a value compares 0 against itself, Comparable or not"
    (is (every? zero-on-identity?
                [nil 0 0.0 ##NaN 1/2 "a" :a 'a \a true [1] #{1} {1 2} (range 3)])))
  (testing "nil sorts before everything else"
    (is (every? nil-sorts-first? [0 0.0 ##NaN "a" :a 'a \a true false [] [1]]))
    (is (zero? (reference-compare nil nil))))
  (testing "swapping the arguments negates the sign, and refusal is symmetric"
    (is (every? sign-antisymmetric?
                [[0 10] [\z \a] ["c" "a"] [1 nil] [##NaN 1] [[] [1 2]] [1 []]]))
    (is (every? refusal-symmetric?
                [[1 []] ["a" []] [[] (list)] [#{1} #{1}] [:a 1] ['a "a"]])))
  (testing "compare answers a sign for numbers but leaks a magnitude for text"
    (is (= 1 (reference-compare 10 -10)))
    (is (= 2 (reference-compare "c" "a")))
    (is (= 25 (reference-compare \z \a)))
    (is (= 9 (reference-compare "abcd" "abZ"))))
  (testing "compare 0 is not =: it merges the numeric tower and swallows NaN"
    (is (zero? (reference-compare 1 1.0)))
    (is (false? (= 1 1.0)))
    (is (zero? (reference-compare ##NaN 1)))
    (is (false? (= ##NaN 1))))
  (testing "compare's zero relation is not transitive, so it is not a total order"
    (is (zero? (reference-compare ##NaN 1)))
    (is (zero? (reference-compare ##NaN 2)))
    (is (neg? (reference-compare 1 2))))
  (testing "= is not compare 0 either: equal collections of different kinds are refused"
    (is (true? (= [1 2] (list 1 2))))
    (is (= "ClassCastException" (:throws (compare-outcome [1 2] (list 1 2))))))
  (testing "vectors order by length before element"
    (is (neg? (reference-compare [2] [1 1])))
    (is (pos? (reference-compare [3] [1]))))
  (testing "the wrong-arity edges are recorded, not hidden"
    (is (= "ArityException" (:throws (compare-outcome))))
    (is (= "ArityException" (:throws (compare-outcome 1))))
    (is (= "ArityException" (:throws (compare-outcome 1 2 3))))))
