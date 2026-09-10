(ns clojure.core-model.eq
  "Behaviour model for `clojure.core/=`. Emits `model/golden/eq.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Any
  "The domain `=` accepts: every value, nil included."
  :any)

(def Outcome
  "One call recorded as data: the arguments, plus either the value returned or
  the simple name of the class thrown."
  [:map
   [:args [:vector Any]]
   [:value {:optional true} Any]
   [:throws {:optional true} :string]])

(defn reference-eq
  "`=`, delegated to the host so the model characterizes rather than restates."
  ([x] (= x))
  ([x y] (= x y))
  ([x y & more] (apply = x y more)))

(m/=> reference-eq [:=> [:cat Any [:* Any]] :boolean])

(defn eq-outcome
  "`=` over `args`, encoded totally: a throwing edge is recorded as data rather
  than propagated, so a golden case records the outcome instead of crashing."
  [& args]
  (let [args (vec args)]
    (try {:args args :value (apply reference-eq args)}
         (catch Throwable t {:args args :throws (.getSimpleName (class t))}))))

(m/=> eq-outcome [:=> [:cat [:* Any]] Outcome])

(defn- chained-outcome
  "The outcome of `pair-fn` folded over consecutive argument pairs — the shape
  every variadic comparison in clojure.core reduces to."
  [pair-fn args]
  (let [args (vec args)]
    (try {:args args
          :value (every? (fn [[a b]] (pair-fn a b)) (partition 2 1 args))}
         (catch Throwable t {:args args :throws (.getSimpleName (class t))}))))

(defn- identity-reflexive?
  "A value is `=` to itself, NaN included: `=` short-circuits on object
  identity before it ever consults numeric equality."
  [x]
  (true? (reference-eq x x)))

(defn- fresh-nan
  "A NaN double that is not the same object as any other."
  []
  (* ##NaN 1.0))

(defn- order-invariant?
  "`=` asserts that its arguments are one value, so reordering cannot change
  the answer."
  [args]
  (= (:value (apply eq-outcome args))
     (:value (apply eq-outcome (reverse args)))))

(defn- eq-laws-hold?
  "Laws `=` obeys over generated arguments: total into the booleans, equal to
  the conjunction over consecutive pairs, and invariant under argument order."
  [{:keys [args value] :as outcome}]
  (and (nil? (:throws outcome))
       (boolean? value)
       (= value (:value (chained-outcome reference-eq args)))
       (order-invariant? args)))

(def ^:private scalar-gen
  (gen/one-of [gen/small-integer
               gen/double
               gen/boolean
               gen/keyword
               gen/string-ascii
               (gen/fmap double gen/small-integer)
               (gen/fmap bigint gen/small-integer)
               (gen/fmap bigdec gen/small-integer)
               (gen/fmap #(/ % 7) gen/small-integer)
               (gen/elements [nil true false 0 0.0 -0.0 1 1.0 1N 1M 1/2
                              ##NaN ##Inf ##-Inf \a "a" :a 'a])]))

(def ^:private value-gen
  (gen/one-of [scalar-gen
               (gen/vector scalar-gen 0 3)
               (gen/fmap #(apply list %) (gen/vector scalar-gen 0 3))
               (gen/fmap set (gen/vector scalar-gen 0 3))
               (gen/map scalar-gen scalar-gen {:max-elements 3})]))

(def ^:private args-gen
  "Argument vectors of arity 1 and up, half of them repetitions of one value so
  the true branch is reached as often as the false one."
  (gen/one-of [(gen/fmap vector value-gen)
               (gen/vector value-gen 2 4)
               (gen/fmap (fn [[x n]] (vec (repeat n x)))
                         (gen/tuple value-gen (gen/choose 2 4)))]))

(deftrifecta eq-behaviour
  clojure.core-model.eq/eq-outcome
  {:golden-path "model/golden/eq.edn"
   :apply?      true
   :cases       {:arity-0                 []
                 :arity-1                 [42]
                 :nil-nil                 [nil nil]
                 :nil-false               [nil false]
                 :true-true               [true true]
                 :char-same               [\a \a]
                 :char-different          [\a \b]
                 :string-same             ["green" "green"]
                 :string-vs-seq-of-chars  ["hello" (list \h \e \l \l \o)]
                 :keyword-same            [:and/ham :and/ham]
                 :keyword-vs-symbol       [:my/hello 'my/hello]
                 :symbol-same             ['two/fish 'two/fish]
                 :long-same               [42 42]
                 :long-vs-double          [2 2.0]
                 :long-vs-bigint          [1 1N]
                 :long-vs-bigdec          [1 1M]
                 :bigdec-differing-scale  [1M 1.0M]
                 :ratio-unreduced         [22/7 44/14]
                 :ratio-vs-double         [1/2 0.5]
                 :float-vs-double-exact   [(float 0.5) (double 0.5)]
                 :float-vs-double-inexact [(float 0.1) (double 0.1)]
                 :nan-vs-nan              [##NaN ##NaN]
                 :nan-in-list             [(list ##NaN) (list ##NaN)]
                 :nan-in-vector           [[##NaN] [##NaN]]
                 :nan-in-set              [#{##NaN} #{##NaN}]
                 :signed-zero             [0.0 -0.0]
                 :vector-vs-list          [[1 2 3] (list 1 2 3)]
                 :vector-vs-range         [[0 1 2] (range 3)]
                 :vector-vs-set           [[1 2 3] #{1 2 3}]
                 :vector-order            [[1 2 3] [3 2 1]]
                 :empty-list-vs-vector    [(list) []]
                 :empty-list-vs-set       [(list) #{}]
                 :empty-vector-vs-map     [[] {}]
                 :map-key-order           [{:a 1 "b" :2 3 \c \d 4}
                                           {"b" :2 \d 4 3 \c :a 1}]
                 :map-value-type          [{:a 1} {:a 1.0}]
                 :sorted-map-vs-hash-map  [{:b 14 :c 15 :a 13}
                                           (sorted-map :a 13 :b 14 :c 15)]
                 :sorted-set-vs-set       [#{6 4 2} (sorted-set 4 2 6)]
                 :nested-collections      [{:just (list :a {:plain [:simple #{:tailor}]})}
                                           {:just (list :a {:plain [:simple #{:tailor}]})}]
                 :variadic-all-equal      [2 2 2]
                 :variadic-break-in-middle [2 2 3 2]
                 :variadic-mixed-empties  [(list) [] [] (list) {}]
                 :variadic-nil-then-chars [nil \a \a \a]}
   :xf          pr-str
   :gen         args-gen
   :pred        eq-laws-hold?
   :num-tests   500
   :mutations   [["always-true"
                  (fn [& args] {:args (vec args) :value true})]
                 ["always-false"
                  (fn [& args] {:args (vec args) :value false})]
                 ["negated"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (not (apply = args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["numeric-equiv"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (if (every? number? args)
                                     (apply == args)
                                     (apply = args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["identity-only"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? #(identical? (first args) %) (rest args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["printed-form"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply = (map pr-str args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]]})

(deftest eq-laws
  (testing "identity reflexivity reaches even NaN"
    (is (every? identity-reflexive? [nil 0 0.0 -0.0 1/2 "a" :a [1] {} #{}]))
    (is (identity-reflexive? (fresh-nan))))
  (testing "two distinct NaN doubles are never equal"
    (is (false? (reference-eq (fresh-nan) (fresh-nan)))))
  (testing "argument order never changes the answer"
    (is (every? order-invariant? [[1 1.0] [1 1 2] [[1 2] (list 1 2)] [nil false]])))
  (testing "= partitions the numeric tower into categories, == does not"
    (is (false? (reference-eq 2 2.0)))
    (is (true? (== 2 2.0))))
  (testing "a NaN inside a list is equal, inside a vector it is not"
    (is (true? (reference-eq (list ##NaN) (list ##NaN))))
    (is (false? (reference-eq [##NaN] [##NaN]))))
  (testing "the zero-arity edge is recorded, not hidden"
    (is (= "ArityException" (:throws (eq-outcome))))))
