(ns clojure.core-model.gt-eq
  "Behaviour model for `clojure.core/>=`. Emits `model/golden/gt_eq.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `>=` orders: the whole numeric tower. Arity 1 never inspects its
  argument, so the domain only binds from arity 2 up."
  number?)

(def Outcome
  "One call recorded as data: the arguments, plus either the value returned or
  the simple name of the class thrown."
  [:map
   [:args [:vector :any]]
   [:value {:optional true} :any]
   [:throws {:optional true} :string]])

(defn reference-gt-eq
  "`>=`, delegated to the host so the model characterizes rather than restates."
  ([x] (>= x))
  ([x y] (>= x y))
  ([x y & more] (apply >= x y more)))

(m/=> reference-gt-eq [:=> [:cat :any [:* :any]] :boolean])

(defn gt-eq-outcome
  "`>=` over `args`, encoded totally: a throwing edge is recorded as data
  rather than propagated, so a golden case records the outcome instead of
  crashing."
  [& args]
  (let [args (vec args)]
    (try {:args args :value (apply reference-gt-eq args)}
         (catch Throwable t {:args args :throws (.getSimpleName (class t))}))))

(m/=> gt-eq-outcome [:=> [:cat [:* :any]] Outcome])

(defn- nan?
  "Whether `x` is the NaN double."
  [x]
  (and (double? x) (Double/isNaN x)))

(defn- chained-outcome
  "The outcome of `pair-fn` folded over consecutive argument pairs, lazily —
  the shape every variadic comparison in clojure.core reduces to, including its
  short circuit before a later argument is ever inspected."
  [pair-fn args]
  (try {:value (every? (fn [[a b]] (pair-fn a b)) (partition 2 1 args))}
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn- reflexive-except-nan?
  "Every number is `>=` itself except NaN, which is unordered and so is not
  `>=` anything, itself included."
  [x]
  (if (nan? x)
    (false? (reference-gt-eq x x))
    (true? (reference-gt-eq x x))))

(defn- compare-consistent?
  "For a numeric pair with no NaN, `>=` agrees with the sign of `compare`. At
  NaN the two part company: `compare` answers 0 and `>=` answers false, so
  `>=` is NOT the ordering `compare` induces."
  [args]
  (if (and (= 2 (count args)) (every? number? args) (not-any? nan? args))
    (let [[a b] args]
      (= (reference-gt-eq a b) (not (neg? (compare a b)))))
    true))

(defn- dual-of-lt-eq?
  "`(>= a b)` is `(<= b a)`: `>=` is the argument-swapped dual, not a negation."
  [[a b]]
  (= (reference-gt-eq a b) (<= b a)))

(defn- gt-eq-laws-hold?
  "Laws `>=` obeys over generated arguments: it reduces to the lazy conjunction
  over consecutive pairs, lands in the booleans, and agrees with `compare`
  away from NaN."
  [{:keys [args value] :as outcome}]
  (and (= (dissoc outcome :args) (chained-outcome reference-gt-eq args))
       (or (some? (:throws outcome))
           (and (boolean? value) (compare-consistent? args)))))

(def ^:private number-gen
  (gen/one-of [gen/small-integer
               gen/large-integer
               gen/double
               (gen/fmap double gen/small-integer)
               (gen/fmap bigint gen/large-integer)
               (gen/fmap bigdec gen/small-integer)
               (gen/fmap #(/ % 7) gen/small-integer)
               (gen/elements [0 0.0 -0.0 1 1.0 1N 1M 1/2 ##NaN ##Inf ##-Inf
                              Long/MAX_VALUE Long/MIN_VALUE])]))

(def ^:private args-gen
  "Argument vectors of arity 1 and up, a third of them already descending (with
  repeats) so the true branch of a chain is reached as often as the false one."
  (gen/one-of [(gen/fmap vector number-gen)
               (gen/vector number-gen 2 4)
               (gen/fmap (comp vec reverse sort)
                         (gen/vector (gen/elements [-1 0 0 1 1 2]) 2 4))]))

(deftrifecta gt-eq-behaviour
  clojure.core-model.gt-eq/gt-eq-outcome
  {:golden-path "model/golden/gt_eq.edn"
   :apply?      true
   :cases       {:arity-0                  []
                 :arity-1-number           [1]
                 :arity-1-string           ["abc"]
                 :arity-1-nil              [nil]
                 :long-descending          [1 0]
                 :long-ascending           [0 1]
                 :long-equal               [1 1]
                 :bigint-descending        [1N 0N]
                 :double-descending        [1.0 0.0]
                 :bigdec-descending        [1.0M 0.0M]
                 :double-vs-long           [1.0 0]
                 :bigdec-vs-bigint         [1.0M 0N]
                 :long-vs-equal-double     [1 1.0]
                 :bigdec-differing-scale   [1M 1.0M]
                 :ratio-descending         [1/2 1/16]
                 :double-vs-ratio          [0.5 1/16]
                 :ratio-equal              [1/2 1/2]
                 :long-vs-neg-inf          [-1 ##-Inf]
                 :inf-vs-long              [##Inf 1]
                 :inf-vs-inf               [##Inf ##Inf]
                 :neg-inf-vs-neg-inf       [##-Inf ##-Inf]
                 :nan-on-the-right         [1 ##NaN]
                 :nan-on-the-left          [##NaN 1]
                 :nan-vs-nan               [##NaN ##NaN]
                 :signed-zero              [0.0 -0.0]
                 :signed-zero-reversed     [-0.0 0.0]
                 :bigger-bigint-vs-long-max [(inc (bigint Long/MAX_VALUE)) Long/MAX_VALUE]
                 :chain-descending         [2 1 0]
                 :chain-broken-at-head     [0 2 1]
                 :chain-not-monotone       [2 0 1]
                 :chain-with-a-plateau     [1 0 0]
                 :chain-plateau-at-the-end [1 1 0]
                 :chain-all-equal          [1 1 1]
                 :chain-across-infinities  [##Inf 0 ##-Inf]
                 :chain-reverse-range-10   (vec (reverse (range 10)))
                 :chain-short-circuits-before-nil [0 1 nil]
                 :nil-on-the-left          [nil 1]
                 :nil-on-the-right         [1 nil]
                 :nil-late-in-chain        [2 1 nil]
                 :string-pair              ["b" "a"]
                 :char-pair                [\b \a]}
   :xf          pr-str
   :gen         args-gen
   :pred        gt-eq-laws-hold?
   :num-tests   500
   :mutations   [["always-true"
                  (fn [& args] {:args (vec args) :value true})]
                 ["always-false"
                  (fn [& args] {:args (vec args) :value false})]
                 ["strict"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply > args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["flipped"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply <= args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["negated-lt"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (not (apply < args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["compare-derived"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? (fn [[a b]] (not (neg? (compare a b))))
                                           (partition 2 1 args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["double-coerced"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply >= (map double args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["first-versus-all"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? #(>= (first args) %) (rest args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["eager-chain"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? true?
                                           (mapv (fn [[a b]] (>= a b))
                                                 (partition 2 1 args)))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]]})

(deftest gt-eq-laws
  (testing "reflexivity holds across the tower, and fails only at NaN"
    (is (every? reflexive-except-nan?
                [0 1 -1 0.0 -0.0 1.0 1/2 1M 1N ##Inf ##-Inf ##NaN
                 Long/MAX_VALUE Long/MIN_VALUE])))
  (testing ">= is <= with the arguments swapped, not the negation of <"
    (is (every? dual-of-lt-eq? [[1 0] [0 1] [1 1] [1 ##NaN] [##NaN 1] [1 1.0]]))
    (is (not= (reference-gt-eq 1 ##NaN) (not (< 1 ##NaN)))))
  (testing ">= is not the ordering compare induces: they disagree at NaN"
    (is (false? (reference-gt-eq 1 ##NaN)))
    (is (zero? (compare 1 ##NaN)))
    (is (false? (reference-gt-eq ##NaN ##NaN)))
    (is (zero? (compare ##NaN ##NaN))))
  (testing "arity 1 answers true without inspecting its argument"
    (is (true? (reference-gt-eq "abc")))
    (is (true? (reference-gt-eq nil))))
  (testing "the chain short-circuits, so a later non-number is never reached"
    (is (false? (:value (gt-eq-outcome 0 1 nil))))
    (is (= "NullPointerException" (:throws (gt-eq-outcome 2 1 nil)))))
  (testing "the zero-arity edge is recorded, not hidden"
    (is (= "ArityException" (:throws (gt-eq-outcome))))))
