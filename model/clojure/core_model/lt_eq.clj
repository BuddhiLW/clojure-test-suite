(ns clojure.core-model.lt-eq
  "Behaviour model for `clojure.core/<=`. Emits `model/golden/lt_eq.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `<=` orders: the whole numeric tower. Arity 1 never inspects its
  argument, so the domain only binds from arity 2 up."
  number?)

(def Outcome
  "One call recorded as data: the arguments, plus either the value returned or
  the simple name of the class thrown."
  [:map
   [:args [:vector :any]]
   [:value {:optional true} :any]
   [:throws {:optional true} :string]])

(defn reference-lt-eq
  "`<=`, delegated to the host so the model characterizes rather than restates."
  ([x] (<= x))
  ([x y] (<= x y))
  ([x y & more] (apply <= x y more)))

(m/=> reference-lt-eq [:=> [:cat :any [:* :any]] :boolean])

(defn lt-eq-outcome
  "`<=` over `args`, encoded totally: a throwing edge is recorded as data
  rather than propagated, so a golden case records the outcome instead of
  crashing."
  [& args]
  (let [args (vec args)]
    (try {:args args :value (apply reference-lt-eq args)}
         (catch Throwable t {:args args :throws (.getSimpleName (class t))}))))

(m/=> lt-eq-outcome [:=> [:cat [:* :any]] Outcome])

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
  "Every number is `<=` itself except NaN, which is unordered and so is not
  `<=` anything, itself included."
  [x]
  (if (nan? x)
    (false? (reference-lt-eq x x))
    (true? (reference-lt-eq x x))))

(defn- compare-consistent?
  "For a numeric pair with no NaN, `<=` agrees with the sign of `compare`. At
  NaN the two part company: `compare` answers 0 and `<=` answers false, so
  `<=` is NOT the ordering `compare` induces."
  [args]
  (if (and (= 2 (count args)) (every? number? args) (not-any? nan? args))
    (let [[a b] args]
      (= (reference-lt-eq a b) (not (pos? (compare a b)))))
    true))

(defn- weaker-than-lt?
  "`<=` holds wherever `<` holds."
  [[a b]]
  (or (not (< a b)) (reference-lt-eq a b)))

(defn- lt-eq-laws-hold?
  "Laws `<=` obeys over generated arguments: it reduces to the lazy conjunction
  over consecutive pairs, lands in the booleans, and agrees with `compare`
  away from NaN."
  [{:keys [args value] :as outcome}]
  (and (= (dissoc outcome :args) (chained-outcome reference-lt-eq args))
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
  "Argument vectors of arity 1 and up, a third of them already sorted (with
  repeats) so the true branch of a chain is reached as often as the false one."
  (gen/one-of [(gen/fmap vector number-gen)
               (gen/vector number-gen 2 4)
               (gen/fmap (comp vec sort)
                         (gen/vector (gen/elements [-1 0 0 1 1 2]) 2 4))]))

(deftrifecta lt-eq-behaviour
  clojure.core-model.lt-eq/lt-eq-outcome
  {:golden-path "model/golden/lt_eq.edn"
   :apply?      true
   :cases       {:arity-0                  []
                 :arity-1-number           [1]
                 :arity-1-string           ["abc"]
                 :arity-1-nil              [nil]
                 :long-ascending           [0 1]
                 :long-descending          [1 0]
                 :long-equal               [1 1]
                 :bigint-ascending         [0N 1N]
                 :double-ascending         [0.0 1.0]
                 :bigdec-ascending         [0.0M 1.0M]
                 :long-vs-double           [0 1.0]
                 :bigint-vs-bigdec         [0N 1.0M]
                 :long-vs-equal-double     [1 1.0]
                 :bigdec-differing-scale   [1M 1.0M]
                 :ratio-ascending          [1/16 1/2]
                 :ratio-vs-double          [1/16 0.5]
                 :ratio-equal              [1/2 1/2]
                 :neg-inf-vs-long          [##-Inf -1]
                 :long-vs-inf              [1 ##Inf]
                 :inf-vs-inf               [##Inf ##Inf]
                 :neg-inf-vs-neg-inf       [##-Inf ##-Inf]
                 :nan-on-the-right         [1 ##NaN]
                 :nan-on-the-left          [##NaN 1]
                 :nan-vs-nan               [##NaN ##NaN]
                 :signed-zero              [0.0 -0.0]
                 :signed-zero-reversed     [-0.0 0.0]
                 :long-max-vs-bigger-bigint [Long/MAX_VALUE (inc (bigint Long/MAX_VALUE))]
                 :chain-ascending          [0 1 2]
                 :chain-broken-at-head     [1 0 2]
                 :chain-not-monotone       [0 2 1]
                 :chain-with-a-plateau     [0 0 1]
                 :chain-plateau-at-the-end [0 1 1]
                 :chain-all-equal          [1 1 1]
                 :chain-across-infinities  [##-Inf 0 ##Inf]
                 :chain-range-10           (vec (range 10))
                 :chain-short-circuits-before-nil [1 0 nil]
                 :nil-on-the-left          [nil 1]
                 :nil-on-the-right         [1 nil]
                 :nil-late-in-chain        [1 2 nil]
                 :string-pair              ["a" "b"]
                 :char-pair                [\a \b]}
   :xf          pr-str
   :gen         args-gen
   :pred        lt-eq-laws-hold?
   :num-tests   500
   :mutations   [["always-true"
                  (fn [& args] {:args (vec args) :value true})]
                 ["always-false"
                  (fn [& args] {:args (vec args) :value false})]
                 ["strict"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply < args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["flipped"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply >= args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["negated-gt"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (not (apply > args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["compare-derived"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? (fn [[a b]] (not (pos? (compare a b))))
                                           (partition 2 1 args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["double-coerced"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply <= (map double args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["first-versus-all"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? #(<= (first args) %) (rest args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["eager-chain"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? true?
                                           (mapv (fn [[a b]] (<= a b))
                                                 (partition 2 1 args)))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]]})

(deftest lt-eq-laws
  (testing "reflexivity holds across the tower, and fails only at NaN"
    (is (every? reflexive-except-nan?
                [0 1 -1 0.0 -0.0 1.0 1/2 1M 1N ##Inf ##-Inf ##NaN
                 Long/MAX_VALUE Long/MIN_VALUE])))
  (testing "<= holds wherever < holds"
    (is (every? weaker-than-lt? [[0 1] [1 0] [1 1] [##-Inf 0] [1 ##NaN]])))
  (testing "<= is not the ordering compare induces: they disagree at NaN"
    (is (false? (reference-lt-eq 1 ##NaN)))
    (is (zero? (compare 1 ##NaN)))
    (is (false? (reference-lt-eq ##NaN ##NaN)))
    (is (zero? (compare ##NaN ##NaN))))
  (testing "<= is not the negation of >, because NaN falsifies both"
    (is (false? (reference-lt-eq ##NaN 1)))
    (is (false? (> ##NaN 1))))
  (testing "arity 1 answers true without inspecting its argument"
    (is (true? (reference-lt-eq "abc")))
    (is (true? (reference-lt-eq nil))))
  (testing "the chain short-circuits, so a later non-number is never reached"
    (is (false? (:value (lt-eq-outcome 1 0 nil))))
    (is (= "NullPointerException" (:throws (lt-eq-outcome 1 2 nil)))))
  (testing "the zero-arity edge is recorded, not hidden"
    (is (= "ArityException" (:throws (lt-eq-outcome))))))
