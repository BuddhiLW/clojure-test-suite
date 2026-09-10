(ns clojure.core-model.lt
  "Behaviour model for `clojure.core/<`. Emits `model/golden/lt.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `<` orders: the whole numeric tower. Arity 1 never inspects its
  argument, so the domain only binds from arity 2 up."
  number?)

(def Outcome
  "One call recorded as data: the arguments, plus either the value returned or
  the simple name of the class thrown."
  [:map
   [:args [:vector :any]]
   [:value {:optional true} :any]
   [:throws {:optional true} :string]])

(defn reference-lt
  "`<`, delegated to the host so the model characterizes rather than restates."
  ([x] (< x))
  ([x y] (< x y))
  ([x y & more] (apply < x y more)))

(m/=> reference-lt [:=> [:cat :any [:* :any]] :boolean])

(defn lt-outcome
  "`<` over `args`, encoded totally: a throwing edge is recorded as data rather
  than propagated, so a golden case records the outcome instead of crashing."
  [& args]
  (let [args (vec args)]
    (try {:args args :value (apply reference-lt args)}
         (catch Throwable t {:args args :throws (.getSimpleName (class t))}))))

(m/=> lt-outcome [:=> [:cat [:* :any]] Outcome])

(defn- chained-outcome
  "The outcome of `pair-fn` folded over consecutive argument pairs, lazily —
  the shape every variadic comparison in clojure.core reduces to, including its
  short circuit before a later argument is ever inspected."
  [pair-fn args]
  (try {:value (every? (fn [[a b]] (pair-fn a b)) (partition 2 1 args))}
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn- irreflexive?
  "Nothing is `<` itself — infinities and NaN included."
  [x]
  (false? (reference-lt x x)))

(defn- nan-poisons?
  "Every comparison against NaN is false, in either position."
  [x]
  (and (false? (reference-lt x ##NaN))
       (false? (reference-lt ##NaN x))))

(defn- compare-consistent?
  "For a numeric pair, `<` agrees with the sign of `compare` — at NaN too,
  where `compare` answers 0 and `<` answers false."
  [args]
  (if (and (= 2 (count args)) (every? number? args))
    (let [[a b] args]
      (= (reference-lt a b) (neg? (compare a b))))
    true))

(defn- lt-laws-hold?
  "Laws `<` obeys over generated arguments: it reduces to the lazy conjunction
  over consecutive pairs, lands in the booleans, and agrees with `compare`."
  [{:keys [args value] :as outcome}]
  (and (= (dissoc outcome :args) (chained-outcome reference-lt args))
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
  "Argument vectors of arity 1 and up, a third of them already sorted so the
  true branch of a chain is reached as often as the false one."
  (gen/one-of [(gen/fmap vector number-gen)
               (gen/vector number-gen 2 4)
               (gen/fmap (comp vec sort) (gen/vector gen/small-integer 2 4))]))

(deftrifecta lt-behaviour
  clojure.core-model.lt/lt-outcome
  {:golden-path "model/golden/lt.edn"
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
                 :ratio-ascending          [1/16 1/2]
                 :ratio-vs-double          [1/16 0.5]
                 :ratio-equal              [1/2 1/2]
                 :neg-inf-vs-long          [##-Inf -1]
                 :long-vs-inf              [1 ##Inf]
                 :inf-vs-inf               [##Inf ##Inf]
                 :nan-on-the-right         [1 ##NaN]
                 :nan-on-the-left          [##NaN 1]
                 :nan-vs-nan               [##NaN ##NaN]
                 :signed-zero              [0.0 -0.0]
                 :long-max-vs-bigger-bigint [Long/MAX_VALUE (inc (bigint Long/MAX_VALUE))]
                 :chain-ascending          [0 1 2]
                 :chain-broken-at-head     [1 0 2]
                 :chain-not-monotone       [0 2 1]
                 :chain-all-equal          [1 1 1]
                 :chain-range-10           (vec (range 10))
                 :chain-short-circuits-before-nil [1 0 nil]
                 :nil-on-the-left          [nil 1]
                 :nil-on-the-right         [1 nil]
                 :nil-late-in-chain        [1 2 nil]
                 :string-pair              ["a" "b"]
                 :char-pair                [\a \b]}
   :xf          pr-str
   :gen         args-gen
   :pred        lt-laws-hold?
   :num-tests   500
   :mutations   [["always-true"
                  (fn [& args] {:args (vec args) :value true})]
                 ["always-false"
                  (fn [& args] {:args (vec args) :value false})]
                 ["non-strict"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply <= args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["flipped"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply > args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["double-coerced"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply < (map double args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["first-versus-all"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? #(< (first args) %) (rest args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["eager-chain"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? true?
                                           (mapv (fn [[a b]] (< a b))
                                                 (partition 2 1 args)))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]]})

(deftest lt-laws
  (testing "irreflexivity holds across the tower"
    (is (every? irreflexive? [0 1 -1 0.0 1.0 1/2 1M 1N ##Inf ##-Inf ##NaN
                              Long/MAX_VALUE Long/MIN_VALUE])))
  (testing "NaN is unordered, so every comparison against it is false"
    (is (every? nan-poisons? [0 1 -1 1.0 ##Inf ##-Inf ##NaN])))
  (testing "arity 1 answers true without inspecting its argument"
    (is (true? (reference-lt "abc")))
    (is (true? (reference-lt nil))))
  (testing "the chain short-circuits, so a later non-number is never reached"
    (is (false? (:value (lt-outcome 1 0 nil))))
    (is (= "NullPointerException" (:throws (lt-outcome 1 2 nil)))))
  (testing "mixing numeric categories is fine here, unlike under ="
    (is (true? (reference-lt 0 1.0)))
    (is (false? (reference-lt 1 1.0)))
    (is (false? (= 1 1.0))))
  (testing "the zero-arity edge is recorded, not hidden"
    (is (= "ArityException" (:throws (lt-outcome))))))
