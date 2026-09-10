(ns clojure.core-model.gt
  "Behaviour model for `clojure.core/>`. Emits `model/golden/gt.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Numeric
  "The domain `>` orders: the whole numeric tower. Arity 1 never inspects its
  argument, so the domain only binds from arity 2 up."
  number?)

(def Outcome
  "One call recorded as data: the arguments, plus either the value returned or
  the simple name of the class thrown."
  [:map
   [:args [:vector :any]]
   [:value {:optional true} :any]
   [:throws {:optional true} :string]])

(defn reference-gt
  "`>`, delegated to the host so the model characterizes rather than restates."
  ([x] (> x))
  ([x y] (> x y))
  ([x y & more] (apply > x y more)))

(m/=> reference-gt [:=> [:cat :any [:* :any]] :boolean])

(defn gt-outcome
  "`>` over `args`, encoded totally: a throwing edge is recorded as data rather
  than propagated, so a golden case records the outcome instead of crashing."
  [& args]
  (let [args (vec args)]
    (try {:args args :value (apply reference-gt args)}
         (catch Throwable t {:args args :throws (.getSimpleName (class t))}))))

(m/=> gt-outcome [:=> [:cat [:* :any]] Outcome])

(defn- chained-outcome
  "The outcome of `pair-fn` folded over consecutive argument pairs, lazily —
  the shape every variadic comparison in clojure.core reduces to, including its
  short circuit before a later argument is ever inspected."
  [pair-fn args]
  (try {:value (every? (fn [[a b]] (pair-fn a b)) (partition 2 1 args))}
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(defn- irreflexive?
  "Nothing is `>` itself — infinities and NaN included."
  [x]
  (false? (reference-gt x x)))

(defn- nan-poisons?
  "Every comparison against NaN is false, in either position."
  [x]
  (and (false? (reference-gt x ##NaN))
       (false? (reference-gt ##NaN x))))

(defn- dual-of-lt?
  "`(> a b)` is `(< b a)`: `>` is the argument-swapped dual, not a negation."
  [[a b]]
  (= (reference-gt a b) (< b a)))

(defn- compare-consistent?
  "For a numeric pair, `>` agrees with the sign of `compare` — at NaN too,
  where `compare` answers 0 and `>` answers false."
  [args]
  (if (and (= 2 (count args)) (every? number? args))
    (let [[a b] args]
      (= (reference-gt a b) (pos? (compare a b))))
    true))

(defn- gt-laws-hold?
  "Laws `>` obeys over generated arguments: it reduces to the lazy conjunction
  over consecutive pairs, lands in the booleans, and agrees with `compare`."
  [{:keys [args value] :as outcome}]
  (and (= (dissoc outcome :args) (chained-outcome reference-gt args))
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
  "Argument vectors of arity 1 and up, a third of them already descending so
  the true branch of a chain is reached as often as the false one."
  (gen/one-of [(gen/fmap vector number-gen)
               (gen/vector number-gen 2 4)
               (gen/fmap (comp vec reverse sort) (gen/vector gen/small-integer 2 4))]))

(deftrifecta gt-behaviour
  clojure.core-model.gt/gt-outcome
  {:golden-path "model/golden/gt.edn"
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
                 :ratio-descending         [1/2 1/16]
                 :double-vs-ratio          [0.5 1/16]
                 :ratio-equal              [1/2 1/2]
                 :long-vs-neg-inf          [-1 ##-Inf]
                 :inf-vs-long              [##Inf 1]
                 :inf-vs-inf               [##Inf ##Inf]
                 :nan-on-the-right         [1 ##NaN]
                 :nan-on-the-left          [##NaN 1]
                 :nan-vs-nan               [##NaN ##NaN]
                 :signed-zero              [0.0 -0.0]
                 :bigger-bigint-vs-long-max [(inc (bigint Long/MAX_VALUE)) Long/MAX_VALUE]
                 :chain-descending         [2 1 0]
                 :chain-broken-at-head     [0 2 1]
                 :chain-not-monotone       [2 0 1]
                 :chain-all-equal          [1 1 1]
                 :chain-reverse-range-10   (vec (reverse (range 10)))
                 :chain-short-circuits-before-nil [0 1 nil]
                 :nil-on-the-left          [nil 1]
                 :nil-on-the-right         [1 nil]
                 :nil-late-in-chain        [2 1 nil]
                 :string-pair              ["b" "a"]
                 :char-pair                [\b \a]}
   :xf          pr-str
   :gen         args-gen
   :pred        gt-laws-hold?
   :num-tests   500
   :mutations   [["always-true"
                  (fn [& args] {:args (vec args) :value true})]
                 ["always-false"
                  (fn [& args] {:args (vec args) :value false})]
                 ["non-strict"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply >= args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["flipped"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply < args)}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["negated-lt"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (not (apply < args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["double-coerced"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args :value (apply > (map double args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["first-versus-all"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? #(> (first args) %) (rest args))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]
                 ["eager-chain"
                  (fn [& args]
                    (let [args (vec args)]
                      (try {:args args
                            :value (every? true?
                                           (mapv (fn [[a b]] (> a b))
                                                 (partition 2 1 args)))}
                           (catch Throwable t
                             {:args args :throws (.getSimpleName (class t))}))))]]})

(deftest gt-laws
  (testing "irreflexivity holds across the tower"
    (is (every? irreflexive? [0 1 -1 0.0 1.0 1/2 1M 1N ##Inf ##-Inf ##NaN
                              Long/MAX_VALUE Long/MIN_VALUE])))
  (testing "NaN is unordered, so every comparison against it is false"
    (is (every? nan-poisons? [0 1 -1 1.0 ##Inf ##-Inf ##NaN])))
  (testing "> is < with the arguments swapped, not the negation of <"
    (is (every? dual-of-lt? [[1 0] [0 1] [1 1] [1 ##NaN] [##NaN 1] [1 1.0]]))
    (is (= (reference-gt ##NaN 1) (< 1 ##NaN)))
    (is (not= (reference-gt 1 1) (not (< 1 1)))))
  (testing "arity 1 answers true without inspecting its argument"
    (is (true? (reference-gt "abc")))
    (is (true? (reference-gt nil))))
  (testing "the chain short-circuits, so a later non-number is never reached"
    (is (false? (:value (gt-outcome 0 1 nil))))
    (is (= "NullPointerException" (:throws (gt-outcome 2 1 nil)))))
  (testing "the zero-arity edge is recorded, not hidden"
    (is (= "ArityException" (:throws (gt-outcome))))))
