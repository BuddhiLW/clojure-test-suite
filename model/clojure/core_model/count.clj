(ns clojure.core-model.count
  "Behaviour model for `clojure.core/count`. Emits `model/golden/count.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Countable
  "The domain `count` accepts without throwing."
  [:maybe [:seqable :any]])

(def Outcome
  "A total observation: the value returned, or the simple name of the class
  thrown."
  [:or [:map [:value :any]] [:map [:throw :string]]])

(defn reference-count
  "`count`, delegated to the host so the model characterizes rather than
  restates."
  [coll]
  (count coll))

(m/=> reference-count [:=> [:cat Countable] [:int {:min 0}]])

(defn- observe
  "Run `thunk`, recording a throw as data instead of propagating it."
  [thunk]
  (try {:value (thunk)}
       (catch Throwable t {:throw (.getSimpleName (class t))})))

(defn observe-count
  "`reference-count` made total: every input yields an Outcome, so a throwing
  edge is recorded in the baseline rather than crashing the emit."
  [coll]
  (observe #(reference-count coll)))

(m/=> observe-count [:=> [:cat :any] Outcome])

(defn- encode
  "EDN-safe rendering of an Outcome."
  [o]
  (if (contains? o :throw)
    (str "throws " (:throw o))
    (pr-str (:value o))))

(defn- natural-or-unsupported?
  "`count` yields a non-negative integer, or refuses the input with
  UnsupportedOperationException — never a negative, never any other class."
  [o]
  (if (contains? o :throw)
    (= "UnsupportedOperationException" (:throw o))
    (let [v (:value o)]
      (and (integer? v) (not (neg? v))))))

(defn- counts-the-seq?
  "`(count c)` = `(count (seq c))` — counting is defined through the seq, so nil
  and the empty collections agree at zero."
  [coll]
  (= (count coll) (count (seq coll))))

(defn- counts-by-walking?
  "`(count c)` = the number of steps a reduce takes over `c`."
  [coll]
  (= (count coll) (reduce (fn [n _] (inc n)) 0 coll)))

(def ^:private gen-seqable
  (gen/one-of [(gen/return nil)
               (gen/vector gen/small-integer 0 6)
               (gen/fmap #(apply list %) (gen/vector gen/small-integer 0 6))
               (gen/set gen/small-integer)
               (gen/map gen/keyword gen/small-integer)
               gen/string-ascii
               (gen/fmap #(lazy-seq %) (gen/vector gen/small-integer 0 4))]))

(def ^:private gen-non-seqable
  (gen/one-of [gen/small-integer gen/keyword gen/boolean gen/double]))

(def ^:private gen-input
  (gen/frequency [[4 gen-seqable] [1 gen-non-seqable]]))

(deftrifecta count-behaviour
  clojure.core-model.count/observe-count
  {:golden-path "model/golden/count.edn"
   :cases       {:nil            nil
                 :empty-vector   []
                 :empty-list     ()
                 :empty-string   ""
                 :empty-map      {}
                 :empty-set      #{}
                 :empty-lazy-seq (lazy-seq nil)
                 :vector         [1 2 3]
                 :list           '(1 2 3)
                 :string         "abc"
                 :map            {:a 1 :b 2}
                 :set            #{1 2}
                 :duplicates     [1 1 2]
                 :nil-element    [nil nil]
                 :lazy-seq       (range 5)
                 :repeat         (repeat 3 :x)
                 :cons           (cons 1 [2 3])
                 :map-entry      (first (array-map :a 1))
                 :long           5
                 :keyword        :a
                 :character      \a
                 :boolean        true}
   :xf          encode
   :gen         gen-input
   :pred        natural-or-unsupported?
   :num-tests   500
   :mutations   [["off-by-one"      (fn [c] (observe #(inc (count c))))]
                 ["always-zero"     (fn [_] (observe (constantly 0)))]
                 ["counts-distinct" (fn [c] (observe #(count (distinct c))))]
                 ["zero-on-throw"   (fn [c] (observe #(try (count c)
                                                           (catch Throwable _ 0))))]
                 ["bounded-at-one"  (fn [c] (observe #(min 1 (count c))))]]})

(deftest count-laws
  (testing "counting goes through the seq"
    (is (every? counts-the-seq? (gen/sample gen-seqable 200))))
  (testing "counting agrees with walking"
    (is (every? counts-by-walking? (gen/sample gen-seqable 200))))
  (testing "nil and every empty collection count zero"
    (is (every? #(zero? (reference-count %)) [nil [] () "" {} #{} (lazy-seq nil)])))
  (testing "a nil element still counts"
    (is (= 2 (reference-count [nil nil]))))
  (testing "a scalar is refused with UnsupportedOperationException, not IllegalArgument"
    (is (thrown? UnsupportedOperationException (reference-count 5)))
    (is (thrown? UnsupportedOperationException (reference-count :a)))
    (is (thrown? UnsupportedOperationException (reference-count \a)))))
