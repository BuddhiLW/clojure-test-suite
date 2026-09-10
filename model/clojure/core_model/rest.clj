(ns clojure.core-model.rest
  "Behaviour model for `clojure.core/rest`. Emits `model/golden/rest.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Seqable
  "The domain `rest` accepts without throwing."
  [:maybe [:seqable :any]])

(def Outcome
  "A total observation: the value returned, or the simple name of the class
  thrown."
  [:or [:map [:value :any]] [:map [:throw :string]]])

(defn reference-rest
  "`rest`, delegated to the host so the model characterizes rather than
  restates."
  [coll]
  (rest coll))

(m/=> reference-rest [:=> [:cat Seqable] [:sequential :any]])

(defn- observe
  "Run `thunk`, recording a throw as data instead of propagating it."
  [thunk]
  (try {:value (thunk)}
       (catch Throwable t {:throw (.getSimpleName (class t))})))

(defn observe-rest
  "`reference-rest` made total: every input yields an Outcome, so a throwing
  edge is recorded in the baseline rather than crashing the emit."
  [coll]
  (observe #(reference-rest coll)))

(m/=> observe-rest [:=> [:cat :any] Outcome])

(defn- encode
  "EDN-safe rendering of an Outcome."
  [o]
  (if (contains? o :throw)
    (str "throws " (:throw o))
    (pr-str (:value o))))

(defn- seq-never-nil?
  "`rest` yields a seq — never nil, even for an empty or nil input — or refuses
  the input with IllegalArgumentException."
  [o]
  (if (contains? o :throw)
    (= "IllegalArgumentException" (:throw o))
    (let [v (:value o)]
      (and (some? v) (seq? v)))))

(defn- tail-agrees-with-next?
  "`(seq (rest c))` = `(next c)` — the two differ only in how they spell
  exhaustion."
  [coll]
  (= (seq (rest coll)) (next coll)))

(defn- tail-is-one-shorter?
  "`(count (rest c))` = `(max 0 (dec (count c)))`."
  [coll]
  (= (count (rest coll)) (max 0 (dec (count coll)))))

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

(deftrifecta rest-behaviour
  clojure.core-model.rest/observe-rest
  {:golden-path "model/golden/rest.edn"
   :cases       {:nil            nil
                 :empty-vector   []
                 :empty-list     ()
                 :empty-string   ""
                 :empty-map      {}
                 :empty-set      #{}
                 :empty-lazy-seq (lazy-seq nil)
                 :singleton      [1]
                 :vector         [1 2 3]
                 :list           '(1 2 3)
                 :string         "abc"
                 :array-map      (array-map :a 1 :b 2)
                 :sorted-set     (sorted-set 3 1 2)
                 :lazy-seq       (range 3)
                 :cons           (cons 1 [2 3])
                 :nil-element    [nil 1]
                 :long           5
                 :keyword        :a
                 :boolean        true}
   :xf          encode
   :gen         gen-input
   :pred        seq-never-nil?
   :num-tests   500
   :mutations   [["next-instead"    (fn [c] (observe #(next c)))]
                 ["seq-instead"     (fn [c] (observe #(seq c)))]
                 ["drops-two"       (fn [c] (observe #(drop 2 c)))]
                 ["swallows-throw"  (fn [c] (observe #(try (rest c)
                                                           (catch Throwable _ nil))))]
                 ["butlast-instead" (fn [c] (observe #(butlast c)))]]})

(deftest rest-laws
  (testing "the tail is the seq of the next"
    (is (every? tail-agrees-with-next? (gen/sample gen-seqable 200))))
  (testing "the tail is exactly one element shorter"
    (is (every? tail-is-one-shorter? (gen/sample gen-seqable 200))))
  (testing "rest is never nil, where next is"
    (is (= () (reference-rest nil)))
    (is (= () (reference-rest [])))
    (is (= () (reference-rest [1])))
    (is (every? #(seq? (reference-rest %)) [nil [] () "" {} #{} [1 2 3]])))
  (testing "a non-seqable is refused, not coerced"
    (is (thrown? IllegalArgumentException (reference-rest 5)))
    (is (thrown? IllegalArgumentException (reference-rest :a)))))
