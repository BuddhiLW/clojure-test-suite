(ns clojure.core-model.first
  "Behaviour model for `clojure.core/first`. Emits `model/golden/first.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Seqable
  "The domain `first` accepts without throwing."
  [:maybe [:seqable :any]])

(def Outcome
  "A total observation: the value returned, or the simple name of the class
  thrown."
  [:or [:map [:value :any]] [:map [:throw :string]]])

(defn reference-first
  "`first`, delegated to the host so the model characterizes rather than
  restates."
  [coll]
  (first coll))

(m/=> reference-first [:=> [:cat Seqable] :any])

(defn- observe
  "Run `thunk`, recording a throw as data instead of propagating it."
  [thunk]
  (try {:value (thunk)}
       (catch Throwable t {:throw (.getSimpleName (class t))})))

(defn observe-first
  "`reference-first` made total: every input yields an Outcome, so a throwing
  edge is recorded in the baseline rather than crashing the emit."
  [coll]
  (observe #(reference-first coll)))

(m/=> observe-first [:=> [:cat :any] Outcome])

(defn- encode
  "EDN-safe rendering of an Outcome."
  [o]
  (if (contains? o :throw)
    (str "throws " (:throw o))
    (pr-str (:value o))))

(defn- value-or-illegal-argument?
  "`first` either yields a value or refuses the input with
  IllegalArgumentException; it raises nothing else."
  [o]
  (if (contains? o :throw)
    (= "IllegalArgumentException" (:throw o))
    (contains? o :value)))

(defn- head-agrees-with-seq?
  "`(first c)` = `(first (seq c))`, and the head is nil exactly when `(seq c)`
  is nil or the first element is itself nil."
  [coll]
  (let [s (seq coll)]
    (and (= (first coll) (first s))
         (or (some? s) (nil? (first coll))))))

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

(deftrifecta first-behaviour
  clojure.core-model.first/observe-first
  {:golden-path "model/golden/first.edn"
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
                 :array-map      (array-map :a 1 :b 2)
                 :sorted-set     (sorted-set 3 1 2)
                 :lazy-seq       (range 3)
                 :infinite-lazy  (range)
                 :cons           (cons 1 [2 3])
                 :nil-element    [nil 1]
                 :long           5
                 :keyword        :a
                 :boolean        true}
   :xf          encode
   :gen         gen-input
   :pred        value-or-illegal-argument?
   :num-tests   500
   :mutations   [["always-nil"       (fn [_] (observe (constantly nil)))]
                 ["second-instead"   (fn [c] (observe #(second c)))]
                 ["swallows-throw"   (fn [c] (observe #(try (first c)
                                                            (catch Throwable _ nil))))]
                 ["empty-not-nil"    (fn [c] (observe #(if-let [s (seq c)] (first s) [])))]]})

(deftest first-laws
  (testing "the head agrees with the head of the seq"
    (is (every? head-agrees-with-seq? (gen/sample gen-seqable 200))))
  (testing "nil and every empty collection have a nil head"
    (is (every? #(nil? (reference-first %))
                [nil [] () "" {} #{} (lazy-seq nil)])))
  (testing "a nil element is a head, not an absence"
    (is (nil? (reference-first [nil 1])))
    (is (= 1 (reference-first (rest [nil 1])))))
  (testing "a non-seqable is refused, not coerced"
    (is (thrown? IllegalArgumentException (reference-first 5)))
    (is (thrown? IllegalArgumentException (reference-first :a)))))
