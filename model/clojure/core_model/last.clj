(ns clojure.core-model.last
  "Behaviour model for `clojure.core/last`. Emits `model/golden/last.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Seqable
  "The domain `last` accepts without throwing."
  [:maybe [:seqable :any]])

(def Outcome
  "A total observation: the value returned, or the simple name of the class
  thrown."
  [:or [:map [:value :any]] [:map [:throw :string]]])

(defn reference-last
  "`last`, delegated to the host so the model characterizes rather than
  restates."
  [coll]
  (last coll))

(m/=> reference-last [:=> [:cat Seqable] :any])

(defn- observe
  "Run `thunk`, recording a throw as data instead of propagating it."
  [thunk]
  (try {:value (thunk)}
       (catch Throwable t {:throw (.getSimpleName (class t))})))

(defn observe-last
  "`reference-last` made total: every input yields an Outcome, so a throwing
  edge is recorded in the baseline rather than crashing the emit."
  [coll]
  (observe #(reference-last coll)))

(m/=> observe-last [:=> [:cat :any] Outcome])

(defn- encode
  "EDN-safe rendering of an Outcome."
  [o]
  (if (contains? o :throw)
    (str "throws " (:throw o))
    (pr-str (:value o))))

(defn- value-or-illegal-argument?
  "`last` either yields a value or refuses the input with
  IllegalArgumentException; it raises nothing else."
  [o]
  (if (contains? o :throw)
    (= "IllegalArgumentException" (:throw o))
    (contains? o :value)))

(defn- last-is-final-element?
  "`(last c)` is the final element of `(vec c)`, and nil for an empty input."
  [coll]
  (= (last coll)
     (when (seq coll) (nth (vec coll) (dec (count coll))))))

(defn- last-walks-the-seq?
  "`(last c)` = `(last (seq c))` — `last` sees the collection only through its
  seq, so a set or map yields whatever its seq order ends on."
  [coll]
  (= (last coll) (last (seq coll))))

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

(deftrifecta last-behaviour
  clojure.core-model.last/observe-last
  {:golden-path "model/golden/last.edn"
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
                 :repeat         (repeat 3 :x)
                 :cons           (cons 1 [2 3])
                 :nil-element    [1 nil]
                 :long           5
                 :keyword        :a
                 :boolean        true}
   :xf          encode
   :gen         gen-input
   :pred        value-or-illegal-argument?
   :num-tests   500
   :mutations   [["first-instead"   (fn [c] (observe #(first c)))]
                 ["peek-instead"    (fn [c] (observe #(peek c)))]
                 ["second-to-last"  (fn [c] (observe #(last (butlast c))))]
                 ["always-nil"      (fn [_] (observe (constantly nil)))]
                 ["swallows-throw"  (fn [c] (observe #(try (last c)
                                                           (catch Throwable _ nil))))]]})

(deftest last-laws
  (testing "last is the final element of the vector view"
    (is (every? last-is-final-element? (gen/sample gen-seqable 200))))
  (testing "last sees the collection only through its seq"
    (is (every? last-walks-the-seq? (gen/sample gen-seqable 200))))
  (testing "an empty collection has a nil last, and so does a trailing nil"
    (is (every? #(nil? (reference-last %)) [nil [] () "" {} #{}]))
    (is (nil? (reference-last [1 nil])))
    (is (= 2 (count [1 nil]))))
  (testing "a non-seqable is refused, not coerced"
    (is (thrown? IllegalArgumentException (reference-last 5)))
    (is (thrown? IllegalArgumentException (reference-last :a)))))
