(ns clojure.core-model.nth
  "Behaviour model for `clojure.core/nth`. Emits `model/golden/nth.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Indexable
  "The collection domain `nth` accepts without throwing."
  [:maybe [:seqable :any]])

(def Index
  "The index domain: `nth` truncates a floating-point index toward zero."
  [:or :int :double])

(def Outcome
  "A total observation: the value returned, or the simple name of the class
  thrown."
  [:or [:map [:value :any]] [:map [:throw :string]]])

(defn reference-nth
  "`nth`, delegated to the host so the model characterizes rather than
  restates."
  ([coll i] (nth coll i))
  ([coll i not-found] (nth coll i not-found)))

(m/=> reference-nth [:function
                     [:=> [:cat Indexable Index] :any]
                     [:=> [:cat Indexable Index :any] :any]])

(defn- observe
  "Run `thunk`, recording a throw as data instead of propagating it."
  [thunk]
  (try {:value (thunk)}
       (catch Throwable t {:throw (.getSimpleName (class t))})))

(defn observe-nth
  "`reference-nth` made total: every argument vector yields an Outcome, so a
  throwing edge is recorded in the baseline rather than crashing the emit."
  ([coll i] (observe #(reference-nth coll i)))
  ([coll i not-found] (observe #(reference-nth coll i not-found))))

(m/=> observe-nth [:function
                   [:=> [:cat :any :any] Outcome]
                   [:=> [:cat :any :any :any] Outcome]])

(defn- encode
  "EDN-safe rendering of an Outcome."
  [o]
  (if (contains? o :throw)
    (str "throws " (:throw o))
    (pr-str (:value o))))

(def ^:private index-failures
  #{"IndexOutOfBoundsException" "StringIndexOutOfBoundsException"
    "UnsupportedOperationException"})

(defn- value-or-index-failure?
  "`nth` fails only two ways: the index is out of range, or the collection is
  not indexable at all. It never reports a different class."
  [o]
  (if (contains? o :throw)
    (contains? index-failures (:throw o))
    (contains? o :value)))

(defn- not-found-never-throws?
  "For a sequential collection the 3-arity never throws: an index outside the
  range yields the not-found value instead."
  [[coll i]]
  (= (nth coll i ::absent)
     (if (and (nat-int? i) (< i (count coll)))
       (nth (vec coll) i)
       ::absent)))

(defn- agrees-with-get-on-vectors?
  "On a vector the 2-arity and `get` agree inside the range; outside it `nth`
  throws where `get` yields nil."
  [[v i]]
  (if (and (nat-int? i) (< i (count v)))
    (= (nth v i) (get v i))
    (nil? (get v i))))

(def ^:private gen-sequential
  (gen/one-of [(gen/return nil)
               (gen/vector gen/small-integer 0 6)
               (gen/fmap #(apply list %) (gen/vector gen/small-integer 0 6))
               gen/string-ascii
               (gen/fmap #(lazy-seq %) (gen/vector gen/small-integer 0 4))]))

(def ^:private gen-not-indexable
  (gen/one-of [(gen/set gen/small-integer)
               (gen/map gen/keyword gen/small-integer)
               gen/small-integer
               gen/keyword
               gen/boolean]))

(def ^:private gen-coll
  (gen/frequency [[4 gen-sequential] [1 gen-not-indexable]]))

(def ^:private gen-index (gen/choose -4 9))

(def ^:private gen-args
  (gen/one-of [(gen/tuple gen-coll gen-index)
               (gen/tuple gen-coll gen-index gen/keyword)]))

(def ^:private gen-sequential-args
  (gen/tuple (gen/one-of [(gen/vector gen/small-integer 0 6)
                          (gen/fmap #(apply list %) (gen/vector gen/small-integer 0 6))
                          (gen/fmap #(lazy-seq %) (gen/vector gen/small-integer 0 4))])
             gen-index))

(def ^:private gen-vector-args
  (gen/tuple (gen/vector gen/small-integer 0 6) gen-index))

(deftrifecta nth-behaviour
  clojure.core-model.nth/observe-nth
  {:golden-path "model/golden/nth.edn"
   :apply?      true
   :cases       {:vector-first       [[1 2 3] 0]
                 :vector-last        [[1 2 3] 2]
                 :vector-past-end    [[1 2 3] 5]
                 :vector-past-end-nf [[1 2 3] 5 :not-found]
                 :vector-negative    [[1 2 3] -1]
                 :vector-negative-nf [[1 2 3] -1 :not-found]
                 :empty-vector       [[] 0]
                 :empty-vector-nf    [[] 0 :not-found]
                 :list               ['(1 2 3) 1]
                 :list-past-end      ['(1 2 3) 5]
                 :list-past-end-nf   ['(1 2 3) 5 :not-found]
                 :list-negative      ['(1 2 3) -1]
                 :list-negative-nf   ['(1 2 3) -1 :not-found]
                 :empty-list         [() 0]
                 :empty-list-nf      [() 0 :not-found]
                 :string             ["abc" 1]
                 :string-past-end    ["abc" 9]
                 :string-past-end-nf ["abc" 9 :not-found]
                 :string-negative    ["abc" -1]
                 :nil-coll           [nil 0]
                 :nil-coll-negative  [nil -1]
                 :nil-coll-nf        [nil 0 :not-found]
                 :set                [#{1 2 3} 0]
                 :set-nf             [#{1 2 3} 0 :not-found]
                 :map                [(array-map :a 1) 0]
                 :map-nf             [(array-map :a 1) 0 :not-found]
                 :map-entry          [(first (array-map :a 1)) 0]
                 :lazy-seq           [(range 5) 2]
                 :lazy-seq-past-end  [(range 5) 9]
                 :infinite-lazy      [(range) 10]
                 :truncating-index   [[1 2 3] 1.7]
                 :nil-index          [[1 2 3] nil]
                 :overflowing-index  [[1 2 3] Long/MAX_VALUE]
                 :non-seqable        [5 0]
                 :non-seqable-nf     [5 0 :not-found]}
   :xf          encode
   :gen         gen-args
   :pred        value-or-index-failure?
   :num-tests   500
   :mutations   [["off-by-one"        (fn ([c i] (observe #(nth c (inc i))))
                                        ([c i nf] (observe #(nth c (inc i) nf))))]
                 ["ignores-not-found" (fn ([c i] (observe #(nth c i)))
                                        ([c i _] (observe #(nth c i))))]
                 ["always-first"      (fn ([c _] (observe #(nth c 0)))
                                        ([c _ nf] (observe #(nth c 0 nf))))]
                 ["get-instead"       (fn ([c i] (observe #(get c i)))
                                        ([c i nf] (observe #(get c i nf))))]
                 ["swallows-throw"    (fn ([c i] (observe #(try (nth c i)
                                                                (catch Throwable _ nil))))
                                        ([c i nf] (observe #(nth c i nf))))]]})

(deftest nth-laws
  (testing "the not-found arity never throws on a sequential collection"
    (is (every? not-found-never-throws? (gen/sample gen-sequential-args 200))))
  (testing "inside the range nth and get agree on a vector; outside, nth throws where get is nil"
    (is (every? agrees-with-get-on-vectors? (gen/sample gen-vector-args 200))))
  (testing "an out-of-range index throws on a vector but not with a not-found"
    (is (thrown? IndexOutOfBoundsException (reference-nth [1 2 3] 5)))
    (is (thrown? IndexOutOfBoundsException (reference-nth [1 2 3] -1)))
    (is (= :not-found (reference-nth [1 2 3] 5 :not-found)))
    (is (= :not-found (reference-nth [1 2 3] -1 :not-found))))
  (testing "nil is indexable and always yields nil, never an out-of-range failure"
    (is (nil? (reference-nth nil 0)))
    (is (nil? (reference-nth nil 99)))
    (is (nil? (reference-nth nil -1)))
    (is (= :not-found (reference-nth nil 0 :not-found))))
  (testing "a set or a map is not indexable, and the not-found arity does not rescue it"
    (is (thrown? UnsupportedOperationException (reference-nth #{1 2 3} 0)))
    (is (thrown? UnsupportedOperationException (reference-nth #{1 2 3} 0 :not-found)))
    (is (thrown? UnsupportedOperationException (reference-nth {:a 1} 0)))
    (is (thrown? UnsupportedOperationException (reference-nth {:a 1} 0 :not-found)))))
