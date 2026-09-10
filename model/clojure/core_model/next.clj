(ns clojure.core-model.next
  "Behaviour model for `clojure.core/next`. Emits `model/golden/next.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Seqable
  "The domain `next` accepts without throwing."
  [:maybe [:seqable :any]])

(def Outcome
  "A total observation: the value returned, or the simple name of the class
  thrown."
  [:or [:map [:value :any]] [:map [:throw :string]]])

(defn reference-next
  "`next`, delegated to the host so the model characterizes rather than
  restates."
  [coll]
  (next coll))

(m/=> reference-next [:=> [:cat Seqable] [:maybe [:sequential :any]]])

(defn- observe
  "Run `thunk`, recording a throw as data instead of propagating it."
  [thunk]
  (try {:value (thunk)}
       (catch Throwable t {:throw (.getSimpleName (class t))})))

(defn observe-next
  "`reference-next` made total: every input yields an Outcome, so a throwing
  edge is recorded in the baseline rather than crashing the emit."
  [coll]
  (observe #(reference-next coll)))

(m/=> observe-next [:=> [:cat :any] Outcome])

(defn- encode
  "EDN-safe rendering of an Outcome."
  [o]
  (if (contains? o :throw)
    (str "throws " (:throw o))
    (pr-str (:value o))))

(defn- nil-or-non-empty-seq?
  "`next` yields either nil or a NON-EMPTY seq — it never yields an empty one —
  or refuses the input with IllegalArgumentException."
  [o]
  (if (contains? o :throw)
    (= "IllegalArgumentException" (:throw o))
    (let [v (:value o)]
      (or (nil? v)
          (and (seq? v) (boolean (seq v)))))))

(defn- next-is-seq-of-rest?
  "`(next c)` = `(seq (rest c))` — the definitional law that separates `next`
  from `rest`."
  [coll]
  (= (next coll) (seq (rest coll))))

(defn- nil-exactly-when-short?
  "`next` is nil exactly when the collection holds fewer than two elements."
  [coll]
  (= (nil? (next coll)) (< (count coll) 2)))

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

(deftrifecta next-behaviour
  clojure.core-model.next/observe-next
  {:golden-path "model/golden/next.edn"
   :cases       {:nil            nil
                 :empty-vector   []
                 :empty-list     ()
                 :empty-string   ""
                 :empty-map      {}
                 :empty-set      #{}
                 :empty-lazy-seq (lazy-seq nil)
                 :singleton      [1]
                 :single-char    "a"
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
   :pred        nil-or-non-empty-seq?
   :num-tests   500
   :mutations   [["rest-instead"    (fn [c] (observe #(rest c)))]
                 ["seq-instead"     (fn [c] (observe #(seq c)))]
                 ["always-nil"      (fn [_] (observe (constantly nil)))]
                 ["drops-two"       (fn [c] (observe #(next (next c))))]
                 ["swallows-throw"  (fn [c] (observe #(try (next c)
                                                           (catch Throwable _ nil))))]]})

(deftest next-laws
  (testing "next is the seq of the rest"
    (is (every? next-is-seq-of-rest? (gen/sample gen-seqable 200))))
  (testing "next is nil exactly below two elements"
    (is (every? nil-exactly-when-short? (gen/sample gen-seqable 200))))
  (testing "next collapses exhaustion to nil, where rest keeps ()"
    (is (nil? (reference-next nil)))
    (is (nil? (reference-next [])))
    (is (nil? (reference-next [1])))
    (is (= () (rest [1]))))
  (testing "a non-seqable is refused, not coerced"
    (is (thrown? IllegalArgumentException (reference-next 5)))
    (is (thrown? IllegalArgumentException (reference-next :a)))))
