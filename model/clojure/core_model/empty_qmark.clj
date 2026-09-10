(ns clojure.core-model.empty-qmark
  "Behaviour model for `clojure.core/empty?`. Emits
  `model/golden/empty_qmark.edn`, the baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Seqable
  "What `empty?` accepts: anything `seq` accepts. Scalars are refused."
  [:or :nil :map [:set :any] [:vector :any] [:sequential :any] :string])

(def Throw
  "The recorded shape of a host exception."
  [:map [:throws :string]])

(def Outcome
  "A total encoding of one call: the host's boolean, or a recorded throw."
  [:or Throw :boolean])

(defn- render
  "Canonical text for `v`. Map entries and set elements are sorted, so the
  encoding never depends on hash order."
  [v]
  (cond
    (map? v)    (str "{" (str/join ", " (sort (map (fn [[k x]] (str (render k) " " (render x))) v))) "}")
    (set? v)    (str "#{" (str/join " " (sort (map render v))) "}")
    (vector? v) (str "[" (str/join " " (map render v)) "]")
    (seq? v)    (str "(" (str/join " " (map render v)) ")")
    :else       (pr-str v)))

(defn reference-empty?
  "`empty?`, delegated to the host so the model characterizes rather than
  restates."
  [coll]
  (empty? coll))

(m/=> reference-empty? [:=> [:cat Seqable] :boolean])

(defn empty-outcome
  "Total encoding of `reference-empty?`: the boolean it returns, or
  `{:throws \"<class>\"}` when the host throws. The golden facet snapshots this,
  so a throwing edge is recorded rather than crashing the emit."
  [coll]
  (try (reference-empty? coll)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(m/=> empty-outcome [:=> [:cat :any] Outcome])

(defn- strictly-boolean?
  "`empty?` answers true or false, never a truthy value and never nil."
  [r]
  (or (true? r) (false? r)))

(defn- agrees-with-seq?
  "`empty?` is exactly `(not (seq coll))`, so it is driven by the seq and not
  by a count."
  [coll]
  (= (reference-empty? coll) (not (seq coll))))

(defn- complement-of-non-empty?
  "A collection is empty or it is not: adding an item always flips a true."
  [coll]
  (false? (reference-empty? (conj (vec coll) :x))))

(def ^:private collection-gen
  "Collections `empty?` accepts, so every result is a boolean rather than a
  recorded throw."
  (gen/one-of [(gen/vector gen/small-integer 0 4)
               (gen/fmap #(apply list %) (gen/vector gen/small-integer 0 4))
               (gen/fmap set (gen/vector gen/small-integer 0 4))
               (gen/fmap #(zipmap % (repeat :v))
                         (gen/vector-distinct gen/keyword {:max-elements 3}))
               gen/string-alphanumeric
               (gen/return nil)]))

(deftrifecta empty-qmark-behaviour
  clojure.core-model.empty-qmark/empty-outcome
  {:golden-path "model/golden/empty_qmark.edn"
   :cases       {:nil               nil
                 :empty-vector      []
                 :vector            [1]
                 :vector-of-nil     [nil]
                 :empty-list        ()
                 :list              '(1)
                 :empty-map         {}
                 :map               {:a 1}
                 :sorted-map        (sorted-map)
                 :empty-set         #{}
                 :set               #{1}
                 :empty-string      ""
                 :string            "a"
                 :blank-string      " "
                 :empty-lazy-seq    (filter even? [1 3])
                 :lazy-seq          (range 3)
                 :empty-range       (range 0)
                 :number            42
                 :zero              0
                 :keyword           :a
                 :boolean           true
                 :character         \a}
   :xf          render
   :gen         collection-gen
   :pred        strictly-boolean?
   :num-tests   500
   :mutations   [["always-true"            (fn [_] true)]
                 ["always-false"           (fn [_] false)]
                 ["nil-is-not-empty"       (fn [c] (if (nil? c) false (empty? c)))]
                 ["strings-are-opaque"     (fn [c] (if (string? c) false (empty? c)))]
                 ["nil-elements-count-as-empty" (fn [c]
                                                  (if (coll? c)
                                                    (every? nil? (seq c))
                                                    (empty? c)))]
                 ["seqs-are-never-empty"   (fn [c] (if (seq? c) false (empty? c)))]
                 ["returns-the-seq"        (fn [c] (seq c))]
                 ["scalars-are-empty"      (fn [c] (if (coll? c) (empty? c) (nil? (seq [c]))))]]})

(deftest empty-qmark-laws
  (testing "empty? is the negation of seq, for every accepted collection"
    (is (every? agrees-with-seq? [nil [] [1] () '(1) {} {:a 1} #{} #{1} "" "a"
                                  (range 0) (range 3)])))
  (testing "adding an item always makes a collection non-empty"
    (is (every? complement-of-non-empty? [nil [] [1] () #{} {}])))
  (testing "a collection holding only nil is not empty"
    (is (false? (reference-empty? [nil]))))
  (testing "an unrealized lazy seq that yields nothing is empty"
    (is (true? (reference-empty? (filter even? [1 3])))))
  (testing "a scalar is refused rather than treated as empty"
    (is (= {:throws "IllegalArgumentException"} (empty-outcome 42)))
    (is (= {:throws "IllegalArgumentException"} (empty-outcome :a)))
    (is (= {:throws "IllegalArgumentException"} (empty-outcome true)))))
