(ns clojure.core-model.contains-qmark
  "Behaviour model for `clojure.core/contains?`. Emits
  `model/golden/contains_qmark.edn`, the baseline each dialect is graded
  against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Searchable
  "Targets `contains?` answers for: maps, sets, vectors, strings and nil. Every
  other target, a non-indexed seq included, throws. A string additionally
  demands a numeric key."
  [:or :nil :map [:set :any] [:vector :any] :string])

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

(defn reference-contains?
  "`contains?`, delegated to the host so the model characterizes rather than
  restates. Asks about KEYS, never values."
  [coll k]
  (contains? coll k))

(m/=> reference-contains? [:=> [:cat Searchable :any] :boolean])

(defn contains-outcome
  "Total encoding of `reference-contains?`: the boolean it returns, or
  `{:throws \"<class>\"}` when the host throws. The golden facet snapshots this,
  so a throwing edge is recorded rather than crashing the emit."
  [coll k]
  (try (reference-contains? coll k)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(m/=> contains-outcome [:=> [:cat :any :any] Outcome])

(defn- strictly-boolean?
  "`contains?` answers true or false, never a truthy value and never nil."
  [r]
  (or (true? r) (false? r)))

(defn- asks-about-keys-not-values?
  "For an indexed target the question is about the index, so the count itself
  is never a key while 0 is whenever the vector is non-empty."
  [v]
  (and (reference-contains? v 0)
       (not (reference-contains? v (count v)))))

(defn- present-with-nil-value?
  "A key stored with a nil value is still present, which is exactly what `get`
  at arity 2 cannot tell you."
  [k]
  (reference-contains? {k nil} k))

(def ^:private any-key-gen
  "Keys spanning both the associative and the indexed reading."
  (gen/one-of [gen/keyword (gen/choose -2 6)]))

(def ^:private membership-gen
  "Arg-vectors over targets `contains?` answers for, so every result is a
  boolean rather than a recorded throw. A string is paired only with a numeric
  key, because a string refuses any other."
  (gen/one-of
   [(gen/tuple (gen/fmap #(zipmap % (repeat :v))
                         (gen/vector-distinct gen/keyword {:max-elements 4}))
               any-key-gen)
    (gen/tuple (gen/fmap set (gen/vector gen/small-integer 0 4)) any-key-gen)
    (gen/tuple (gen/fmap #(vec (repeat % :v)) (gen/choose 0 4)) any-key-gen)
    (gen/tuple gen/string-alphanumeric (gen/choose -2 6))
    (gen/tuple (gen/return nil) any-key-gen)]))

(deftrifecta contains-qmark-behaviour
  clojure.core-model.contains-qmark/contains-outcome
  {:golden-path "model/golden/contains_qmark.edn"
   :apply?      true
   :cases       {:map-key             [{:a 1} :a]
                 :map-absent-key      [{:a 1} :b]
                 :map-nil-value       [{:a nil} :a]
                 :map-value-not-key   [{:a 1} 1]
                 :sorted-map-key      [(sorted-map :a 1) :a]
                 :vector-first-index  [[1 2 3] 0]
                 :vector-last-index   [[1 2 3] 2]
                 :vector-value        [[1 2 3] 3]
                 :vector-past-end     [[1 2 3] 99]
                 :vector-negative     [[1 2 3] -1]
                 :vector-double-index [[1 2 3] 1.0]
                 :vector-keyword-key  [[1 2 3] :a]
                 :set-member          [#{1 2 3} 3]
                 :set-absent          [#{1 2 3} 9]
                 :sorted-set-member   [(sorted-set 1 2 3) 3]
                 :string-index        ["abc" 0]
                 :string-past-end     ["abc" 9]
                 :string-double-index ["abc" 1.0]
                 :string-character    ["abc" \a]
                 :string-keyword-key  ["abc" :a]
                 :list-index          ['(1 2 3) 0]
                 :lazy-seq-index      [(range 3) 0]
                 :nil-target          [nil :a]
                 :number-target       [42 :a]
                 :keyword-target      [:kw :a]}
   :xf          render
   :gen         membership-gen
   :pred        strictly-boolean?
   :num-tests   500
   :mutations   [["always-true"             (fn [_ _] true)]
                 ["always-false"            (fn [_ _] false)]
                 ["nil-value-reads-absent"  (fn [coll k] (some? (get coll k)))]
                 ["vector-searches-values"  (fn [coll k]
                                              (if (vector? coll)
                                                (boolean (some #{k} coll))
                                                (contains? coll k)))]
                 ["returns-the-value"       (fn [coll k] (get coll k))]
                 ["seqs-are-searchable"     (fn [coll k]
                                              (if (seq? coll)
                                                (and (int? k) (< -1 k (count coll)))
                                                (contains? coll k)))]
                 ["numbers-are-searchable"  (fn [coll k]
                                              (if (number? coll) false (contains? coll k)))]
                 ["negative-index-wraps"    (fn [coll k]
                                              (if (and (vector? coll) (int? k) (neg? k))
                                                (contains? coll (+ (count coll) k))
                                                (contains? coll k)))]
                 ["strings-behave-like-vectors" (fn [coll k]
                                                  (if (string? coll)
                                                    (and (int? k) (< -1 k (count coll)))
                                                    (contains? coll k)))]]})

(deftest contains-qmark-laws
  (testing "the question is about keys, so a value that is not an index is absent"
    (is (every? asks-about-keys-not-values? [[1 2 3] [:a] [0 1]]))
    (is (false? (reference-contains? [1 2 3] 3)))
    (is (true? (reference-contains? [1 2 3] 0))))
  (testing "a nil-valued key is present, unlike what arity-2 get can tell you"
    (is (every? present-with-nil-value? [:a "s" 1]))
    (is (nil? (get {:a nil} :a))))
  (testing "sets are asked about members, maps about keys"
    (is (true? (reference-contains? #{1 2 3} 3)))
    (is (false? (reference-contains? {:a 1} 1))))
  (testing "a non-indexed seq is refused rather than scanned"
    (is (= {:throws "IllegalArgumentException"} (contains-outcome '(1 2 3) 0)))
    (is (= {:throws "IllegalArgumentException"} (contains-outcome 42 :a))))
  (testing "a string and a vector disagree on a non-integer key"
    (is (false? (reference-contains? [1 2 3] 1.0)))
    (is (true? (reference-contains? "abc" 1.0)))
    (is (= {:throws "IllegalArgumentException"} (contains-outcome "abc" :a))))
  (testing "nil is searchable and always answers false"
    (is (false? (reference-contains? nil :a)))))
