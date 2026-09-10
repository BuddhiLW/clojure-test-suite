(ns clojure.core-model.assoc
  "Behaviour model for `clojure.core/assoc`. Emits `model/golden/assoc.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Associable
  "Targets `assoc` accepts: a map, a vector whose key is an index already in
  range or exactly at the end, and nil, which becomes a map."
  [:or :nil :map [:vector :any]])

(def Throw
  "The recorded shape of a host exception."
  [:map [:throws :string]])

(def Outcome
  "A total encoding of one call: the host's value, or a recorded throw."
  [:or Throw :any])

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

(defn reference-assoc
  "`assoc`, delegated to the host so the model characterizes rather than
  restates."
  [& args]
  (apply assoc args))

(m/=> reference-assoc [:=> [:cat [:* :any]] :any])

(defn assoc-outcome
  "Total encoding of `reference-assoc`: the value it returns, or
  `{:throws \"<class>\"}` when the host throws. The golden facet snapshots this,
  so a throwing edge is recorded rather than crashing the emit."
  [& args]
  (try (apply reference-assoc args)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(m/=> assoc-outcome [:=> [:cat [:* :any]] Outcome])

(defn- places-the-value?
  "An `assoc` result is a map that holds the value just written."
  [r]
  (boolean (and (map? r) (some #{:placed} (vals r)))))

(defn- overwrites-in-place?
  "Writing the same key twice leaves one entry carrying the last value."
  [m k v]
  (= (reference-assoc m k v)
     (reference-assoc (reference-assoc m k v) k v)))

(defn- never-drops-a-key?
  "`assoc` only adds or replaces: every key of the target survives."
  [m k v]
  (let [r (reference-assoc m k v)]
    (every? #(contains? r %) (keys m))))

(def ^:private assoc-gen
  "Arg-vectors writing the marker `:placed` into a map or into nil, so every
  result must be a map that holds the marker."
  (gen/let [target (gen/one-of
                    [(gen/fmap #(zipmap % (repeat 0))
                               (gen/vector-distinct gen/keyword {:max-elements 4}))
                     (gen/return nil)])
            k      gen/keyword]
    [target k :placed]))

(deftrifecta assoc-behaviour
  clojure.core-model.assoc/assoc-outcome
  {:golden-path "model/golden/assoc.edn"
   :apply?      true
   :cases       {:map-new-key         [{:a 1} :b 2]
                 :map-overwrite       [{:a 1} :a 9]
                 :map-nil-value       [{:a 1} :b nil]
                 :map-string-key      [{} "k" 1]
                 :map-nan-key         [{} ##NaN 1]
                 :map-multiple-pairs  [{} :a 1 :b 2]
                 :map-repeated-key    [{} :a 1 :a 2]
                 :map-odd-arg-count   [{} :a 1 :b]
                 :sorted-map          [(sorted-map :b 1) :a 2]
                 :nil-target          [nil :a 1]
                 :vector-in-range     [[1 2] 0 9]
                 :vector-at-end       [[1 2] 2 9]
                 :vector-past-end     [[1 2] 5 9]
                 :vector-negative     [[1 2] -1 9]
                 :vector-keyword-key  [[1 2] :a 9]
                 :vector-double-key   [[1 2] 1.0 9]
                 :list-target         ['(1 2) 0 9]
                 :set-target          [#{1 2} 0 9]
                 :string-target       ["ab" 0 \z]
                 :number-target       [42 :a 1]}
   :xf          render
   :gen         assoc-gen
   :pred        places-the-value?
   :num-tests   500
   :mutations   [["returns-the-target"    (fn [coll & _] coll)]
                 ["drops-the-value"       (fn [coll k & _] (assoc coll k nil))]
                 ["only-the-first-pair"   (fn [coll k v & _] (assoc coll k v))]
                 ["first-write-wins"      (fn [coll & kvs]
                                            (reduce (fn [m [k v]]
                                                      (if (contains? m k) m (assoc m k v)))
                                                    coll
                                                    (partition 2 kvs)))]
                 ["vector-grows-past-end" (fn [coll k v & more]
                                            (if (and (vector? coll) (int? k) (>= k (count coll)))
                                              (conj coll v)
                                              (apply assoc coll k v more)))]
                 ["vector-index-is-a-key" (fn [coll k v & more]
                                            (if (vector? coll)
                                              (apply assoc (zipmap (range (count coll)) coll) k v more)
                                              (apply assoc coll k v more)))]
                 ["nil-target-stays-nil"  (fn [coll k v & more]
                                            (if (nil? coll)
                                              nil
                                              (apply assoc coll k v more)))]
                 ["lists-are-associative" (fn [coll k v & more]
                                            (if (list? coll)
                                              (apply list (apply assoc (vec coll) k v more))
                                              (apply assoc coll k v more)))]]})

(deftest assoc-laws
  (testing "writing the same key twice is idempotent"
    (is (every? #(overwrites-in-place? % :k :v) [{} {:k 0} {:a 1} nil])))
  (testing "no existing key is ever dropped"
    (is (every? #(never-drops-a-key? % :new :v) [{} {:a 1} {:a 1 :b 2}])))
  (testing "a vector index must already exist or sit exactly at the end"
    (is (= [9 2] (reference-assoc [1 2] 0 9)))
    (is (= [1 2 9] (reference-assoc [1 2] 2 9)))
    (is (= {:throws "IndexOutOfBoundsException"} (assoc-outcome [1 2] 3 9)))
    (is (= {:throws "IndexOutOfBoundsException"} (assoc-outcome [1 2] -1 9))))
  (testing "a vector demands an integer index, a map takes any key"
    (is (= {:throws "IllegalArgumentException"} (assoc-outcome [1 2] :a 9)))
    (is (= {:throws "IllegalArgumentException"} (assoc-outcome [1 2] 1.0 9)))
    (is (= {1.0 9} (reference-assoc {} 1.0 9))))
  (testing "nil becomes a map, other non-associative targets are refused"
    (is (= {:a 1} (reference-assoc nil :a 1)))
    (is (= {:throws "ClassCastException"} (assoc-outcome '(1 2) 0 9)))
    (is (= {:throws "ClassCastException"} (assoc-outcome #{1 2} 0 9))))
  (testing "an odd number of key/value arguments is refused"
    (is (= {:throws "IllegalArgumentException"} (assoc-outcome {} :a 1 :b)))))
