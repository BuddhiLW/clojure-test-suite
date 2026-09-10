(ns clojure.core-model.dissoc
  "Behaviour model for `clojure.core/dissoc`. Emits `model/golden/dissoc.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Dissociable
  "Targets `dissoc` accepts: a map, or nil. A vector, a set and a string are
  all refused, even though `assoc` accepts a vector."
  [:or :nil :map])

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

(defn reference-dissoc
  "`dissoc`, delegated to the host so the model characterizes rather than
  restates."
  [& args]
  (apply dissoc args))

(m/=> reference-dissoc [:=> [:cat [:* :any]] :any])

(defn dissoc-outcome
  "Total encoding of `reference-dissoc`: the value it returns, or
  `{:throws \"<class>\"}` when the host throws. The golden facet snapshots this,
  so a throwing edge is recorded rather than crashing the emit."
  [& args]
  (try (apply reference-dissoc args)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(m/=> dissoc-outcome [:=> [:cat [:* :any]] Outcome])

(defn- keeps-untouched-keys?
  "`dissoc` removes only the keys it was handed: the sentinel entry survives."
  [r]
  (boolean (and (map? r) (contains? r :sentinel))))

(defn- removal-is-idempotent?
  "Removing the same key twice is the same as removing it once."
  [m k]
  (= (reference-dissoc m k)
     (reference-dissoc (reference-dissoc m k) k)))

(defn- never-grows?
  "`dissoc` never adds a key: the result's keys are a subset of the target's."
  [m k]
  (let [r (reference-dissoc m k)]
    (every? #(contains? m %) (keys r))))

(def ^:private dissoc-gen
  "Arg-vectors removing a key that is never the sentinel from a map that always
  carries the sentinel, so the sentinel must survive every result."
  (gen/let [others (gen/vector-distinct
                    (gen/such-that #(not= :sentinel %) gen/keyword)
                    {:max-elements 4})
            k      (gen/such-that #(not= :sentinel %) gen/keyword)]
    [(assoc (zipmap others (repeat 0)) :sentinel 1) k]))

(deftrifecta dissoc-behaviour
  clojure.core-model.dissoc/dissoc-outcome
  {:golden-path "model/golden/dissoc.edn"
   :apply?      true
   :cases       {:map-present-key      [{:a 1 :b 2} :a]
                 :map-absent-key       [{:a 1} :zz]
                 :map-nil-value        [{:a nil} :a]
                 :map-last-key         [{:a 1} :a]
                 :map-multiple-keys    [{:a 1 :b 2 :c 3} :a :c]
                 :map-repeated-key     [{:a 1 :b 2} :a :a]
                 :map-no-keys          [{:a 1}]
                 :map-string-key       [{"a" 1 "b" 2} "a"]
                 :map-int-key-double   [{1 :one} 1.0]
                 :sorted-map           [(sorted-map :a 1 :b 2) :a]
                 :nil-target           [nil :a]
                 :nil-target-no-keys   [nil]
                 :vector-target        [[1 2] 0]
                 :set-target           [#{1 2} 1]
                 :list-target          ['(1 2) 0]
                 :string-target        ["ab" 0]
                 :number-target        [42 :a]}
   :xf          render
   :gen         dissoc-gen
   :pred        keeps-untouched-keys?
   :num-tests   500
   :mutations   [["returns-the-target"     (fn [coll & _] coll)]
                 ["removes-everything"     (fn [& _] {})]
                 ["only-the-first-key"     (fn [coll k & _] (dissoc coll k))]
                 ["nil-target-becomes-map" (fn [coll & ks] (apply dissoc (or coll {}) ks))]
                 ["absent-key-is-refused"  (fn [coll & ks]
                                             (if (and (map? coll)
                                                      (some #(not (contains? coll %)) ks))
                                               (throw (ex-info "absent key" {}))
                                               (apply dissoc coll ks)))]
                 ["removes-by-value"       (fn [coll & ks]
                                             (if (map? coll)
                                               (into (empty coll)
                                                     (remove (fn [[_ v]] (some #{v} ks)) coll))
                                               (apply dissoc coll ks)))]
                 ["vectors-are-dissociable" (fn [coll & ks]
                                              (if (vector? coll)
                                                (vec (keep-indexed
                                                      (fn [i x] (when-not (some #{i} ks) x))
                                                      coll))
                                                (apply dissoc coll ks)))]
                 ["nil-valued-key-survives" (fn [coll & ks]
                                              (if (map? coll)
                                                (reduce (fn [m k]
                                                          (if (nil? (get m k)) m (dissoc m k)))
                                                        coll ks)
                                                (apply dissoc coll ks)))]]})

(deftest dissoc-laws
  (testing "removing a key twice is the same as removing it once"
    (is (every? #(removal-is-idempotent? % :a) [{} {:a 1} {:a 1 :b 2} nil])))
  (testing "the result's keys are always a subset of the target's"
    (is (every? #(never-grows? % :a) [{} {:a 1} {:a 1 :b 2}])))
  (testing "a key held with a nil value is still removed"
    (is (= {} (reference-dissoc {:a nil} :a)))
    (is (false? (contains? (reference-dissoc {:a nil} :a) :a))))
  (testing "an absent key is a no-op rather than an error"
    (is (= {:a 1} (reference-dissoc {:a 1} :zz))))
  (testing "nil stays nil, unlike assoc which turns it into a map"
    (is (nil? (reference-dissoc nil :a)))
    (is (= {:a 1} (assoc nil :a 1))))
  (testing "a vector is refused even though assoc accepts one"
    (is (= {:throws "ClassCastException"} (dissoc-outcome [1 2] 0)))
    (is (= {:throws "ClassCastException"} (dissoc-outcome #{1 2} 1)))
    (is (= [9 2] (assoc [1 2] 0 9)))))
