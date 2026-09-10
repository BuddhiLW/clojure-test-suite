(ns clojure.core-model.get
  "Behaviour model for `clojure.core/get`. Emits `model/golden/get.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Target
  "What `get` looks into. Total in this argument: maps, vectors, sets, strings
  and nil each carry a lookup rule, and every other value misses."
  :any)

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

(defn reference-get
  "`get`, delegated to the host so the model characterizes rather than restates."
  ([coll k]           (get coll k))
  ([coll k not-found] (get coll k not-found)))

(m/=> reference-get
      [:function
       [:=> [:cat Target :any] :any]
       [:=> [:cat Target :any :any] :any]])

(defn get-outcome
  "Total encoding of `reference-get`: the value it returns, or
  `{:throws \"<class>\"}` when the host throws. The golden facet snapshots this,
  so a throwing edge is recorded rather than crashing the emit."
  [& args]
  (try (apply reference-get args)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(m/=> get-outcome [:=> [:cat [:* :any]] Outcome])

(defn- absent-and-nil-valued-agree?
  "At arity 2 a missing key and a key stored with a nil value are
  indistinguishable: both yield nil."
  [k]
  (= (reference-get {} k) (reference-get {k nil} k)))

(defn- default-only-for-absent-key?
  "The arity-3 default is returned only when the key is ABSENT; a stored nil
  value wins over the default."
  [k not-found]
  (and (= not-found (reference-get {} k not-found))
       (nil? (reference-get {k nil} k not-found))))

(defn- confined-to-store-or-default?
  "A lookup yields only a stored value, the supplied default, or nil."
  [r]
  (contains? #{nil :found :fallback} r))

(def ^:private lookup-gen
  "Arg-vectors whose targets store only `:found` and whose defaults are only
  `:fallback`, confining every result to #{nil :found :fallback}."
  (gen/let [target        (gen/one-of
                           [(gen/fmap #(zipmap % (repeat :found))
                                      (gen/vector-distinct gen/keyword {:max-elements 4}))
                            (gen/fmap #(vec (repeat % :found)) (gen/choose 0 4))
                            (gen/return #{:found})
                            (gen/return nil)])
            k             (gen/one-of [gen/keyword (gen/choose -2 6)])
            with-default? gen/boolean]
    (if with-default? [target k :fallback] [target k])))

(deftrifecta get-behaviour
  clojure.core-model.get/get-outcome
  {:golden-path "model/golden/get.edn"
   :apply?      true
   :cases       {:map-hit                     [{:a 1} :a]
                 :map-miss                    [{:a 1} :b]
                 :map-nil-value               [{:a nil} :a]
                 :map-hit-default             [{:a 1} :a :none]
                 :map-miss-default            [{:a 1} :b :none]
                 :map-nil-value-default       [{:a nil} :a :none]
                 :map-int-key-double-lookup   [{1 :one} 1.0]
                 :map-string-key              [{"a" 1} "a"]
                 :vector-index                [[10 20 30] 1]
                 :vector-out-of-range         [[10 20 30] 5]
                 :vector-out-of-range-default [[10 20 30] 5 :none]
                 :vector-negative-index       [[10 20 30] -1]
                 :vector-double-index         [[10 20 30] 1.0]
                 :vector-keyword-key          [[10 20 30] :a]
                 :set-member                  [#{1 2 3} 2]
                 :set-absent                  [#{1 2 3} 9]
                 :string-index                ["abc" 1]
                 :string-out-of-range         ["abc" 9]
                 :list-index                  ['(1 2 3) 0]
                 :nil-target                  [nil :a]
                 :nil-target-default          [nil :a :none]
                 :number-target               [42 :a]}
   :xf          render
   :gen         lookup-gen
   :pred        confined-to-store-or-default?
   :num-tests   500
   :mutations   [["always-nil"            (fn [& _] nil)]
                 ["ignores-the-default"   (fn [coll k & _] (get coll k))]
                 ["returns-the-key"       (fn [_ k & _] k)]
                 ["returns-the-target"    (fn [coll & _] coll)]
                 ["default-on-nil-value"  (fn [coll k & more]
                                            (let [v (get coll k)]
                                              (if (and (nil? v) (seq more)) (first more) v)))]
                 ["set-lookup-is-boolean" (fn [coll k & more]
                                            (if (set? coll)
                                              (contains? coll k)
                                              (apply get coll k more)))]
                 ["string-is-opaque"      (fn [coll k & more]
                                            (if (string? coll)
                                              (first more)
                                              (apply get coll k more)))]
                 ["list-is-indexed"       (fn [coll k & more]
                                            (if (and (list? coll) (int? k))
                                              (nth coll k (first more))
                                              (apply get coll k more)))]]})

(deftest get-laws
  (testing "absent and nil-valued keys are indistinguishable at arity 2"
    (is (every? absent-and-nil-valued-agree? [:a "s" 1 [1 2]])))
  (testing "the default answers an absent key only, never a stored nil"
    (is (every? #(default-only-for-absent-key? % :none) [:a "s" 1])))
  (testing "lists are not associative, so an index misses"
    (is (nil? (reference-get '(1 2 3) 0)))
    (is (= 20 (reference-get [10 20 30] 1))))
  (testing "a double index never matches an integer key"
    (is (nil? (reference-get {1 :one} 1.0)))
    (is (nil? (reference-get [10 20 30] 1.0))))
  (testing "a non-collection target misses rather than throwing"
    (is (nil? (reference-get 42 :a)))
    (is (= :none (reference-get 42 :a :none)))))
