(ns clojure.core-model.conj
  "Behaviour model for `clojure.core/conj`. Emits `model/golden/conj.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Conjable
  "Targets `conj` grows: vectors grow at the back, lists and seqs at the front,
  sets by membership, maps by pair, and nil becomes a list."
  [:or :nil :map [:set :any] [:vector :any] [:sequential :any]])

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

(defn reference-conj
  "`conj`, delegated to the host so the model characterizes rather than
  restates."
  [& args]
  (apply conj args))

(m/=> reference-conj [:=> [:cat [:* :any]] :any])

(defn conj-outcome
  "Total encoding of `reference-conj`: the value it returns, or
  `{:throws \"<class>\"}` when the host throws. The golden facet snapshots this,
  so a throwing edge is recorded rather than crashing the emit."
  [& args]
  (try (apply reference-conj args)
       (catch Throwable t {:throws (.getSimpleName (class t))})))

(m/=> conj-outcome [:=> [:cat [:* :any]] Outcome])

(defn- holds-the-conjoined-item?
  "Whatever end the collection grows at, the item is in the result."
  [r]
  (boolean (and (coll? r) (some #{:placed} (seq r)))))

(defn- grows-at-the-back?
  "A vector puts the new item last."
  [v x]
  (= x (last (reference-conj v x))))

(defn- grows-at-the-front?
  "A list, a seq and nil all put the new item first."
  [l x]
  (= x (first (reference-conj l x))))

(defn- keeps-every-existing-item?
  "`conj` only adds: nothing already in the collection is lost."
  [coll x]
  (let [members (set (seq (reference-conj coll x)))]
    (every? members (seq coll))))

(def ^:private conj-gen
  "Arg-vectors conjoining the marker `:placed` onto a vector, list, set or nil,
  so every result must be a collection holding the marker."
  (gen/let [target (gen/one-of
                    [(gen/vector gen/small-integer 0 4)
                     (gen/fmap #(apply list %) (gen/vector gen/small-integer 0 4))
                     (gen/fmap set (gen/vector gen/small-integer 0 4))
                     (gen/return nil)])]
    [target :placed]))

(deftrifecta conj-behaviour
  clojure.core-model.conj/conj-outcome
  {:golden-path "model/golden/conj.edn"
   :apply?      true
   :cases       {:vector-back           [[1 2] 3]
                 :vector-many-items     [[1] 2 3 4]
                 :vector-nil-item       [[1] nil]
                 :empty-vector          [[] 1]
                 :list-front            ['(1 2) 0]
                 :list-many-items       ['(1) 2 3]
                 :empty-list-front      [() 1]
                 :seq-front             [(seq [1 2]) 0]
                 :lazy-seq-front        [(map inc [1 2]) 0]
                 :set-new-member        [#{1 2} 3]
                 :set-duplicate-member  [#{1 2} 2]
                 :sorted-set            [(sorted-set 3 1) 2]
                 :map-pair-vector       [{:a 1} [:b 2]]
                 :map-whole-map         [{:a 1} {:b 2 :c 3}]
                 :map-map-entry         [{:a 1} (first {:b 2})]
                 :map-overwrite         [{:a 1} [:a 9]]
                 :map-bare-number       [{:a 1} 5]
                 :map-triple-vector     [{:a 1} [:b 2 3]]
                 :nil-target            [nil 1]
                 :nil-target-many-items [nil 1 2]
                 :number-target         [1 2]
                 :string-target         ["ab" \c]
                 :no-extra-items        [[1]]
                 :no-arguments          []}
   :xf          render
   :gen         conj-gen
   :pred        holds-the-conjoined-item?
   :num-tests   500
   :mutations   [["returns-the-target"       (fn [coll & _] coll)]
                 ["drops-extra-items"        (fn [coll x & _] (conj coll x))]
                 ["vector-grows-at-the-front" (fn [coll & xs]
                                                (if (vector? coll)
                                                  (reduce (fn [v x] (vec (cons x v))) coll xs)
                                                  (apply conj coll xs)))]
                 ["list-grows-at-the-back"   (fn [coll & xs]
                                               (if (seq? coll)
                                                 (apply list (concat coll xs))
                                                 (apply conj coll xs)))]
                 ["nil-becomes-a-vector"     (fn [coll & xs] (apply conj (or coll []) xs))]
                 ["sets-keep-duplicates"     (fn [coll & xs]
                                               (if (set? coll)
                                                 (apply conj (vec coll) xs)
                                                 (apply conj coll xs)))]
                 ["map-takes-a-bare-item"    (fn [coll & xs]
                                               (if (map? coll)
                                                 (reduce (fn [m x]
                                                           (if (vector? x) (conj m x) (assoc m x x)))
                                                         coll xs)
                                                 (apply conj coll xs)))]
                 ["no-arguments-is-nil"      (fn [& args] (if (seq args) (apply conj args) nil))]
                 ["one-argument-is-nil"      (fn [& args]
                                               (if (= 1 (count args)) nil (apply conj args)))]]})

(deftest conj-laws
  (testing "a vector grows at the back and a list at the front"
    (is (every? #(grows-at-the-back? % :x) [[] [1] [1 2 3]]))
    (is (every? #(grows-at-the-front? % :x) [() '(1) '(1 2 3) nil (seq [1 2])]))
    (is (= [1 2 3] (reference-conj [1 2] 3)))
    (is (= '(0 1 2) (reference-conj '(1 2) 0))))
  (testing "nil grows into a list, so it grows at the front"
    (is (= '(1) (reference-conj nil 1)))
    (is (= '(2 1) (reference-conj nil 1 2))))
  (testing "nothing already present is lost"
    (is (every? #(keeps-every-existing-item? % :x)
                [[] [1 2] '(1 2) #{1 2} nil])))
  (testing "several items are conjoined left to right"
    (is (= [1 2 3 4] (reference-conj [1] 2 3 4)))
    (is (= '(3 2 1) (reference-conj '(1) 2 3))))
  (testing "a map takes a pair, never a bare item"
    (is (= {:a 1 :b 2} (reference-conj {:a 1} [:b 2])))
    (is (= {:throws "IllegalArgumentException"} (conj-outcome {:a 1} 5)))
    (is (= {:throws "IllegalArgumentException"} (conj-outcome {:a 1} [:b 2 3]))))
  (testing "a set absorbs a duplicate instead of growing"
    (is (= #{1 2} (reference-conj #{1 2} 2)))
    (is (= 2 (count (reference-conj #{1 2} 2)))))
  (testing "a non-collection target is refused"
    (is (= {:throws "ClassCastException"} (conj-outcome 1 2)))
    (is (= {:throws "ClassCastException"} (conj-outcome "ab" \c))))
  (testing "the zero- and one-argument arities are identities"
    (is (= [] (reference-conj)))
    (is (= [1] (reference-conj [1])))))
