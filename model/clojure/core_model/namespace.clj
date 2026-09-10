(ns clojure.core-model.namespace
  "Behaviour model for `clojure.core/namespace`. Emits `model/golden/namespace.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Args
  "An argument vector handed to the characterizer. Deliberately wider than the
  domain: the wrong-type calls are the point of the baseline."
  [:sequential :any])

(def Outcome
  "A recorded call result: the host's qualifier, nil, or the class it threw."
  [:or :nil :string [:tuple [:= :throws] :string]])

(defn reference-namespace
  "`namespace`, delegated to the host so the model characterizes rather than
  restates."
  [x]
  (namespace x))

(m/=> reference-namespace [:=> [:cat [:or :keyword :symbol]] [:maybe :string]])

(defn- outcome
  "Total result of `thunk`: its value, or [:throws \"<class>\"] when it throws."
  [thunk]
  (try (thunk)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(defn characterize-namespace
  "`reference-namespace` applied to the argument vector `args`, as a total outcome."
  [args]
  (outcome #(apply reference-namespace args)))

(m/=> characterize-namespace [:=> [:cat Args] Outcome])

(def ^:private refusal-classes
  #{"NullPointerException" "ClassCastException"})

(defn- known-outcome?
  "`namespace` returns a string, returns nil for an unqualified ident, or
  refuses in one of two known ways; it never throws anything else."
  [o]
  (or (nil? o)
      (string? o)
      (and (vector? o) (= :throws (first o)) (contains? refusal-classes (second o)))))

(def ^:private gen-name-part
  (gen/fmap (fn [[c s]] (str c s))
            (gen/tuple (gen/elements (map char (range 97 123)))
                       gen/string-alphanumeric)))

(def ^:private gen-args
  (gen/fmap vector
            (gen/one-of [gen/keyword gen/keyword-ns
                         gen/symbol gen/symbol-ns
                         gen/string gen/small-integer gen/boolean
                         (gen/return nil)])))

(deftrifecta namespace-behaviour
  clojure.core-model.namespace/characterize-namespace
  {:golden-path "model/golden/namespace.edn"
   :cases       {:qualified-keyword    [:abc/def]
                 :qualified-symbol     ['abc/def]
                 :core-symbol          ['clojure.core/+]
                 :dotted-namespace     [:a.b.c/d]
                 :special-characters   [:abc*+!-_'?<>=/abc]
                 :unqualified-keyword  [:abc]
                 :unqualified-symbol   ['abc]
                 :empty-name-keyword   [(keyword "")]
                 :empty-namespace-kw   [(keyword "" "x")]
                 :empty-namespace-sym  [(symbol "" "x")]
                 :slashed-string-kw    [(keyword "a/b")]
                 :string               ["abc"]
                 :nil                  [nil]
                 :long                 [1]
                 :boolean              [true]
                 :char                 [\a]
                 :vector               [[1]]}
   :xf          pr-str
   :gen         gen-args
   :pred        known-outcome?
   :num-tests   500
   ;; Each mutant must be REJECTED by the golden cases; a survivor means the
   ;; model asserts nothing.
   :mutations   [["identity"       (fn [args] args)]
                 ["always-nil"     (fn [_] nil)]
                 ["returns-name"   (fn [[x]] (outcome #(name x)))]
                 ["str-of-input"   (fn [[x]] (outcome #(str x)))]
                 ["empty-for-nil"  (fn [args]
                                     (let [o (characterize-namespace args)]
                                       (if (nil? o) "" o)))]
                 ["never-refuses"  (fn [args]
                                     (let [o (characterize-namespace args)]
                                       (if (vector? o) nil o)))]]})

(defspec namespace-recovers-the-qualifier 500
  (prop/for-all [ns-part gen-name-part
                 nm      gen-name-part]
    (and (= ns-part (reference-namespace (keyword ns-part nm)))
         (= ns-part (reference-namespace (symbol ns-part nm))))))

(defspec namespace-is-nil-exactly-when-unqualified 500
  (prop/for-all [nm gen-name-part]
    (and (nil? (reference-namespace (keyword nm)))
         (nil? (reference-namespace (symbol nm))))))

(defspec namespace-and-name-partition-a-qualified-ident 500
  (prop/for-all [ns-part gen-name-part
                 nm      gen-name-part]
    (let [k (keyword ns-part nm)]
      (= k (keyword (reference-namespace k) (name k))))))

(deftest namespace-laws
  (testing "a qualified ident yields the namespace half only"
    (is (= "abc" (reference-namespace :abc/def)))
    (is (= "abc" (reference-namespace 'abc/def)))
    (is (= "clojure.core" (reference-namespace 'clojure.core/+)))
    (is (= "a.b.c" (reference-namespace :a.b.c/d))))
  (testing "an unqualified ident yields nil, not the empty string"
    (is (nil? (reference-namespace :abc)))
    (is (nil? (reference-namespace 'abc))))
  (testing "an explicitly empty namespace is the empty string, not nil"
    (is (= "" (reference-namespace (keyword "" "x"))))
    (is (= "" (reference-namespace (symbol "" "x")))))
  (testing "a non-Named argument is refused, and a string is not Named"
    (is (thrown? NullPointerException (reference-namespace nil)))
    (is (thrown? ClassCastException (reference-namespace "abc")))
    (is (thrown? ClassCastException (reference-namespace 1)))))
