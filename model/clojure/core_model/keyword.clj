(ns clojure.core-model.keyword
  "Behaviour model for `clojure.core/keyword`. Emits `model/golden/keyword.edn`,
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
  "A recorded call result: the host's keyword, nil, or the class it threw."
  [:or :nil :keyword [:tuple [:= :throws] :string]])

(defn reference-keyword
  "`keyword`, delegated to the host so the model characterizes rather than
  restates."
  ([x] (keyword x))
  ([ns-part nm] (keyword ns-part nm)))

(m/=> reference-keyword [:function
                         [:=> [:cat :any] [:maybe :keyword]]
                         [:=> [:cat [:maybe :string] :string] :keyword]])

(defn- outcome
  "Total result of `thunk`: its value, or [:throws \"<class>\"] when it throws."
  [thunk]
  (try (thunk)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(defn characterize-keyword
  "`reference-keyword` applied to the argument vector `args`, as a total outcome."
  [args]
  (outcome #(apply reference-keyword args)))

(m/=> characterize-keyword [:=> [:cat Args] Outcome])

(def ^:private refusal-classes
  #{"NullPointerException" "ClassCastException"})

(defn- known-outcome?
  "`keyword` returns a keyword, returns nil for an argument it cannot name, or
  refuses in one of two known ways; it never throws anything else."
  [o]
  (or (nil? o)
      (keyword? o)
      (and (vector? o) (= :throws (first o)) (contains? refusal-classes (second o)))))

(def ^:private gen-name-part
  (gen/fmap (fn [[c s]] (str c s))
            (gen/tuple (gen/elements (map char (range 97 123)))
                       gen/string-alphanumeric)))

(def ^:private gen-args
  (gen/one-of
   [(gen/fmap vector
              (gen/one-of [gen/string
                           gen/keyword gen/keyword-ns
                           gen/symbol gen/symbol-ns
                           gen/small-integer gen/boolean gen/char
                           (gen/return nil)]))
    (gen/tuple (gen/one-of [gen-name-part (gen/return nil)]) gen-name-part)]))

(deftrifecta keyword-behaviour
  clojure.core-model.keyword/characterize-keyword
  {:golden-path "model/golden/keyword.edn"
   :cases       {:from-string           ["abc"]
                 :from-symbol           ['abc]
                 :from-keyword          [:abc]
                 :from-qualified-symbol ['abc/def]
                 :from-qualified-keyword [:abc/def]
                 :from-slashed-string   ["abc/def"]
                 :from-empty-string     [""]
                 :from-spaced-string    ["a b"]
                 :from-special-chars    ["abc*+!-_'?<>="]
                 :from-nil              [nil]
                 :from-long             [1]
                 :from-boolean          [true]
                 :from-char             [\a]
                 :from-vector           [[1]]
                 :two-strings           ["abc" "def"]
                 :dotted-namespace      ["abc.def" "abc"]
                 :special-namespace     ["abc*+!-_'?<>=" "abc*+!-_'?<>="]
                 :nil-namespace         [nil "abc"]
                 :empty-namespace       ["" "abc"]
                 :nil-name              ["abc" nil]
                 :symbol-namespace      ['abc "abc"]
                 :symbol-name           ["abc" 'abc]
                 :keyword-namespace     [:abc "abc"]
                 :keyword-name          ["abc" :abc]}
   :xf          pr-str
   :gen         gen-args
   :pred        known-outcome?
   :num-tests   500
   ;; Each mutant must be REJECTED by the golden cases; a survivor means the
   ;; model asserts nothing.
   :mutations   [["identity"           (fn [args] args)]
                 ["always-nil"         (fn [_] nil)]
                 ["always-abc"         (fn [_] :abc)]
                 ["drops-namespace"    (fn [args]
                                         (outcome
                                          #(keyword (name (apply reference-keyword args)))))]
                 ["str-then-keyword"   (fn [args] (outcome #(keyword (str (first args)))))]
                 ["ignores-namespace"  (fn [args] (outcome #(reference-keyword (last args))))]
                 ["never-refuses"      (fn [args]
                                         (let [o (characterize-keyword args)]
                                           (if (vector? o) nil o)))]]})

(defspec keyword-round-trips-through-name-and-namespace 500
  (prop/for-all [ns-part gen-name-part
                 nm      gen-name-part]
    (let [k (reference-keyword ns-part nm)]
      (and (= ns-part (namespace k))
           (= nm (name k))
           (= k (reference-keyword (namespace k) (name k)))
           (= k (reference-keyword k))))))

(defspec keyword-is-idempotent-on-simple-names 500
  (prop/for-all [nm gen-name-part]
    (let [k (reference-keyword nm)]
      (and (= k (reference-keyword k))
           (= nm (name k))
           (nil? (namespace k))))))

(defspec keyword-agrees-across-string-and-symbol-input 500
  (prop/for-all [nm gen-name-part]
    (= (reference-keyword nm) (reference-keyword (symbol nm)))))

(deftest keyword-laws
  (testing "a string, a symbol and a keyword all name the same keyword"
    (is (= :abc (reference-keyword "abc")))
    (is (= :abc (reference-keyword 'abc)))
    (is (= :abc (reference-keyword :abc))))
  (testing "a slash in the one-argument string splits into namespace and name"
    (is (= :abc/def (reference-keyword "abc/def")))
    (is (= "abc" (namespace (reference-keyword "abc/def"))))
    (is (= "def" (name (reference-keyword "abc/def")))))
  (testing "nil in, nil out; a non-nameable argument is nil rather than a throw"
    (is (nil? (reference-keyword nil)))
    (is (nil? (reference-keyword 1)))
    (is (nil? (reference-keyword true))))
  (testing "a nil namespace is ignored, an empty namespace is kept"
    (is (= :abc (reference-keyword nil "abc")))
    (is (nil? (namespace (reference-keyword nil "abc"))))
    (is (= "" (namespace (reference-keyword "" "abc")))))
  (testing "the two-argument arity takes strings only"
    (is (thrown? NullPointerException (reference-keyword "abc" nil)))
    (is (thrown? ClassCastException (reference-keyword 'abc "abc")))
    (is (thrown? ClassCastException (reference-keyword "abc" 'abc)))
    (is (thrown? ClassCastException (reference-keyword :abc "abc")))))
