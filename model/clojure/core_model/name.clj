(ns clojure.core-model.name
  "Behaviour model for `clojure.core/name`. Emits `model/golden/name.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
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
  "A recorded call result: the host's name string, or the class it threw."
  [:or :string [:tuple [:= :throws] :string]])

(defn reference-name
  "`name`, delegated to the host so the model characterizes rather than restates."
  [x]
  (name x))

(m/=> reference-name [:=> [:cat [:or :string :keyword :symbol]] :string])

(defn- outcome
  "Total result of `thunk`: its value, or [:throws \"<class>\"] when it throws."
  [thunk]
  (try (thunk)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(defn characterize-name
  "`reference-name` applied to the argument vector `args`, as a total outcome."
  [args]
  (outcome #(apply reference-name args)))

(m/=> characterize-name [:=> [:cat Args] Outcome])

(def ^:private refusal-classes
  #{"NullPointerException" "ClassCastException"})

(defn- known-outcome?
  "`name` either returns a string or refuses in one of two known ways; it never
  returns nil and never throws anything else."
  [o]
  (or (string? o)
      (and (vector? o) (= :throws (first o)) (contains? refusal-classes (second o)))))

(def ^:private gen-name-part
  (gen/fmap (fn [[c s]] (str c s))
            (gen/tuple (gen/elements (map char (range 97 123)))
                       gen/string-alphanumeric)))

(def ^:private gen-args
  (gen/fmap vector
            (gen/one-of [gen/string
                         gen/keyword gen/keyword-ns
                         gen/symbol gen/symbol-ns
                         gen/small-integer gen/boolean gen/char
                         (gen/return nil)])))

(deftrifecta name-behaviour
  clojure.core-model.name/characterize-name
  {:golden-path "model/golden/name.edn"
   :cases       {:empty-string       [""]
                 :string             ["abc"]
                 :slashed-string     ["abc/def"]
                 :keyword            [:abc]
                 :qualified-keyword  [:abc/def]
                 :symbol             ['abc]
                 :qualified-symbol   ['abc/def]
                 :dotted-namespace   [:a.b.c/d]
                 :special-characters [:abc/abc*+!-_'?<>=]
                 :special-symbol     ['abc/abc*+!-_'?<>=]
                 :empty-namespace-kw [(keyword "" "x")]
                 :empty-namespace-sym [(symbol "" "x")]
                 :empty-name-keyword [(keyword "")]
                 :nil                [nil]
                 :long               [1]
                 :boolean            [true]
                 :char               [\a]
                 :vector             [[1]]}
   :xf          pr-str
   :gen         gen-args
   :pred        known-outcome?
   :num-tests   500
   ;; Each mutant must be REJECTED by the golden cases; a survivor means the
   ;; model asserts nothing.
   :mutations   [["identity"      (fn [args] args)]
                 ["always-abc"    (fn [_] "abc")]
                 ["str-of-input"  (fn [[x]] (outcome #(str x)))]
                 ["returns-ns"    (fn [[x]] (outcome #(namespace x)))]
                 ["upper-cased"   (fn [args]
                                    (let [o (characterize-name args)]
                                      (if (string? o) (string/upper-case o) o)))]
                 ["never-refuses" (fn [args]
                                    (let [o (characterize-name args)]
                                      (if (string? o) o "")))]]})

(defspec name-drops-the-qualifier 500
  (prop/for-all [ns-part gen-name-part
                 nm      gen-name-part]
    (and (= nm (reference-name (keyword ns-part nm)))
         (= nm (reference-name (symbol ns-part nm))))))

(defspec name-of-a-string-is-that-string 500
  (prop/for-all [s gen/string]
    (= s (reference-name s))))

(defspec name-inverts-keyword-and-symbol-construction 500
  (prop/for-all [nm gen-name-part]
    (and (= nm (reference-name (keyword nm)))
         (= nm (reference-name (symbol nm))))))

(deftest name-laws
  (testing "a string is its own name, empty string included"
    (is (= "" (reference-name "")))
    (is (= "abc" (reference-name "abc")))
    (is (= "abc/def" (reference-name "abc/def"))))
  (testing "a qualified ident yields the name half only"
    (is (= "def" (reference-name :abc/def)))
    (is (= "def" (reference-name 'abc/def)))
    (is (= "d" (reference-name :a.b.c/d))))
  (testing "an empty namespace still leaves the name reachable"
    (is (= "x" (reference-name (keyword "" "x"))))
    (is (= "x" (reference-name (symbol "" "x")))))
  (testing "a non-Named argument is refused, not coerced"
    (is (thrown? NullPointerException (reference-name nil)))
    (is (thrown? ClassCastException (reference-name 1)))
    (is (thrown? ClassCastException (reference-name \a)))))
