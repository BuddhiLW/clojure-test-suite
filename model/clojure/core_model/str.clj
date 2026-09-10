(ns clojure.core-model.str
  "Behaviour model for `clojure.core/str`. Emits `model/golden/str.edn`, the
  baseline each dialect is graded against."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Args
  "The domain `str` accepts: any number of arbitrary values."
  [:sequential :any])

(def Outcome
  "A recorded call result: the host's value, or the class it threw."
  [:or :string [:tuple [:= :throws] :string]])

(defn reference-str
  "`str`, delegated to the host so the model characterizes rather than restates."
  [& xs]
  (apply str xs))

(m/=> reference-str [:=> [:cat [:* :any]] :string])

(defn- outcome
  "Total result of `thunk`: its value, or [:throws \"<class>\"] when it throws."
  [thunk]
  (try (thunk)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(defn characterize-str
  "`reference-str` applied to the argument vector `args`, as a total outcome."
  [args]
  (outcome #(apply reference-str args)))

(m/=> characterize-str [:=> [:cat Args] Outcome])

(defn- total-into-strings?
  "`str` is total into strings: no argument list makes the host throw."
  [o]
  (string? o))

(defn- concatenative?
  "`(str a b ...)` is the concatenation of the one-argument `str` of each."
  [args]
  (= (apply reference-str args)
     (apply str (map #(reference-str %) args))))

(defn- nil-erasing?
  "nil contributes nothing: dropping nil arguments leaves the result unchanged."
  [args]
  (= (apply reference-str args)
     (apply reference-str (remove nil? args))))

(def ^:private gen-arg
  (gen/one-of [gen/string
               gen/small-integer
               gen/boolean
               gen/keyword
               gen/symbol
               gen/double
               gen/char
               (gen/return nil)
               (gen/vector gen/small-integer 0 3)]))

(def ^:private gen-args (gen/vector gen-arg 0 4))

(deftrifecta str-behaviour
  clojure.core-model.str/characterize-str
  {:golden-path "model/golden/str.edn"
   :cases       {:no-args           []
                 :nil               [nil]
                 :two-nils          [nil nil]
                 :empty-string      [""]
                 :string            ["a string"]
                 :strings           ["a" "string" "with" "no" "spaces"]
                 :zero              [0]
                 :negative-long     [-1]
                 :double            [1.0]
                 :negative-zero     [-0.0]
                 :float             [(float 1.0)]
                 :float-zero        [(float 0.0)]
                 :ratio             [1/2]
                 :negative-ratio    [-1/2]
                 :whole-ratio       [0/2]
                 :bigint            [1N]
                 :bigdec            [1.0M]
                 :bigdec-zero       [0.0M]
                 :bigdec-unscaled   [0M]
                 :bigdec-negative   [-1.0M]
                 :infinity          [##Inf]
                 :negative-infinity [##-Inf]
                 :nan               [##NaN]
                 :true              [true]
                 :false             [false]
                 :char              [\a]
                 :space-char        [\space]
                 :keyword           [:a-keyword]
                 :qualified-keyword [:a/b]
                 :symbol            ['a-sym]
                 :vector            [[:a :vector]]
                 :list              ['(:a :list)]
                 :empty-list        ['()]
                 :map               [{:a :map}]
                 :set               [#{:a-set}]
                 :nested-nil        [[1 2 nil]]
                 :nil-between       [1 nil 2]
                 :many              [1 2 3]}
   :xf          pr-str
   :gen         gen-args
   :pred        total-into-strings?
   :num-tests   500
   ;; Each mutant must be REJECTED by the golden cases; a survivor means the
   ;; model asserts nothing.
   :mutations   [["identity"        (fn [args] args)]
                 ["always-empty"    (fn [_] "")]
                 ["first-only"      (fn [args] (str (first args)))]
                 ["space-joined"    (fn [args] (string/join " " args))]
                 ["reversed"        (fn [args] (apply str (reverse args)))]
                 ["prints-readably" (fn [args] (apply str (map pr-str args)))]
                 ["trimmed"         (fn [args] (string/trim (apply str args)))]
                 ["upper-cased"     (fn [args] (string/upper-case (apply str args)))]]})

(defspec str-concatenation 500
  (prop/for-all [args gen-args]
    (concatenative? args)))

(defspec str-nil-erasure 500
  (prop/for-all [args gen-args]
    (nil-erasing? args)))

(deftest str-laws
  (testing "the empty argument list and nil both produce the empty string"
    (is (= "" (reference-str)))
    (is (= "" (reference-str nil)))
    (is (= "" (reference-str nil nil))))
  (testing "a string is its own str"
    (is (every? #(= % (reference-str %)) ["" "a" "a string" "0" "-1"])))
  (testing "a collection prints as its readable form"
    (is (= "[:a :vector]" (reference-str [:a :vector])))
    (is (= "(:a :list)" (reference-str '(:a :list))))
    (is (= "{:a :map}" (reference-str {:a :map}))))
  (testing "a char is its own character, not its printed literal"
    (is (= "a" (reference-str \a)))
    (is (= " " (reference-str \space))))
  (testing "doubles keep their decimal point and their signed zero"
    (is (= "1.0" (reference-str 1.0)))
    (is (= "-0.0" (reference-str -0.0)))
    (is (= "NaN" (reference-str ##NaN)))
    (is (= "Infinity" (reference-str ##Inf)))))
