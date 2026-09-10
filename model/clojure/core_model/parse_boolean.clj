(ns clojure.core-model.parse-boolean
  "Behaviour model for `clojure.core/parse-boolean`. Emits `model/golden/parse-boolean.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def BooleanText
  "The domain `parse-boolean` is defined on. Every other argument throws."
  :string)

(def Outcome
  "A total observation of one call: the parsed boolean, nil, or a marker naming
  the class that was thrown."
  [:or :nil :boolean [:map [:throws :string]]])

(defn reference-parse-boolean
  "`parse-boolean`, delegated to the host so the model characterizes rather than restates."
  [s]
  (parse-boolean s))

(m/=> reference-parse-boolean [:=> [:cat BooleanText] [:maybe :boolean]])

(defn- observe
  "The value `thunk` yields, or a marker naming the class it threw."
  [thunk]
  (try (thunk) (catch Throwable t {:throws (.getName (class t))})))

(defn observed-parse-boolean
  "`reference-parse-boolean` over any argument at all: an off-domain argument is
  recorded as a throw marker instead of aborting the snapshot."
  [x]
  (observe #(reference-parse-boolean x)))

(m/=> observed-parse-boolean [:=> [:cat :any] Outcome])

(defn- throw-marker?
  "True when the outcome records a throw rather than a returned value."
  [o]
  (and (map? o) (contains? o :throws)))

(defn- closed-codomain?
  "The codomain is exactly `true`, `false` and `nil`, and each boolean is
  reproduced by parsing its own rendering. A throw marker is an accepted outcome."
  [o]
  (cond
    (nil? o)          true
    (throw-marker? o) true
    (boolean? o)      (= o (reference-parse-boolean (str o)))
    :else             false))

(def ^:private boolean-text-gen
  (gen/one-of [(gen/elements ["true" "false" "True" "TRUE" "False" "FALSE"
                              " true" "true " "tr ue" "t" "f" "" "0" "1"
                              "ttrue" "truee" "ffalse" "falsee"])
               gen/string-alphanumeric
               (gen/fmap str gen/boolean)
               gen/simple-type]))

(deftrifecta parse-boolean-behaviour
  clojure.core-model.parse-boolean/observed-parse-boolean
  {:golden-path "model/golden/parse-boolean.edn"
   :cases       {:true               "true"
                 :false              "false"
                 :capitalized-true   "True"
                 :upper-true         "TRUE"
                 :capitalized-false  "False"
                 :upper-false        "FALSE"
                 :prefixed-true      "ttrue"
                 :suffixed-true      "truee"
                 :prefixed-false     "ffalse"
                 :suffixed-false     "falsee"
                 :leading-space      " true"
                 :trailing-space     "true "
                 :internal-space     "tr ue"
                 :zero               "0"
                 :one                "1"
                 :empty              ""
                 :word               "foo"
                 :initial            "t"
                 :yes                "yes"
                 :non-string-boolean true
                 :non-string-long    0
                 :non-string-double  0.0
                 :non-string-char    \a
                 :non-string-keyword :key
                 :non-string-map     {}
                 :non-string-list    '()
                 :non-string-set     #{}
                 :non-string-vector  []
                 :nil                nil}
   :xf          pr-str
   :gen         boolean-text-gen
   :pred        closed-codomain?
   :num-tests   500
   :mutations   [["identity"         (fn [x] x)]
                 ["always-nil"       (fn [_] nil)]
                 ["always-true"      (fn [_] true)]
                 ["never-throws"     (fn [x] (try (parse-boolean x) (catch Throwable _ nil)))]
                 ["case-insensitive" (fn [x] (observe #(parse-boolean (str/lower-case x))))]
                 ["trims-whitespace" (fn [x] (observe #(parse-boolean (str/trim x))))]
                 ["truthy-fallback"  (fn [x] (observe #(boolean (parse-boolean x))))]]})

(deftest parse-boolean-laws
  (testing "exactly two strings parse; everything else in the domain is nil"
    (is (true? (reference-parse-boolean "true")))
    (is (false? (reference-parse-boolean "false")))
    (is (every? #(nil? (reference-parse-boolean %))
                ["True" "TRUE" "False" "FALSE" "t" "f" "0" "1" "" "yes" "foo"])))
  (testing "the match is case sensitive and untrimmed"
    (is (nil? (reference-parse-boolean " true")))
    (is (nil? (reference-parse-boolean "true ")))
    (is (nil? (reference-parse-boolean "tr ue"))))
  (testing "an unrecognized string is nil, never false"
    (is (not (false? (reference-parse-boolean "0"))))
    (is (nil? (reference-parse-boolean "0"))))
  (testing "a non-string argument throws, and a boolean argument is not a string"
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-boolean true)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-boolean nil)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-boolean 0)))))
