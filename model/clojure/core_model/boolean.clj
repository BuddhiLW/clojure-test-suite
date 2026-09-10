(ns clojure.core-model.boolean
  "Behaviour model for `clojure.core/boolean`. Emits `model/golden/boolean.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Coercible
  "The domain `boolean` accepts: everything. It is the one total coercion in the
  family — it never refuses and never loses information a caller could recover,
  because it keeps only the one bit it was asked for."
  :any)

(defn reference-boolean
  "`boolean`, delegated to the host so the model characterizes rather than
  restates."
  [x]
  (boolean x))

(m/=> reference-boolean [:=> [:cat Coercible] :boolean])

(defn- falsey?
  "The two values `boolean` maps to false. Every other value, zero and the empty
  collections included, maps to true."
  [x]
  (or (nil? x) (false? x)))

(defn- encode
  "Total encoding of one outcome: the printed value with its host type, or
  `nil`."
  [r]
  (if (nil? r)
    "nil"
    (str (pr-str r) " " (.getSimpleName (class r)))))

(def ^:private inputs
  (gen/one-of [gen/any-printable
               (gen/return nil)
               (gen/return false)
               gen/boolean
               gen/double
               gen/large-integer
               (gen/fmap #(/ % 7) gen/large-integer)]))

(deftrifecta boolean-behaviour
  clojure.core-model.boolean/reference-boolean
  {:golden-path "model/golden/boolean.edn"
   :cases       {:nil            nil
                 :false          false
                 :true           true
                 :zero           0
                 :zero-double    0.0
                 :negative-zero  -0.0
                 :nan            ##NaN
                 :infinity       ##Inf
                 :zero-bigint    0N
                 :zero-bigdec    0.0M
                 :ratio          1/2
                 :empty-string   ""
                 :string-false   "false"
                 :string-zero    "0"
                 :empty-vector   []
                 :empty-map      {}
                 :empty-list     ()
                 :empty-set      #{}
                 :keyword-false  :false
                 :char-zero      \0
                 :symbol         'a
                 :regex          #""
                 :function       inc}
   :xf          encode
   :gen         inputs
   :pred        boolean?
   :num-tests   500
   :mutations   [["identity"        (fn [x] x)]
                 ["always-true"     (fn [_] true)]
                 ["negated"         (fn [x] (not (boolean x)))]
                 ["nil-only-falsey" (fn [x] (not (nil? x)))]
                 ["c-truthiness"    (fn [x] (not (or (nil? x) (false? x)
                                                     (and (number? x) (zero? x)))))]
                 ["empty-falsey"    (fn [x] (boolean (if (seqable? x) (seq x) x)))]
                 ["string-parsed"   (fn [x] (if (string? x)
                                              (Boolean/parseBoolean x)
                                              (boolean x)))]]})

(deftest boolean-laws
  (testing "the coercion is total: every value maps to a boolean, none refused"
    (is (every? (comp boolean? reference-boolean) (gen/sample inputs 300))))
  (testing "only nil and false are falsey"
    (is (every? (fn [x] (= (falsey? x) (not (reference-boolean x))))
                (gen/sample inputs 300))))
  (testing "zero, NaN and the empty collections are all truthy"
    (is (every? reference-boolean [0 0.0 -0.0 0N 0.0M ##NaN "" [] {} #{} ()])))
  (testing "the string \"false\" and the keyword :false are truthy — `boolean` is
            not a parser"
    (is (reference-boolean "false"))
    (is (reference-boolean :false)))
  (testing "the two falsey values are the only ones an `if` also treats as false"
    (is (= (map reference-boolean [nil false]) [false false]))
    (is (= (map #(if % :truthy :falsey) [nil false]) [:falsey :falsey])))
  (testing "the host boxes the result to its canonical booleans, so the coercion
            is idempotent and its outputs are identical, not merely equal"
    (is (identical? true (reference-boolean 0)))
    (is (identical? false (reference-boolean nil)))
    (is (= (reference-boolean 0) (reference-boolean (reference-boolean 0))))))
