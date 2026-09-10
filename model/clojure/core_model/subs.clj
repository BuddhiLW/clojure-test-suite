(ns clojure.core-model.subs
  "Behaviour model for `clojure.core/subs`. Emits `model/golden/subs.edn`, the
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
  domain: the out-of-range and wrong-type calls are the point of the baseline."
  [:sequential :any])

(def Outcome
  "A recorded call result: the host's substring, or the class it threw."
  [:or :string [:tuple [:= :throws] :string]])

(defn reference-subs
  "`subs`, delegated to the host so the model characterizes rather than restates."
  ([s start] (subs s start))
  ([s start end] (subs s start end)))

(m/=> reference-subs [:function
                      [:=> [:cat :string :int] :string]
                      [:=> [:cat :string :int :int] :string]])

(defn- outcome
  "Total result of `thunk`: its value, or [:throws \"<class>\"] when it throws."
  [thunk]
  (try (thunk)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(defn characterize-subs
  "`reference-subs` applied to the argument vector `args`, as a total outcome."
  [args]
  (outcome #(apply reference-subs args)))

(m/=> characterize-subs [:=> [:cat Args] Outcome])

(def ^:private bounds-classes
  #{"StringIndexOutOfBoundsException" "NullPointerException" "ClassCastException"})

(defn- known-outcome?
  "`subs` either returns a string or fails in one of three known ways; it never
  returns nil and never throws anything else."
  [o]
  (or (string? o)
      (and (vector? o) (= :throws (first o)) (contains? bounds-classes (second o)))))

(defn- in-bounds?
  "Whether `args` names an index range that lies inside the string."
  [[s start end]]
  (and (string? s)
       (int? start)
       (<= 0 start (count s))
       (or (nil? end) (and (int? end) (<= start end (count s))))))

(defn- slice-of-input?
  "Inside its bounds the result is exactly the character range [start end): the
  prefix, the result and the suffix reassemble the input."
  [[s start end :as args]]
  (let [o (characterize-subs args)
        e (or end (count s))]
    (and (string? o)
         (= (- e start) (count o))
         (= s (str (apply str (take start s)) o (apply str (drop e s)))))))

(def ^:private gen-in-bounds
  (gen/let [s     gen/string
            start (gen/choose 0 (count s))
            end   (gen/choose start (count s))
            two?  gen/boolean]
    (if two? [s start] [s start end])))

(def ^:private gen-out-of-bounds
  (gen/let [s     gen/string
            good  (gen/choose 0 (count s))
            bad   (gen/one-of [(gen/choose -5 -1)
                               (gen/choose (inc (count s)) (+ (count s) 5))])
            shape (gen/elements [:bad-start-2 :bad-start-3 :bad-end-3 :inverted-3])]
    (case shape
      :bad-start-2 [s bad]
      :bad-start-3 [s bad (count s)]
      :bad-end-3   [s good bad]
      :inverted-3  [s (inc good) good])))

(def ^:private gen-args (gen/one-of [gen-in-bounds gen-out-of-bounds]))

(deftrifecta subs-behaviour
  clojure.core-model.subs/characterize-subs
  {:golden-path "model/golden/subs.edn"
   :cases       {:whole                ["abcde" 0]
                 :whole-explicit       ["abcde" 0 5]
                 :non-ascii-whole      ["ab֎de" 0]
                 :non-ascii-explicit   ["ab֎de" 0 5]
                 :from-one             ["abcde" 1]
                 :non-ascii-from-one   ["֎bcde" 1]
                 :middle               ["abcde" 1 4]
                 :non-ascii-middle     ["֎bcde" 1 4]
                 :prefix               ["abcde" 0 3]
                 :non-ascii-prefix     ["ab֎de" 0 3]
                 :astral-first-unit    ["a😀b" 0 1]
                 :astral-pair          ["a😀b" 1 3]
                 :astral-tail          ["a😀b" 3]
                 :empty-string         ["" 0]
                 :empty-string-range   ["" 0 0]
                 :empty-at-start       ["abcde" 0 0]
                 :empty-at-end         ["abcde" 5]
                 :empty-range-at-end   ["abcde" 5 5]
                 :empty-range-inside   ["abcde" 4 4]
                 :inverted-range       ["abcde" 2 1]
                 :end-past-length      ["abcde" 1 6]
                 :end-far-past-length  ["abcde" 1 200]
                 :start-past-length    ["abcde" 6]
                 :negative-start       ["abcde" -1]
                 :negative-start-range ["abcde" -1 3]
                 :both-negative        ["abcde" -1 -3]
                 :nil-string           [nil 1 2]
                 :nil-start            ["abcde" nil 2]
                 :nil-end              ["abcde" 1 nil]
                 :non-string           [42 0]}
   :xf          pr-str
   :gen         gen-args
   :pred        known-outcome?
   :num-tests   500
   ;; Each mutant must be REJECTED by the golden cases; a survivor means the
   ;; model asserts nothing.
   :mutations   [["identity"      (fn [args] args)]
                 ["always-empty"  (fn [_] "")]
                 ["whole-string"  (fn [[s]] (outcome #(str s)))]
                 ["ignores-end"   (fn [[s start _]] (outcome #(subs s start)))]
                 ["off-by-one"    (fn [[s start end]]
                                    (outcome #(if end
                                                (subs s (inc start) end)
                                                (subs s (inc start)))))]
                 ["clamped"       (fn [[s start end]]
                                    (outcome
                                     #(let [t (str s)
                                            n (count t)
                                            a (max 0 (min n (int (or start 0))))
                                            b (max a (min n (int (or end n))))]
                                        (subs t a b))))]
                 ["reversed"      (fn [args]
                                    (let [o (characterize-subs args)]
                                      (if (string? o) (string/reverse o) o)))]
                 ["prefix"        (fn [[s start end]]
                                    (outcome #(subs s 0 (or end start))))]]})

(defspec subs-is-a-slice-inside-its-bounds 500
  (prop/for-all [args gen-in-bounds]
    (slice-of-input? args)))

(defspec subs-throws-outside-its-bounds 500
  (prop/for-all [args gen-out-of-bounds]
    (let [o (characterize-subs args)]
      (and (vector? o) (= :throws (first o))))))

(deftest subs-laws
  (testing "the identity slice returns the input"
    (is (every? #(= % (reference-subs % 0)) ["" "a" "abcde" "ab֎de"]))
    (is (= "abcde" (reference-subs "abcde" 0 5))))
  (testing "an empty range anywhere in bounds is the empty string"
    (is (every? #(= "" (reference-subs "abcde" % %)) [0 1 2 3 4 5]))
    (is (= "" (reference-subs "abcde" 5))))
  (testing "indices count UTF-16 code units, not code points"
    (is (= 4 (count "a😀b")))
    (is (= "😀" (reference-subs "a😀b" 1 3)))
    (is (= "b" (reference-subs "a😀b" 3))))
  (testing "the bounds are checked, not clamped"
    (is (thrown? StringIndexOutOfBoundsException (reference-subs "abcde" 6)))
    (is (thrown? StringIndexOutOfBoundsException (reference-subs "abcde" -1)))
    (is (thrown? StringIndexOutOfBoundsException (reference-subs "abcde" 2 1)))
    (is (thrown? StringIndexOutOfBoundsException (reference-subs "abcde" 1 6)))))
