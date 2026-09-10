(ns clojure.core-model.parse-double
  "Behaviour model for `clojure.core/parse-double`. Emits `model/golden/parse-double.edn`,
  the baseline each dialect is graded against."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def DoubleText
  "The domain `parse-double` is defined on. Every other argument throws."
  :string)

(def Outcome
  "A total observation of one call: the parsed double, nil, or a marker naming
  the class that was thrown."
  [:or :nil :double [:map [:throws :string]]])

(defn reference-parse-double
  "`parse-double`, delegated to the host so the model characterizes rather than restates."
  [s]
  (parse-double s))

(m/=> reference-parse-double [:=> [:cat DoubleText] [:maybe :double]])

(defn- observe
  "The value `thunk` yields, or a marker naming the class it threw."
  [thunk]
  (try (thunk) (catch Throwable t {:throws (.getName (class t))})))

(defn observed-parse-double
  "`reference-parse-double` over any argument at all: an off-domain argument is
  recorded as a throw marker instead of aborting the snapshot."
  [x]
  (observe #(reference-parse-double x)))

(m/=> observed-parse-double [:=> [:cat :any] Outcome])

(defn- throw-marker?
  "True when the outcome records a throw rather than a returned value."
  [o]
  (and (map? o) (contains? o :throws)))

(defn- canonical-round-trip?
  "Every double `parse-double` returns is reproduced by parsing that double's own
  rendering, NaN compared as unordered. nil and a throw marker are accepted."
  [o]
  (cond
    (nil? o)          true
    (throw-marker? o) true
    (double? o)       (let [again (reference-parse-double (str o))]
                        (if (Double/isNaN o)
                          (and (double? again) (Double/isNaN again))
                          (= o again)))
    :else             false))

(def ^:private double-text-gen
  (gen/one-of [(gen/fmap str gen/double)
               (gen/fmap str gen/large-integer)
               gen/string-alphanumeric
               (gen/elements ["" "  " " 1.0 " "1.0\n" "Infinity" "-Infinity" "NaN"
                              "0x1p3" "1d" "1f" ".5" "5." "-0.0" "1e400" "1e-400"
                              "1/2" "5e" "1_0" "##Inf"])
               gen/simple-type]))

(deftrifecta parse-double-behaviour
  clojure.core-model.parse-double/observed-parse-double
  {:golden-path "model/golden/parse-double.edn"
   :cases       {:empty              ""
                 :blank              "  "
                 :integer            "1"
                 :one                "1.0"
                 :trailing-zeros     "1.000"
                 :explicit-plus      "+5.6"
                 :negative           "-8.7"
                 :exponent-lower     "9.0006e4"
                 :exponent-upper     "56851E-2"
                 :negative-exponent  "-1.058e2"
                 :infinity           "Infinity"
                 :negative-infinity  "-Infinity"
                 :nan                "NaN"
                 :signed-nan         "+NaN"
                 :reader-infinity    "##Inf"
                 :leading-space      " 1.0"
                 :trailing-space     "1.0 "
                 :tab-and-newline    "\t1.0\n"
                 :hex-float          "0x1p3"
                 :d-suffix           "1d"
                 :f-suffix           "1f"
                 :leading-dot        ".5"
                 :trailing-dot       "5."
                 :negative-zero      "-0.0"
                 :overflow           "1e400"
                 :underflow          "1e-400"
                 :ratio-text         "1/2"
                 :underscored        "1_0"
                 :bare-exponent      "5e"
                 :double-sign        "+-5.6"
                 :infinity-suffix    "Infinity7"
                 :word               "foo"
                 :non-string-long    1000
                 :non-string-double  0.0
                 :non-string-char    \a
                 :non-string-keyword :key
                 :nil                nil}
   :xf          pr-str
   :gen         double-text-gen
   :pred        canonical-round-trip?
   :num-tests   500
   :mutations   [["identity"           (fn [x] x)]
                 ["always-nil"         (fn [_] nil)]
                 ["never-throws"       (fn [x] (try (parse-double x) (catch Throwable _ nil)))]
                 ["rejects-whitespace" (fn [x] (observe #(when (= x (str/trim x)) (parse-double x))))]
                 ["drops-exponent"     (fn [x] (observe #(parse-double (str/replace x #"[eE].*$" ""))))]
                 ["float-precision"    (fn [x] (observe #(double (Float/parseFloat ^String x))))]]})

(deftest parse-double-laws
  (testing "surrounding whitespace IS trimmed, unlike parse-long"
    (is (= 1.0 (reference-parse-double " 1.0")))
    (is (= 1.0 (reference-parse-double "1.0 ")))
    (is (= 1.0 (reference-parse-double "\t1.0\n")))
    (is (nil? (reference-parse-double "  "))))
  (testing "the host's own float grammar is accepted, not only Clojure's"
    (is (= 8.0 (reference-parse-double "0x1p3")))
    (is (= 1.0 (reference-parse-double "1d")))
    (is (= 1.0 (reference-parse-double "1f")))
    (is (= 0.5 (reference-parse-double ".5")))
    (is (= 5.0 (reference-parse-double "5."))))
  (testing "the infinities and NaN are spelled the host's way, not the reader's"
    (is (= ##Inf (reference-parse-double "Infinity")))
    (is (= ##-Inf (reference-parse-double "-Infinity")))
    (is (Double/isNaN (reference-parse-double "NaN")))
    (is (nil? (reference-parse-double "##Inf"))))
  (testing "out-of-range magnitudes saturate rather than answering nil"
    (is (= ##Inf (reference-parse-double "1e400")))
    (is (= 0.0 (reference-parse-double "1e-400"))))
  (testing "negative zero survives the parse"
    (is (= "-0.0" (pr-str (reference-parse-double "-0.0")))))
  (testing "a non-string argument throws instead of answering nil"
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-double 1000)))
    (is (= {:throws "java.lang.IllegalArgumentException"} (observed-parse-double nil)))))
