(ns clojure.core-model.format
  "Behaviour model for `clojure.core/format`. Emits `model/golden/format.edn`,
  the baseline each dialect is graded against.

  `format` delegates to the host formatter, so this baseline is the JVM's
  `java.util.Formatter` answer. Two cases depend on the JVM's environment
  rather than on Clojure: `:grouped-decimal` on the default locale (en_US
  here) and `:platform-newline` on the line separator."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

(def Args
  "An argument vector handed to the characterizer: a format string followed by
  its arguments. Deliberately wider than the domain — the malformed calls are
  the point of the baseline."
  [:sequential :any])

(def Outcome
  "A recorded call result: the host's formatted string, or the class it threw."
  [:or :string [:tuple [:= :throws] :string]])

(defn reference-format
  "`format`, delegated to the host so the model characterizes rather than
  restates."
  [fmt & args]
  (apply format fmt args))

(m/=> reference-format [:=> [:cat :string [:* :any]] :string])

(defn- outcome
  "Total result of `thunk`: its value, or [:throws \"<class>\"] when it throws."
  [thunk]
  (try (thunk)
       (catch Throwable t [:throws (.getSimpleName (class t))])))

(defn characterize-format
  "`reference-format` applied to the argument vector `args`, as a total outcome."
  [args]
  (outcome #(apply reference-format args)))

(m/=> characterize-format [:=> [:cat Args] Outcome])

(defn- total-on-well-formed-calls?
  "A conversion matched by its argument always yields a string: the formatter
  refuses only on a mismatch, a missing argument or an unknown conversion."
  [o]
  (string? o))

(def ^:private gen-literal-format
  (gen/fmap #(string/replace % "%" "") gen/string))

(def ^:private gen-any
  (gen/one-of [gen/string gen/small-integer gen/boolean gen/keyword gen/symbol
               gen/char gen/double (gen/vector gen/small-integer 0 3)]))

(def ^:private gen-args
  (gen/one-of
   [(gen/fmap vector gen-literal-format)
    (gen/fmap (fn [x] ["%s" x]) gen-any)
    (gen/fmap (fn [x] ["%b" x]) gen-any)
    (gen/fmap (fn [n] ["%d" n]) gen/small-integer)
    (gen/fmap (fn [d] ["%.2f" d]) (gen/double* {:infinite? false :NaN? false}))]))

(deftrifecta format-behaviour
  clojure.core-model.format/characterize-format
  {:golden-path "model/golden/format.edn"
   :cases       {:literal              ["test"]
                 :empty-format         [""]
                 :string-of-long       ["%s" 1]
                 :string-of-nil        ["%s" nil]
                 :string-of-double     ["%s" 1.0]
                 :string-of-keyword    ["%s" :a/b]
                 :string-of-vector     ["%s" [1 2]]
                 :string-of-map        ["%s" {:a 1}]
                 :string-of-nan        ["%s" ##NaN]
                 :upper-string         ["%S" "ab"]
                 :precision-on-string  ["%10.3s|" "abcdef"]
                 :left-justified       ["%-10s|" "a"]
                 :decimal              ["%d" 42]
                 :decimal-of-double    ["%d" 1.5]
                 :decimal-of-bigint    ["%d" 10N]
                 :grouped-decimal      ["%,d" 1234567]
                 :hex                  ["%x" 255]
                 :zero-padded-float    ["%05.2f" 3.14159]
                 :float-of-ratio       ["%.3f" 1/3]
                 :float-of-bigdec      ["%.2f" 1.5M]
                 :float-of-infinity    ["%.2f" ##Inf]
                 :scientific           ["%e" 1234.5]
                 :boolean-of-nil       ["%b" nil]
                 :boolean-of-zero      ["%b" 0]
                 :char-of-long         ["%c" 65]
                 :two-arguments        ["%s %s" 1 2]
                 :indexed-arguments    ["%2$s %1$s" 1 2]
                 :extra-arguments      ["%s" 1 2 3]
                 :missing-argument     ["%s"]
                 :unknown-conversion   ["%q" 1]
                 :literal-percent      ["%%"]
                 :platform-newline     ["%n"]
                 :nil-format           [nil]}
   :xf          pr-str
   :gen         gen-args
   :pred        total-on-well-formed-calls?
   :num-tests   500
   ;; Each mutant must be REJECTED by the golden cases; a survivor means the
   ;; model asserts nothing.
   :mutations   [["identity"           (fn [args] args)]
                 ["always-empty"       (fn [_] "")]
                 ["format-string-only" (fn [[fmt]] (outcome #(str fmt)))]
                 ["ignores-arguments"  (fn [[fmt]] (outcome #(format fmt)))]
                 ["str-joined"         (fn [args] (outcome #(apply str args)))]
                 ["upper-cased"        (fn [args]
                                         (let [o (characterize-format args)]
                                           (if (string? o) (string/upper-case o) o)))]
                 ["never-refuses"      (fn [args]
                                         (let [o (characterize-format args)]
                                           (if (string? o) o "")))]]})

(defspec format-passes-a-literal-string-through 500
  (prop/for-all [s gen-literal-format]
    (= s (reference-format s))))

(defspec format-string-conversion-agrees-with-str-off-nil 500
  (prop/for-all [x gen-any]
    (= (str x) (reference-format "%s" x))))

(deftest format-laws
  (testing "a format string with no conversion passes through unchanged"
    (is (= "test" (reference-format "test")))
    (is (= "" (reference-format ""))))
  (testing "the string conversion agrees with str, except that nil is null"
    (is (= "1" (reference-format "%s" 1)))
    (is (= "[1 2]" (reference-format "%s" [1 2])))
    (is (= "null" (reference-format "%s" nil)))
    (is (= "" (str nil))))
  (testing "surplus arguments are ignored, missing ones are refused"
    (is (= "1" (reference-format "%s" 1 2 3)))
    (is (thrown? java.util.MissingFormatArgumentException (reference-format "%s"))))
  (testing "a conversion is checked against its argument's type"
    (is (thrown? java.util.IllegalFormatConversionException (reference-format "%d" 1.5)))
    (is (thrown? java.util.IllegalFormatConversionException (reference-format "%d" 10N)))
    (is (thrown? java.util.IllegalFormatConversionException (reference-format "%c" 65)))
    (is (thrown? java.util.UnknownFormatConversionException (reference-format "%q" 1))))
  (testing "the numeric tower is not uniformly accepted by a numeric conversion"
    (is (= "1.50" (reference-format "%.2f" 1.5M)))
    (is (thrown? java.util.IllegalFormatConversionException (reference-format "%.3f" 1/3)))))
