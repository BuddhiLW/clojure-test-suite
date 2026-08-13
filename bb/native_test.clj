(ns native-test
  "Runs the suite under the two native (JVM-free, self-hosting) dialects:
  ClojureWasm (`cljw`) and clojurust (`cljrs`).

  Both read `.cljc` straight off a source path, so neither needs a build
  step — only a binary. The binary is resolved from `$CLJW_BIN` / `$CLJRS_BIN`
  when set, otherwise from `PATH`, so a checkout that builds its own binary
  (`zig-out/bin/cljw`, `target/release/cljrs`) does not have to be installed
  to be tested against.

  Each namespace runs in its OWN process, with a timeout. On a mature dialect
  one in-process run would do; on a young one a single namespace can hard-abort
  the runtime (an error the dialect raises outside its own catchable hierarchy)
  or hang it, and in one process that costs the other 247 namespaces' coverage.
  A per-namespace process turns both failures into one row of the report.

  `$NATIVE_TEST_TIMEOUT` (seconds, default 60) bounds a single namespace."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]))

(def ^:private test-root "test")

(defn- env
  [name default]
  (or (not-empty (System/getenv name)) default))

(defn- timeout-ms []
  (* 1000 (parse-long (env "NATIVE_TEST_TIMEOUT" "60"))))

(defn path->ns
  "The namespace a suite file declares: `test/clojure/core_test/set.cljc`
  -> `clojure.core-test.set`."
  [path]
  (-> (str path)
      (str/replace (re-pattern (str "^" test-root "/")) "")
      (str/replace #"\.cljc$" "")
      (str/replace "_" "-")
      (str/replace "/" ".")))

(defn test-namespaces
  "Every test namespace under `test/`, sorted. `portability` is support code
  rather than a test namespace — every other file requires it."
  []
  (->> (fs/glob test-root "**/*.cljc")
       (map path->ns)
       (remove #{"clojure.core-test.portability"})
       sort))

;; ---------------------------------------------------------------------------
;; Per-dialect: the command for one namespace, and how to read its counts.
;; ---------------------------------------------------------------------------

(defn- cljw-command [ns-name]
  ;; The counts are printed on a marker line: cljw's summary map is a value,
  ;; and parsing a printed map is more brittle than printing what we need.
  [(env "CLJW_BIN" "cljw") "-cp" test-root "-e"
   (str "(require 'clojure.test) (require '" ns-name ")"
        " (let [s (clojure.test/run-tests '" ns-name ")]"
        "   (println \"#counts\" (:test s) (:pass s) (:fail s) (:error s)))")])

(defn- cljrs-command [ns-name]
  [(env "CLJRS_BIN" "cljrs") "test" "--src-path" test-root ns-name])

(defn- parse-counts
  "The {:tests :pass :fail :error} a run reported, or nil when it never got
  far enough to report anything."
  [dialect out]
  (case dialect
    :cljw (when-let [[_ t p f e] (re-find #"#counts (\d+) (\d+) (\d+) (\d+)" out)]
            {:tests (parse-long t) :pass (parse-long p)
             :fail (parse-long f) :error (parse-long e)})
    :cljrs (when-let [[_ p f e] (re-find #"(\d+) passed, (\d+) failed, (\d+) errors" out)]
             ;; cljrs reports assertions, not test vars, in its pass line
             {:tests nil :pass (parse-long p)
              :fail (parse-long f) :error (parse-long e)})))

;; ---------------------------------------------------------------------------
;; Runner.
;; ---------------------------------------------------------------------------

(defn- run-one
  "Run one namespace and classify it: :green, :red (it reported failures),
  :abort (it died without reporting), or :timeout."
  [dialect command ns-name]
  (let [proc (process/process command {:out :string :err :string})
        done (deref proc (timeout-ms) ::timeout)]
    (if (= ::timeout done)
      (do (process/destroy-tree proc)
          {:ns ns-name :status :timeout})
      (let [out (str (:out done) (:err done))
            counts (parse-counts dialect out)]
        (cond
          (nil? counts) {:ns ns-name :status :abort :out out}
          (pos? (+ (:fail counts) (:error counts)))
          (assoc counts :ns ns-name :status :red :out out)
          :else (assoc counts :ns ns-name :status :green))))))

(defn- report!
  "Print the per-namespace detail for everything that is not green, then the
  totals. Returns the process exit code."
  [dialect-label results]
  (let [by-status (group-by :status results)
        tally (fn [k] (reduce + (keep k results)))]
    (doseq [status [:red :abort :timeout]
            {:keys [ns out]} (get by-status status)]
      (println)
      (println (str/upper-case (name status)) "-" ns)
      (when out
        (doseq [line (take-last 40 (str/split-lines (str/trim out)))]
          (println "   " line))))
    (println)
    (println (str dialect-label ":")
             (count (get by-status :green)) "green,"
             (count (get by-status :red)) "with failures,"
             (count (get by-status :abort)) "aborted,"
             (count (get by-status :timeout)) "timed out,"
             "of" (count results) "namespaces.")
    (println "Assertions:" (tally :pass) "passed," (tally :fail) "failed,"
             (tally :error) "errors.")
    (if (= (count results) (count (get by-status :green))) 0 1)))

(defn- run-suite [dialect label command-fn]
  (let [nses (test-namespaces)]
    (println "Running" (count nses) "namespaces under" label)
    (let [results (doall (map #(run-one dialect (command-fn %) %) nses))]
      (System/exit (report! label results)))))

(defn cljw
  "Run the suite under ClojureWasm."
  [& _]
  (run-suite :cljw "ClojureWasm" cljw-command))

(defn cljrs
  "Run the suite under clojurust."
  [& _]
  (run-suite :cljrs "clojurust" cljrs-command))
