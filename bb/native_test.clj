(ns native-test
  "Runs the suite one namespace at a time under a non-JVM dialect, prints a
  report, and records the run.

  A dialect is one row of the `dialects` registry: the argv that runs a single
  namespace, the reader that pulls counts back out of its output, and the
  environment variable that overrides its binary. Registered: clojurust
  (`cljrs`), ClojureWasm (`cljw`) and babashka (`bb`). All three read `.cljc`
  straight off a source path, so none needs a build step — only a binary.

  Every namespace runs in its OWN process: on a young dialect a single
  namespace can abort the runtime outside its own catchable hierarchy, or hang
  it, and in one shared process that would cost every other namespace's
  coverage.

  `run-suite` returns the run and writes it to
  `target/conformance/<dialect>.edn`; `conformance` joins those files into
  `doc/conformance.md`.

  `$NATIVE_TEST_TIMEOUT` (seconds, default 60) bounds one namespace.
  `$CLJRS_BIN` / `$CLJW_BIN` / `$BB_BIN` override binary lookup."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def ^:private test-root "test")

(def results-dir
  "Where a run is persisted. Under `target`, which `.gitignore` ignores:
  a run is a per-machine measurement, not a source."
  "target/conformance")

(defn- env
  [name default]
  (or (not-empty (System/getenv name)) default))

(defn- timeout-seconds []
  (parse-long (env "NATIVE_TEST_TIMEOUT" "60")))

(defn- timeout-ms []
  (* 1000 (timeout-seconds)))

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
  "Every test namespace under `test/`, sorted. `portability` and
  `number-range` are support code rather than test namespaces: they declare no
  `deftest`, and the other files require them."
  []
  (->> (fs/glob test-root "**/*.cljc")
       (map path->ns)
       (remove #{"clojure.core-test.portability"
                 "clojure.core-test.number-range"})
       sort))

;; ---------------------------------------------------------------------------
;; The dialect registry: how to invoke one, and how to read its counts.
;; ---------------------------------------------------------------------------

(defn- run-tests-expr
  "A `clojure.test/run-tests` form for `ns-name` that echoes its summary on a
  `#counts <tests> <pass> <fail> <error>` marker line."
  [ns-name]
  (str "(require 'clojure.test) (require '" ns-name ")"
       " (let [s (clojure.test/run-tests '" ns-name ")]"
       "   (println \"#counts\" (:test s) (:pass s) (:fail s) (:error s)))"))

(defn- cp-e-command
  "The argv for a dialect whose CLI takes `-cp <path> -e <expr>`."
  [bin ns-name]
  [bin "-cp" test-root "-e" (run-tests-expr ns-name)])

(defn- marker-counts
  [out]
  (when-let [[_ t p f e] (re-find #"#counts (\d+) (\d+) (\d+) (\d+)" out)]
    {:tests (parse-long t) :pass (parse-long p)
     :fail (parse-long f) :error (parse-long e)}))

(defn- cljrs-counts
  [out]
  (when-let [[_ p f e] (re-find #"(\d+) passed, (\d+) failed, (\d+) errors" out)]
    {:tests nil :pass (parse-long p)
     :fail (parse-long f) :error (parse-long e)}))

(def dialects
  "Every dialect the sweep can measure, in report order, keyed by the name its
  `bb` task uses.

  `:command` builds the argv for one namespace. `:counts` reads
  `{:tests :pass :fail :error}` out of that run's combined stdout+stderr, or
  nil when the run never reported. `:reports` names the units the dialect
  emits: they differ between dialects, so counts are compared within a dialect
  and never across them."
  (array-map
   :cljrs {:label "clojurust"
           :binary-env "CLJRS_BIN"
           :binary-default "cljrs"
           :version-args ["--version"]
           :reports #{:assertions}
           :command (fn [bin ns-name]
                      [bin "test" "--src-path" test-root ns-name])
           :counts cljrs-counts}
   :cljw {:label "ClojureWasm"
          :binary-env "CLJW_BIN"
          :binary-default "cljw"
          :version-args ["--version"]
          :reports #{:test-vars :assertions}
          :command cp-e-command
          :counts marker-counts}
   :babashka {:label "babashka"
              :binary-env "BB_BIN"
              :binary-default "bb"
              :version-args ["--version"]
              :reports #{:test-vars :assertions}
              :command cp-e-command
              :counts marker-counts}))

(defn dialect
  "The registry row for `dialect-key`, or nil."
  [dialect-key]
  (get dialects dialect-key))

(defn binary
  "The binary `dialect-key` will invoke."
  [dialect-key]
  (let [{:keys [binary-env binary-default]} (dialect dialect-key)]
    (env binary-env binary-default)))

(defn available?
  "Whether `dialect-key`'s binary resolves to something runnable."
  [dialect-key]
  (let [bin (binary dialect-key)]
    (boolean (or (fs/executable? bin) (fs/which bin)))))

(defn- binary-version
  "What `<binary> --version` prints, or nil when it cannot be run."
  [dialect-key]
  (let [{:keys [version-args]} (dialect dialect-key)]
    (try
      (let [{:keys [out err exit]} (process/sh (into [(binary dialect-key)]
                                                     version-args))]
        (when (zero? exit)
          (not-empty (str/trim (str out err)))))
      (catch Exception _ nil))))

;; ---------------------------------------------------------------------------
;; Runner.
;; ---------------------------------------------------------------------------

(defn- tail
  [n out]
  (when (not-empty (str/trim (str out)))
    (str/join "\n" (take-last n (str/split-lines (str/trim out))))))

(defn- classify
  "Classify one finished run of one namespace.

  :green   it reported, and every assertion passed
  :red     it reported failures or errors
  :abort   it never reported, or reported an empty run
  :timeout it was still running when the deadline passed

  A run that reports zero tests AND zero assertions is an :abort, not a
  :green: cljrs prints `0 passed, 0 failed, 0 errors` for a namespace it could
  not load at all."
  [ns-name counts exit out]
  (let [row {:ns ns-name :exit exit}]
    (cond
      (nil? counts)
      (assoc row :status :abort :out (tail 40 out))

      (zero? (+ (or (:tests counts) 0) (:pass counts)
                (:fail counts) (:error counts)))
      (merge row counts {:status :abort :reported-nothing true
                         :out (tail 40 out)})

      (pos? (+ (:fail counts) (:error counts)))
      (merge row counts {:status :red :out (tail 40 out)})

      :else
      (merge row counts {:status :green}))))

(defn- run-one
  "Run `ns-name` under `dialect-key` in its own process and classify it."
  [dialect-key bin ns-name]
  (let [{:keys [command counts]} (dialect dialect-key)
        proc (process/process (command bin ns-name) {:out :string :err :string})
        done (deref proc (timeout-ms) ::timeout)]
    (if (= ::timeout done)
      (do (process/destroy-tree proc)
          {:ns ns-name :status :timeout})
      (let [out (str (:out done) (:err done))]
        (classify ns-name (counts out) (:exit done) out)))))

(defn- tally
  [results]
  (let [by-status (group-by :status results)]
    (into {} (for [status [:green :red :abort :timeout]]
               [status (count (get by-status status))]))))

(defn- report!
  "Print the per-namespace detail for everything that is not green, then the
  totals. Returns the process exit code."
  [{:keys [label results]}]
  (let [by-status (group-by :status results)
        sum (fn [k] (reduce + (keep k results)))
        counts (tally results)]
    (doseq [status [:red :abort :timeout]
            {:keys [ns out]} (get by-status status)]
      (println)
      (println (str/upper-case (name status)) "-" ns)
      (when out
        (doseq [line (str/split-lines out)]
          (println "   " line))))
    (println)
    (println (str label ":")
             (:green counts) "green,"
             (:red counts) "with failures,"
             (:abort counts) "aborted,"
             (:timeout counts) "timed out,"
             "of" (count results) "namespaces.")
    (println "Assertions:" (sum :pass) "passed," (sum :fail) "failed,"
             (sum :error) "errors.")
    (if (= (count results) (:green counts)) 0 1)))

(defn- persist!
  "Write `run` to `target/conformance/<dialect>.edn`. Returns the file."
  [{:keys [dialect] :as run}]
  (fs/create-dirs results-dir)
  (let [file (fs/file results-dir (str (name dialect) ".edn"))]
    (spit file (with-out-str (pprint/pprint run)))
    file))

(defn run-suite
  "Run every test namespace under `dialect-key`, print a report, persist the
  run under `target/conformance`, and RETURN it:

    {:dialect :label :binary :version :reports :timeout-seconds :recorded-at
     :total :tally :results :exit}

  `:results` is one row per namespace; `:exit` is the code a CI task should
  exit with. Nothing here exits the process."
  [dialect-key]
  (when-not (dialect dialect-key)
    (throw (ex-info (str "Unknown dialect: " dialect-key)
                    {:known (vec (keys dialects))})))
  (let [{:keys [label reports]} (dialect dialect-key)
        bin (binary dialect-key)
        nses (test-namespaces)]
    (println "Running" (count nses) "namespaces under" label (str "(" bin ")"))
    (let [results (mapv #(run-one dialect-key bin %) nses)
          run {:dialect dialect-key
               :label label
               :binary bin
               :version (binary-version dialect-key)
               :reports reports
               :timeout-seconds (timeout-seconds)
               :recorded-at (str (java.time.Instant/now))
               :total (count results)
               :tally (tally results)
               :results results}
          exit (report! run)
          file (persist! run)]
      (println "Wrote" (str file))
      (assoc run :exit exit))))

(defn sweep
  "Run `dialect-keys` (default: every registered dialect whose binary
  resolves) without exiting. Returns one map per dialect, in the order given;
  a dialect with no binary is returned unmeasured rather than run."
  [& dialect-keys]
  (let [ks (map #(keyword (name %)) (or (seq dialect-keys) (keys dialects)))]
    (vec (for [k ks]
           (if (available? k)
             (run-suite k)
             (do (println "Skipping" (name k) "- no binary at" (binary k))
                 {:dialect k :label (:label (dialect k)) :binary (binary k)
                  :measured false}))))))

(defn- run-and-exit
  [dialect-key]
  (System/exit (:exit (run-suite dialect-key))))

(defn cljw
  "Run the suite under ClojureWasm. Exits with the suite's code."
  [& _]
  (run-and-exit :cljw))

(defn cljrs
  "Run the suite under clojurust. Exits with the suite's code."
  [& _]
  (run-and-exit :cljrs))

(defn babashka
  "Run the suite under babashka, one namespace per subprocess. Exits with the
  suite's code."
  [& _]
  (run-and-exit :babashka))
