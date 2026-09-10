(ns conformance
  "Joins the per-dialect runs `native-test` persisted under
  `target/conformance` into a namespace-by-dialect status matrix, and writes
  it to `doc/conformance.md`.

  Only STATUS is compared across dialects. Assertion and test-var totals are
  each dialect's own unit — clojurust reports assertions where the `#counts`
  dialects report both — so a cross-dialect total would add unlike things.
  Per-dialect totals are reported per dialect and never summed across
  columns.

  A registered dialect with no file on disk gets a column reading
  `not measured`, never a silently missing one."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [native-test]))

(def doc-file "doc/conformance.md")

(def ^:private status-order [:green :red :abort :timeout])

(defn- read-run
  [file]
  (try
    (let [run (edn/read-string (slurp (fs/file file)))]
      (when (:dialect run) run))
    (catch Exception e
      (println "Skipping" (str file) "-" (ex-message e))
      nil)))

(defn read-runs
  "Every readable run under `native-test/results-dir`, keyed by dialect."
  []
  (into {} (keep (fn [f] (when-let [run (read-run f)] [(:dialect run) run]))
                 (fs/glob native-test/results-dir "*.edn"))))

(defn columns
  "One entry `{:dialect :label :run}` per column: every registered dialect in
  registry order, then any extra dialect found on disk. `:run` is nil when
  that dialect has not been measured."
  []
  (let [on-disk (read-runs)
        registered (keys native-test/dialects)
        extra (sort (remove (set registered) (keys on-disk)))]
    (vec (for [k (concat registered extra)]
           {:dialect k
            :label (or (:label (on-disk k))
                       (:label (native-test/dialect k))
                       (name k))
            :run (on-disk k)}))))

(defn matrix
  "namespace -> {dialect-key -> status}, over every namespace any run saw."
  [cols]
  (reduce (fn [m {:keys [dialect run]}]
            (reduce (fn [m {:keys [ns status]}] (assoc-in m [ns dialect] status))
                    m
                    (:results run)))
          (sorted-map)
          cols))

(defn namespaces-with
  "The namespaces `run` classified as `status`, sorted."
  [run status]
  (->> (:results run)
       (filter #(= status (:status %)))
       (map :ns)
       sort
       vec))

(defn- reference-column
  "The measured column with the most greens — the yardstick the other columns
  are read against."
  [cols]
  (->> cols
       (filter :run)
       (sort-by #(- (get-in % [:run :tally :green] 0)))
       first))

;; ---------------------------------------------------------------------------
;; Markdown.
;; ---------------------------------------------------------------------------

(defn- row [cells] (str "| " (str/join " | " (map str cells)) " |"))

(defn- table
  [headers rows]
  (str/join "\n" (concat [(row headers) (row (repeat (count headers) "---"))]
                         (map row rows))))

(defn- code-list
  [nses]
  (if (seq nses)
    (str/join ", " (map #(str "`" % "`") nses))
    "_none_"))

(defn- short-ns
  "`clojure.core-test.abs` -> `abs`, `clojure.string-test.replace` ->
  `string-test.replace`."
  [ns-name]
  (-> ns-name
      (str/replace #"^clojure\.core-test\." "")
      (str/replace #"^clojure\." "")))

(defn- provenance-table
  [cols]
  (table ["Dialect" "Binary" "Version" "Recorded (UTC)" "Timeout" "Namespaces"
          "green" "red" "abort" "timeout"]
         (for [{:keys [label run]} cols]
           (if run
             [label (str "`" (:binary run) "`") (or (:version run) "?")
              (:recorded-at run) (str (:timeout-seconds run) "s") (:total run)
              (get-in run [:tally :green]) (get-in run [:tally :red])
              (get-in run [:tally :abort]) (get-in run [:tally :timeout])]
             [label "-" "-" "not measured" "-" "-" "-" "-" "-" "-"]))))

(defn- goal-sections
  [cols]
  (for [{:keys [label run]} cols
        :when run
        :let [nses (namespaces-with run :abort)]]
    (str "### " label " — " (count nses) " of " (:total run) " abort\n\n"
         (code-list nses) "\n")))

(defn- red-sections
  [cols]
  (for [{:keys [label run]} cols
        :when run
        :let [nses (namespaces-with run :red)]]
    (str "### " label " — " (count nses) " of " (:total run)
         " ran and reported failures\n\n"
         (code-list nses) "\n")))

(defn- timeout-sections
  [cols]
  (for [{:keys [label run]} cols
        :when run
        :let [nses (namespaces-with run :timeout)]]
    (str "### " label " — " (count nses) " of " (:total run)
         " still running after " (:timeout-seconds run) "s\n\n"
         (code-list nses) "\n")))

(defn- divergence-section
  [cols]
  (if-let [{ref-label :label ref-run :run ref-key :dialect} (reference-column cols)]
    (let [ref-green (set (namespaces-with ref-run :green))
          others (remove #(= ref-key (:dialect %)) (filter :run cols))]
      (str "`" ref-label "` is the strongest measured column ("
           (count ref-green) " of " (:total ref-run)
           " green), so it stands in for what the suite itself supports. Each\n"
           "row splits those " (count ref-green)
           " namespaces by how the other dialect classified them.\n\n"
           (table (into ["Dialect"]
                        (for [s status-order] (str (name s) " here")))
                  (for [{:keys [label run]} others
                        :let [by-status (into {} (for [s status-order]
                                                   [s (set (namespaces-with run s))]))]]
                    (into [label]
                          (for [s status-order]
                            (count (filter ref-green (get by-status s)))))))))
    "_No dialect has been measured yet._"))

(defn- matrix-section
  [cols m]
  (let [measured (filter :run cols)]
    (table (into ["Namespace"] (map :label measured))
           (for [[ns-name statuses] m]
             (into [(str "`" (short-ns ns-name) "`")]
                   (for [{:keys [dialect]} measured]
                     (if-let [s (get statuses dialect)] (name s) "-")))))))

(defn markdown
  "The whole document, as a string."
  [cols]
  (let [m (matrix cols)
        unmeasured (remove :run cols)]
    (str
     "# clojure.core conformance matrix\n\n"
     "Generated by `bb conformance` from `" native-test/results-dir "/*.edn`.\n"
     "Each cell is one namespace run in its own OS process under one dialect.\n\n"
     "## What was measured\n\n"
     (provenance-table cols) "\n\n"
     (when (seq unmeasured)
       (str "Not measured: "
            (str/join ", " (map #(str (:label %) " (no `"
                                      native-test/results-dir "/"
                                      (name (:dialect %)) ".edn`)")
                                unmeasured))
            ". Run `bb conformance-sweep` to fill the column.\n\n"))
     "## How to read a status\n\n"
     "- `green` — the namespace ran and every assertion passed.\n"
     "- `red` — it ran and reported failures or errors: implemented, implemented wrong.\n"
     "- `abort` — it never reported, or reported an empty run: not implemented at all.\n"
     "- `timeout` — still running when the per-namespace deadline passed.\n"
     "- `-` — that dialect has no measurement for that namespace.\n\n"
     "Status is the only thing compared across columns. Each dialect counts in\n"
     "its own unit — clojurust reports assertions, the `#counts` dialects report\n"
     "test vars and assertions — so the counts above are per dialect and are\n"
     "never summed across dialects.\n\n"
     "## Goal list: namespaces a dialect does not implement at all\n\n"
     "These aborted: the run produced no test report at all, or reported an\n"
     "empty one. This is the roadmap.\n\n"
     (str/join "\n" (goal-sections cols)) "\n"
     "## Implemented, but reporting failures\n\n"
     (str/join "\n" (red-sections cols)) "\n"
     "## Timed out\n\n"
     "Neither missing nor wrong: too slow for the deadline this run used, or\n"
     "hung. Re-measure with `NATIVE_TEST_TIMEOUT` raised before reading one as\n"
     "a hang.\n\n"
     (str/join "\n" (timeout-sections cols)) "\n"
     "## Divergence\n\n"
     (divergence-section cols) "\n\n"
     "## Full matrix\n\n"
     "Namespace names are shown without their `clojure.` / `clojure.core-test.`\n"
     "prefix.\n\n"
     (matrix-section cols m) "\n")))

(defn generate!
  "Write `doc/conformance.md` from whatever runs are on disk. Returns the
  path."
  [& _]
  (let [cols (columns)
        measured (filter :run cols)]
    (when (empty? measured)
      (println "No runs under" native-test/results-dir
               "- run `bb test-rust`, `bb test-cljw` or `bb test-babashka` first."))
    (fs/create-dirs (fs/parent doc-file))
    (spit doc-file (markdown cols))
    (doseq [{:keys [label run]} measured]
      (println (str label ":")
               (str/join ", " (for [s status-order]
                                (str (get-in run [:tally s]) " " (name s))))))
    (println "Wrote" doc-file)
    doc-file))
