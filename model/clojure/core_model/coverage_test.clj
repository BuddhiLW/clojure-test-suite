(ns clojure.core-model.coverage-test
  "Coverage gates over the behaviour model, requiring both rungs.

  Rung 1 (malli): every public function under `model/` carries an `m/=>`
  contract or a reasoned exemption. Delegated to `hive-schemas.coverage`.

  Rung 2 (typedclojure): every model namespace carrying `m/=>` contracts has a
  `*_typed.clj` sibling that exists, carries `^:typed.clojure` ns metadata,
  declares at least one `t/ann`, annotates every name it defines, and annotates
  the contracted names or exempts them with a reason. The sibling is read as
  DATA; nothing here invokes the checker.

  Levers:
    static-rung-exemptions  -> {qualified-sym reason} derived from the typed files
    malli-rung-opts         -> the opts `hive-schemas.coverage/coverage` runs on
    typed-rung              -> the rung-2 report
    typed-rung-failures     -> the rung-2 gate messages"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as rt]
            [hive-schemas.coverage :as cov]))

(def ^:private model-paths
  "Source roots holding the behaviour model."
  ["model"])

(def ^:private typed-suffix
  "Filename suffix marking the static-rung sibling of a model file."
  "_typed.clj")

(def ^:private helper-exemptions
  "Public functions in the model that carry no `m/=>`, each with its reason."
  {'clojure.core-model.abs/non-negative-except-long-min?
   "model-internal predicate consumed by deftrifecta :pred; not part of the modelled clojure.core surface — the cleaner fix is defn- in abs.clj"

   'clojure.core-model.abs/idempotent?
   "model-internal law predicate exercised by abs-laws; not part of the modelled clojure.core surface — the cleaner fix is defn- in abs.clj"

   'clojure.core-model.abs/sign-symmetric?
   "model-internal law predicate exercised by abs-laws; not part of the modelled clojure.core surface — the cleaner fix is defn- in abs.clj"})

(def ^:private annotation-exemptions
  "Contracted names with no same-named `t/ann`, each with its reason."
  {'clojure.core-model.abs/reference-abs
   "delegates to the host `abs`; the static rung states the domain restrictions as abs-int / abs-num rather than re-annotating the delegate"})

;; =============================================================================
;; Reading source as data
;; =============================================================================

(defn- read-forms
  "`{:file path :forms [form ...]}` for `f`, or `{:file path :error message}`.

  Reader conditionals are allowed under `:clj`, `*read-eval*` is off, and every
  alias resolves to one placeholder, so a file is scannable without loading it."
  [f]
  (let [file (io/file f)
        path (.getPath file)]
    (try
      (let [rdr (rt/string-push-back-reader (slurp file))]
        (binding [tr/*read-eval*  false
                  tr/*alias-map*  (fn [_] 'clojure.core-model.coverage-test.unresolved-alias)]
          (loop [forms []]
            (let [form (tr/read {:read-cond :allow :features #{:clj} :eof ::eof} rdr)]
              (if (= ::eof form)
                {:file path :forms forms}
                (recur (conj forms form)))))))
      (catch Exception e
        {:file path :error (str (.getSimpleName (class e)) ": " (.getMessage e))}))))

(defn- head-named
  "Simple symbols `s` of every top-level `(head s ...)` in `forms` whose head
  symbol has short name `head`, alias-agnostically."
  [forms head]
  (into (sorted-set)
        (keep (fn [form]
                (when (and (seq? form)
                           (symbol? (first form))
                           (symbol? (second form))
                           (= head (name (first form))))
                  (second form))))
        forms))

(defn- ns-symbol
  "The symbol named by the `ns` form in `forms`, metadata intact; nil when absent."
  [forms]
  (first (keep (fn [form]
                 (when (and (seq? form) (= 'ns (first form)) (symbol? (second form)))
                   (second form)))
               forms)))

(defn- defined-names
  "Simple symbols introduced by a top-level `def`, `defn` or `defn-` in `forms`."
  [forms]
  (into (sorted-set) (mapcat #(head-named forms %)) ["def" "defn" "defn-"]))

(defn- model-files
  "The `.clj` files under `model-paths` that are not static-rung siblings."
  []
  (into [] (remove #(str/ends-with? (.getPath ^java.io.File %) typed-suffix))
        (cov/source-files model-paths #{"clj"})))

(defn- typed-files
  "The static-rung sibling files under `model-paths`."
  []
  (into [] (filter #(str/ends-with? (.getPath ^java.io.File %) typed-suffix))
        (cov/source-files model-paths #{"clj"})))

(defn- typed-path
  "The static-rung sibling path of model file `path`."
  [path]
  (str/replace path #"\.clj$" typed-suffix))

;; =============================================================================
;; Rung 1 — malli
;; =============================================================================

(defn- static-rung-exemptions
  "`{qualified-sym reason}` for every public `defn` in a `*_typed.clj` file:
  its signature is a `t/ann`, which malli's registry never holds. Derived from
  whatever typed files are present, never from a fixed list."
  []
  (into {}
        (comp (map read-forms)
              (mapcat (fn [{:keys [forms file error]}]
                        (when-not error
                          (when-let [nsym (ns-symbol forms)]
                            (map (fn [s]
                                   [(symbol (str nsym) (str s))
                                    (str "static rung: the signature is a t/ann in " file
                                         "; malli's registry never sees it")])
                                 (head-named forms "defn")))))))
        (typed-files)))

(defn- malli-rung-opts
  "Opts for `hive-schemas.coverage/coverage`, evaluated at test time."
  []
  {:paths  model-paths
   :exempt (merge (static-rung-exemptions) helper-exemptions)})

;; =============================================================================
;; Rung 2 — typedclojure
;; =============================================================================

(defn- audit-model-file
  "Static-rung audit of one model file, or nil when it declares no `m/=>`."
  [f]
  (let [{:keys [forms error file]} (read-forms f)]
    (if error
      {:file file :error error}
      (let [nsym      (ns-symbol forms)
            contracts (head-named forms "=>")]
        (when (and nsym (seq contracts))
          (let [tpath    (typed-path file)
                present? (.exists (io/file tpath))
                base     {:ns nsym :file file :contracts contracts
                          :typed-file tpath :typed-present? present?}]
            (if-not present?
              base
              (let [{tforms :forms terror :error} (read-forms tpath)]
                (if terror
                  (assoc base :typed-error terror)
                  (let [tns  (ns-symbol tforms)
                        anns (head-named tforms "ann")]
                    (assoc base
                           :typed-ns tns
                           :typed-ns-metadata? (boolean (:typed.clojure (meta tns)))
                           :annotations anns
                           :unannotated (into (sorted-set) (remove anns) (defined-names tforms))
                           :contracts-without-annotation
                           (into (sorted-set)
                                 (remove (fn [s]
                                           (or (contains? anns s)
                                               (contains? annotation-exemptions
                                                          (symbol (str nsym) (str s))))))
                                 contracts))))))))))))

(defn- qualify
  "`names` qualified into namespace `nsym`."
  [nsym names]
  (map (fn [s] (symbol (str nsym) (str s))) names))

(defn- typed-rung
  "Static-rung report over every model namespace carrying `m/=>` contracts.

  `:namespaces` holds one audit per such namespace; `:stale-exemptions` and
  `:blank-reasons` keep `annotation-exemptions` honest."
  []
  (let [audits     (into [] (keep audit-model-file) (model-files))
        annotated  (into #{} (mapcat #(qualify (:ns %) (:annotations %))) audits)
        contracted (into #{} (mapcat #(qualify (:ns %) (:contracts %))) audits)]
    {:paths      model-paths
     :namespaces audits
     :annotated  annotated
     :contracted contracted
     :exempt     annotation-exemptions
     :blank-reasons
     (into (sorted-set)
           (keep (fn [[k v]] (when (or (not (string? v)) (str/blank? v)) k)))
           annotation-exemptions)
     :stale-exemptions
     (into (sorted-set)
           (filter (fn [k] (or (not (contains? contracted k))
                               (contains? annotated k))))
           (keys annotation-exemptions))}))

(defn- namespace-failures
  "The gate messages one audit fails, in order."
  [{ns-sym :ns :keys [file contracts typed-file typed-present? typed-error typed-ns
                      typed-ns-metadata? annotations unannotated
                      contracts-without-annotation error]}]
  (cond
    error          [(str file ": unreadable — " error)]
    (not typed-present?)
    [(str ns-sym " carries " (count contracts) " m/=> contract(s) but no static rung: "
          typed-file " is absent")]
    typed-error    [(str typed-file ": unreadable — " typed-error)]
    :else
    (cond-> []
      (not typed-ns-metadata?)
      (conj (str typed-file ": ns " typed-ns
                 " lacks ^:typed.clojure metadata — the checker never looks at it"))

      (empty? annotations)
      (conj (str typed-file ": declares no t/ann — the static rung is vacuous"))

      (seq unannotated)
      (conj (str typed-file ": defined without a t/ann: " (pr-str (vec unannotated))))

      (seq contracts-without-annotation)
      (conj (str ns-sym ": contracted but not annotated in " typed-file ": "
                 (pr-str (vec contracts-without-annotation)))))))

(defn- typed-rung-failures
  "The gate messages `report` fails, in order; empty when it passes.

  Gates: at least one contracted namespace was found; each has a readable,
  `^:typed.clojure`, non-empty and fully annotated static sibling; every
  contracted name is annotated or exempted; every exemption carries a prose
  reason and still applies."
  [report]
  (vec
   (concat
    (when (empty? (:namespaces report))
      [(str "vacuous static-rung scan — no namespace under " (pr-str (:paths report))
            " carries an m/=> contract")])
    (mapcat namespace-failures (:namespaces report))
    (when (seq (:blank-reasons report))
      [(str "annotation exemptions without a reason: "
            (pr-str (vec (:blank-reasons report))))])
    (when (seq (:stale-exemptions report))
      [(str "stale annotation exemptions (never contracted, or now annotated): "
            (pr-str (vec (:stale-exemptions report))))]))))

;; =============================================================================
;; Report
;; =============================================================================

(defn- print-report!
  "Print what each rung found, naming every gap."
  []
  (let [malli (cov/coverage (malli-rung-opts))
        typed (typed-rung)]
    (println)
    (println "=== behaviour-model coverage:" (pr-str model-paths) "===")
    (println (format "rung 1 (malli): %d/%d contracted, ratio %.3f"
                     (count (:covered malli)) (count (:universe malli)) (:ratio malli)))
    (doseq [s (:missing malli)]           (println "  MISSING m/=>       " s))
    (doseq [f (:unreadable malli)]        (println "  UNREADABLE         " (:file f)))
    (doseq [f (:unloadable malli)]        (println "  UNLOADABLE         " (:ns f) (:error f)))
    (doseq [s (:stale-exemptions malli)]  (println "  STALE exemption    " s))
    (doseq [[k v] (sort-by key (:exempt malli))] (println "  exempt             " k "--" v))
    (println (format "rung 2 (typedclojure): %d contracted namespace(s)"
                     (count (:namespaces typed))))
    (doseq [{ns-sym :ns :keys [contracts typed-file typed-present? annotations
                               unannotated contracts-without-annotation]}
            (:namespaces typed)]
      (println " " ns-sym "->" typed-file (if typed-present? "" "[ABSENT]"))
      (println "    contracts  " (pr-str (vec contracts)))
      (println "    t/ann      " (pr-str (vec annotations)))
      (when (seq unannotated)
        (println "    NO t/ann FOR DEFINITION" (pr-str (vec unannotated))))
      (when (seq contracts-without-annotation)
        (println "    CONTRACT WITHOUT t/ann " (pr-str (vec contracts-without-annotation)))))
    (doseq [s (:stale-exemptions typed)] (println "  STALE annotation exemption" s))
    (doseq [[k v] (sort-by key (:exempt typed))] (println "  annotation exempt  " k "--" v))
    (println "=== end coverage report ===")))

(use-fixtures :once (fn [run] (print-report!) (run)))

;; =============================================================================
;; Gates
;; =============================================================================

(cov/deftest-contract-coverage malli-rung-coverage (malli-rung-opts))

(deftest typed-rung-coverage
  (let [report   (typed-rung)
        failures (typed-rung-failures report)]
    (doseq [f failures]
      (is false f))
    (is (empty? failures)
        (str "static rung over " (count (:namespaces report))
             " contracted namespace(s) under " (pr-str model-paths)))))
