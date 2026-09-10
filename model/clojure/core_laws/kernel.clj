(ns clojure.core-laws.kernel
  "The same law registry read as ansatz CIC obligations.

  A run installs the registry's model definitions in a booted kernel env,
  discharges every obligation (lemmas first), checks that the kernel REFUSES
  each obligation's false variant under a comparable tactic script, then
  exports the decls this session added and re-imports them — which
  kernel-re-checks every proof term. The snapshot is trusted only at
  `:all-verified true`.

  Needs the `:laws` alias. ansatz is resolved at call time, so the namespace
  loads and reports `:kernel/absent` without a prover on the classpath."
  (:require [clojure.core-laws.registry :as reg]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def default-snapshot-path
  "Where a run writes its proof snapshot. Under the gitignored target/ root."
  "target/laws/core-laws.fressian")

(def ^:private kernel-nses
  '[ansatz.core hive-ansatz.adapters.ansatz hive-ansatz.persistence])

(defn kernel-available?
  "Whether the ansatz kernel and the hive-ansatz adapter both load here."
  []
  (try (apply require kernel-nses) true
       (catch Throwable _ false)))

(defn- rs [sym] (requiring-resolve sym))

(defn- kernel-ns [] (the-ns 'clojure.core-laws.kernel))

(defn boot!
  "Load the kernel Init store and return the names it already carries — the
  exclusion set an export uses to keep the snapshot to this session's decls."
  []
  ((rs 'ansatz.core/load-init!))
  (set (map str (.allConstants ^Object ((rs 'ansatz.core/env))))))

(defn install-model!
  "Evaluate every registry model definition, in registry order."
  []
  (binding [*ns* (kernel-ns)]
    (when-not ('a (ns-aliases *ns*))
      (alias 'a (the-ns 'ansatz.core)))
    (mapv (fn [{:keys [def/name def/form]}] (eval form) name) reg/model-defs)))

(defn discharge!
  "Prove and install one obligation. Returns the install status: `:admitted`
  when this run proved it, `:present` when it was already in the env."
  [obligation]
  ((rs 'ansatz.core/install-theorem!)
   (:obligation/name obligation)
   (:obligation/params obligation)
   (:obligation/prop obligation)
   (:obligation/tactics obligation)))

(defn refused?
  "Whether the kernel REFUSES the obligation's false variant. The load-bearing
  soundness check: the script that closed the true proposition must not close
  the false one."
  [obligation]
  (let [{:keys [refutation/prop refutation/tactics]} (:obligation/refutation obligation)]
    (try ((rs 'ansatz.core/prove-law) (:obligation/params obligation) prop tactics)
         false
         (catch Throwable _ true))))

(defn export-and-reverify!
  "Snapshot every decl added since `base-names`, boot a fresh env, and import
  the snapshot there — which kernel-re-checks every theorem it carries.
  Returns the hive-ansatz ImportReport."
  [path base-names]
  (let [live-env (rs 'hive-ansatz.adapters.ansatz/live-env)
        codec ((rs 'hive-ansatz.adapters.ansatz/fressian-codec))
        export! (rs 'hive-ansatz.persistence/export!)
        import! (rs 'hive-ansatz.persistence/import!)]
    (io/make-parents path)
    (let [env (live-env)
          exported (export! env env codec path {:exclude base-names})]
      ((rs 'ansatz.core/load-init!))
      (let [fresh (live-env)]
        (assoc (import! fresh fresh codec path) :exported exported)))))

(defn unverifiable
  "Names among `added` that ansatz tags `:thm` but whose stored value the kernel
  checker does not accept, checked in the env that BUILT them — so a name here
  is not a snapshot round-trip failure."
  [added]
  (let [e ((rs 'hive-ansatz.adapters.ansatz/live-env))
        theorem? (rs 'hive-ansatz.ports/theorem?)
        verify (rs 'hive-ansatz.ports/verify)
        by-name (into {} (map (fn [d] [(str (.name ^Object d)) d]))
                      (.allConstants ^Object ((rs 'ansatz.core/env))))]
    (->> added
         (keep (fn [n] (when-let [d (by-name n)]
                         (when (and (theorem? e d) (not (boolean (verify e d)))) n))))
         sort vec)))

(defn discharge-all!
  "Discharge the whole registry in a kernel env and re-verify the snapshot.
  Returns a report; `{:kernel/absent true}` when no prover is on the classpath.

  Two verdicts, because they answer different questions. `:all-verified` is
  hive-ansatz's own, over EVERY `:thm`-tagged decl the snapshot carries — which
  includes the SizeOf / Prod.Lex prelude that ansatz installs to compile a
  well-founded recursion, and whose decls its checker does not accept even in
  the env that built them (`:unverifiable-prelude`). `:laws-verified` is over
  the registry's own obligations."
  ([] (discharge-all! default-snapshot-path))
  ([path]
   (if-not (kernel-available?)
     {:kernel/absent true}
     (let [base (boot!)
           installed (install-model!)
           obligations (reg/obligations)
           wanted (set (map (comp str :obligation/name) obligations))
           statuses (reduce (fn [m o] (assoc m (:obligation/name o) (discharge! o)))
                            {} obligations)
           refused (reduce (fn [m o] (assoc m (:obligation/name o) (refused? o)))
                           {} obligations)
           added (remove base (map str (.allConstants ^Object ((rs 'ansatz.core/env)))))
           prelude (unverifiable added)
           report (export-and-reverify! path base)
           results (:results report)]
       {:model installed
        :obligations statuses
        :refused refused
        :snapshot path
        :import report
        :all-verified (:all-verified report)
        :unverifiable-prelude prelude
        :laws-verified (and (every? #(contains? results %) wanted)
                            (every? #(true? (get results %)) wanted))}))))

(defn- summarize [r]
  (if (:kernel/absent r)
    "kernel absent — run under the :laws alias"
    (format (str "%d obligations %s | %d refutations refused %s | "
                 "laws re-verified %s | snapshot all-verified %s "
                 "(%d decls, %d thm-tagged, %d unverifiable prelude: %s)")
            (count (:obligations r)) (pr-str (frequencies (vals (:obligations r))))
            (count (:refused r)) (pr-str (frequencies (vals (:refused r))))
            (pr-str (:laws-verified r))
            (pr-str (:all-verified r))
            (count (get-in r [:import :added]))
            (count (get-in r [:import :results]))
            (count (:unverifiable-prelude r))
            (pr-str (:unverifiable-prelude r)))))

(defn -main
  "Run the kernel tier and print a one-line summary. Exits non-zero unless every
  obligation was discharged, every refutation refused, and every registry
  obligation re-verified in a fresh env after import."
  [& _]
  (let [r (discharge-all!)
        ok (and (not (:kernel/absent r))
                (every? #{:admitted :present} (vals (:obligations r)))
                (every? true? (vals (:refused r)))
                (true? (:laws-verified r)))]
    (println (summarize r))
    (when-not ok (println (pr-str (dissoc r :import))))
    (shutdown-agents)
    (System/exit (if ok 0 1))))

;; ------------------------------------------------------------------ tests

(def ^:private once (delay (discharge-all!)))

(deftest kernel-discharges-every-obligation
  (let [r @once]
    (if (:kernel/absent r)
      (is false "ansatz is not on the classpath — run under the :laws alias")
      (do
        (testing "every obligation is proved and installed"
          (doseq [[nm status] (:obligations r)]
            (is (#{:admitted :present} status) (str nm " => " status))))
        (testing "every false variant is refused"
          (doseq [[nm ref?] (:refused r)]
            (is (true? ref?) (str nm " proved its own refutation"))))
        (testing "the snapshot carries a proof term for every obligation"
          (is (= (set (map (comp str :obligation/name) (reg/obligations)))
                 (set (filter (set (map (comp str :obligation/name) (reg/obligations)))
                              (keys (get-in r [:import :results])))))))
        (testing "every obligation re-verifies in a fresh env after import"
          (is (true? (:laws-verified r))
              (pr-str (select-keys (get-in r [:import :results])
                                   (map (comp str :obligation/name) (reg/obligations))))))
        (testing "nothing the registry produced is unverifiable — only the ansatz
                  prelude a well-founded recursion drags in may be"
          (is (empty? (filter (set (map (comp str :obligation/name) (reg/obligations)))
                              (:unverifiable-prelude r)))
              (pr-str (:unverifiable-prelude r))))))))
