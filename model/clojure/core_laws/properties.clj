(ns clojure.core-laws.properties
  "The law registry read as test.check properties over the JVM reference.

  Three obligations, all machine-checked without a prover: every law holds on
  every sample of its own domain; every fixed instance is in that domain and
  satisfies the law; and every recorded witness still produces exactly the
  value the registry records for it — so a witness that stopped being a
  counterexample fails the suite instead of quietly rotting.

  Kernel-free: this namespace runs under the `:model` alias as well as
  `:laws`."
  (:require [clojure.core-laws.registry :as reg]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]))

(def ^:dynamic *num-tests*
  "Samples drawn per law per run."
  500)

(defn binder-gens
  "The generators of `law`'s quantified variables, in argument order."
  [law]
  (mapv (comp :domain/gen :binder/domain) (:law/binders law)))

(m/=> binder-gens [:=> [:cat :any] [:vector :any]])

(defn check-law
  "Quick-check `law`'s predicate over `n` samples of its binder domains."
  [law n]
  (tc/quick-check n (prop/for-all* (binder-gens law) (:law/holds? law))))

(defn check-refinement
  "Quick-check `law`'s refinement over its own binder domains. nil when the law
  carries no refinement."
  [law n]
  (when-let [{:keys [refinement/binders refinement/holds?]} (:law/refinement law)]
    (tc/quick-check n (prop/for-all* (mapv (comp :domain/gen :binder/domain) binders)
                                     holds?))))

(defn- eval-1
  "Evaluate one expression, recording a thrown class as its symbol and forcing
  any lazy result."
  [expr]
  (try (let [v (eval expr)] (if (seq? v) (doall v) v))
       (catch Throwable t (symbol (.getName (class t))))))

(defn witness-actual
  "What the reference does today for `expr`. A vector expression is evaluated
  element-wise, so one element throwing does not mask the others."
  [expr]
  (if (vector? expr) (mapv eval-1 expr) (eval-1 expr)))

(defn witness-drift
  "nil when `witness` still observes what the registry records, otherwise
  {:expr .. :recorded .. :actual ..}. Compared through `pr-str`: `=` is not
  reflexive on NaN, and one witness is about NaN."
  [witness]
  (let [actual (witness-actual (:witness/expr witness))]
    (when-not (= (pr-str (:witness/observed witness)) (pr-str actual))
      {:expr (:witness/expr witness)
       :recorded (:witness/observed witness)
       :actual actual})))

(defn in-domain?
  "Whether `v` is a member of `domain`, by the domain's malli schema."
  [domain v]
  (m/validate (:domain/malli domain) v))

(m/=> in-domain? [:=> [:cat :any :any] :boolean])

;; ------------------------------------------------------------------ tests

(deftest registry-conforms
  (testing "every entry validates against the Law schema"
    (doseq [law reg/laws]
      (is (nil? (m/explain reg/Law law)) (str (:law/id law)))))
  (testing "the registry is a vector of laws with distinct ids"
    (is (nil? (m/explain reg/Registry reg/laws)))
    (is (= (count reg/laws) (count (set (map :law/id reg/laws))))))
  (testing "the signature a law lifts from is a real malli schema, and every
            argument type in it is one of the law's own binder domains — the
            signature types the SUBJECT, the binders quantify the LAW, so the
            two need not have the same arity"
    (doseq [law reg/laws]
      (let [[_ cat _] (:law/lifts-from law)
            domains (set (map (comp :domain/malli :binder/domain) (:law/binders law)))]
        (is (some? (m/schema (:law/lifts-from law))) (str (:law/id law)))
        (is (every? domains (rest cat)) (str (:law/id law))))))
  (testing "every obligation names model definitions the registry defines"
    (let [defined (set (map :def/name reg/model-defs))]
      (doseq [o (reg/obligations)]
        (is (every? defined (:obligation/needs o)) (str (:obligation/name o))))))
  (testing "obligation names are distinct"
    (let [ns- (map :obligation/name (reg/obligations))]
      (is (= (count ns-) (count (set ns-)))))))

(deftest laws-hold-on-their-domain
  (doseq [law reg/laws]
    (testing (str (:law/id law) " over " (:domain/id (:law/domain law)))
      (let [r (check-law law *num-tests*)]
        (is (:pass? r) (pr-str (select-keys r [:shrunk :fail :seed])))))))

(deftest refinements-hold-on-their-wider-domain
  (doseq [law reg/laws
          :let [r (check-refinement law *num-tests*)]
          :when r]
    (testing (str (:refinement/id (:law/refinement law)))
      (is (:pass? r) (pr-str (select-keys r [:shrunk :fail :seed]))))))

(deftest fixed-instances-are-in-domain-and-hold
  (doseq [law reg/laws]
    (testing (str (:law/id law))
      (doseq [args (:law/instances law)]
        (is (= (count args) (count (:law/binders law)))
            (str "arity of " (pr-str args)))
        (doseq [[arg binder] (map vector args (:law/binders law))]
          (is (in-domain? (:binder/domain binder) arg)
              (str (:binder/sym binder) " = " (pr-str arg))))
        (is (apply (:law/holds? law) args) (pr-str args))))))

(deftest witnesses-still-fall-outside-the-domain
  (doseq [law reg/laws]
    (testing (str (:law/id law))
      (doseq [w (:law/outside-domain law)]
        (is (nil? (witness-drift w)) (pr-str (witness-drift w)))))))

(deftest generators-sample-their-own-domain
  (doseq [law reg/laws
          binder (:law/binders law)]
    (testing (str (:law/id law) " / " (:binder/sym binder))
      (let [d (:binder/domain binder)
            r (tc/quick-check 200 (prop/for-all* [(:domain/gen d)] #(in-domain? d %)))]
        (is (:pass? r) (pr-str (select-keys r [:shrunk :seed])))))))
