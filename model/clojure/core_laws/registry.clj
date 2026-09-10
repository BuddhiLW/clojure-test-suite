(ns clojure.core-laws.registry
  "The universally quantified `clojure.core` laws, as first-class data.

  One entry names: the subject vars, the quantified statement, the domain it
  is quantified over, the malli signature it lifts from, a predicate that
  decides one instance on the JVM reference, fixed regression instances,
  the measured witnesses that fall OUTSIDE the domain, and the kernel
  obligation that discharges it.

  `clojure.core-laws.properties` reads these entries as test.check properties;
  `clojure.core-laws.kernel` reads the SAME entries as ansatz CIC obligations.
  This namespace requires neither and must stay free of both."
  (:require [clojure.test.check.generators :as gen]
            [malli.core :as m]))

;; ---------------------------------------------------------------- schemas

(def Domain
  "The set a law is quantified over: a malli membership schema plus the
  generator that samples it."
  [:map {:closed true}
   [:domain/id :keyword]
   [:domain/doc :string]
   [:domain/malli :any]
   [:domain/gen [:fn gen/generator?]]])

(def Witness
  "A measured value outside a law's domain, with what the reference actually
  returned there. `:witness/observed` is the recorded JVM result; a thrown
  class is recorded as its symbol."
  [:map {:closed true}
   [:witness/doc :string]
   [:witness/expr :any]
   [:witness/observed :any]])

(def Obligation
  "The kernel-side reading of a law. `:obligation/prop` is an ansatz surface
  proposition over the model definitions named in `:obligation/needs`;
  `:obligation/lemmas` are the obligations it rests on, discharged first; and
  `:obligation/refutation` is a FALSE variant the kernel must reject under a
  comparable tactic script."
  [:schema
   {:registry
    {::obligation
     [:map {:closed true}
      [:obligation/name :symbol]
      [:obligation/params [:vector :any]]
      [:obligation/prop :any]
      [:obligation/tactics [:vector :any]]
      [:obligation/needs [:vector :symbol]]
      [:obligation/covers :string]
      [:obligation/lemmas {:optional true} [:vector [:ref ::obligation]]]
      [:obligation/refutation
       [:map {:closed true}
        [:refutation/prop :any]
        [:refutation/tactics [:vector :any]]]]]}}
   ::obligation])

(def Binder
  "One quantified variable and the domain it ranges over."
  [:map {:closed true}
   [:binder/sym :symbol]
   [:binder/domain Domain]])

(def Refinement
  "A second quantification of the same law that is universal where the headline
  statement is not. Carries its own binders, which need not match the law's."
  [:map {:closed true}
   [:refinement/id :keyword]
   [:refinement/doc :string]
   [:refinement/statement :any]
   [:refinement/domain Domain]
   [:refinement/binders [:vector Binder]]
   [:refinement/holds? fn?]])

(def Law
  "A law entry. `:law/binders` gives one domain per quantified variable, in the
  order `:law/holds?` takes them; `:law/domain` names the principal domain the
  law is quantified over."
  [:map {:closed true}
   [:law/id :keyword]
   [:law/subject [:vector :symbol]]
   [:law/statement :any]
   [:law/domain Domain]
   [:law/binders [:vector Binder]]
   [:law/lifts-from :any]
   [:law/holds? fn?]
   [:law/instances [:vector [:vector :any]]]
   [:law/outside-domain [:vector Witness]]
   [:law/refinement {:optional true} Refinement]
   [:law/obligation Obligation]])

(def Registry [:vector Law])

;; ------------------------------------------------------------- generators

(def ^:private element
  (gen/one-of [gen/small-integer gen/keyword gen/string-alphanumeric gen/boolean]))

(def ^:private sequential-coll
  (gen/one-of
   [(gen/vector element)
    (gen/fmap #(apply list %) (gen/vector element))
    (gen/fmap seq (gen/not-empty (gen/vector element)))
    (gen/fmap #(into clojure.lang.PersistentQueue/EMPTY %) (gen/vector element))]))

(def ^:private sequential-coll-or-nil
  (gen/one-of [sequential-coll (gen/return nil)]))

(def ^:private nth-able-coll
  (gen/one-of [sequential-coll-or-nil
               (gen/fmap #(apply str %) (gen/vector gen/char-alpha))]))

(def ^:private any-seqable
  (gen/one-of [nth-able-coll
               (gen/fmap set (gen/vector element))
               (gen/fmap #(into {} (map vector % %)) (gen/vector gen/small-integer))]))

(def ^:private exact-integer
  (gen/one-of [(gen/fmap bigint gen/small-integer)
               (gen/fmap bigint gen/large-integer)
               (gen/fmap #(*' (bigint %) 1000000000000000000000N) gen/large-integer)]))

(defn- sequential-coll?
  [c]
  (or (sequential? c) (instance? clojure.lang.PersistentQueue c)))

(defn- sequential-coll-or-nil? [c] (or (nil? c) (sequential-coll? c)))

(defn- nth-able?
  [c]
  (or (sequential-coll-or-nil? c) (string? c)))

;; ----------------------------------------------------------------- domains

(def elements
  "Values a collection may hold. Doubles are excluded: `=` is not reflexive on
  NaN, which would make every element-wise comparison in these laws fail for
  reasons that have nothing to do with the law."
  {:domain/id :element
   :domain/doc "Integers, keywords, strings and booleans."
   :domain/malli [:or :int :keyword :string :boolean]
   :domain/gen element})

(def exact-integers
  {:domain/id :exact-integer
   :domain/doc
   "Exact integers of unbounded precision (clojure.lang.BigInt). `+` on these
   operands is total: it never overflows, so both groupings are always
   defined. Restricting the same domain to `long` makes `+` PARTIAL, and
   definedness is not grouping-invariant (see the witnesses)."
   :domain/malli [:fn #(or (instance? clojure.lang.BigInt %)
                           (instance? java.math.BigInteger %))]
   :domain/gen exact-integer})

(def fixed-width-longs
  {:domain/id :long
   :domain/doc "Fixed-width 64-bit integers, where `+` throws on overflow."
   :domain/malli :int
   :domain/gen gen/large-integer})

(def sequential-collections
  "The domain `reverse` round-trips as a collection."
  {:domain/id :sequential-collection
   :domain/doc
   "Proper sequential collections: vectors, lists, seqs and queues. nil is
   excluded — `reverse` returns a seq, and the empty seq is not `=` nil. Sets
   and maps are excluded because two reversals return their iteration order as
   a seq, which is never `=` the container."
   :domain/malli [:fn sequential-coll?]
   :domain/gen sequential-coll})

(def conjable-sequentials
  "The domain where `conj` always adds a cell."
  {:domain/id :conjable-sequential
   :domain/doc
   "Sequential collections and nil — `(conj nil x)` is a one-element list. Sets
   and maps are excluded: conjoining a value already present leaves count
   unchanged."
   :domain/malli [:fn sequential-coll-or-nil?]
   :domain/gen sequential-coll-or-nil})

(def nth-able-seqables
  {:domain/id :nth-able-seqable
   :domain/doc
   "Seqables `distinct` accepts. Its 1-arity destructures the collection with
   `[f :as xs]`, which calls `nth`, so sets and maps are outside the domain —
   they throw rather than dedupe."
   :domain/malli [:fn nth-able?]
   :domain/gen nth-able-coll})

(def seqables
  {:domain/id :seqable
   :domain/doc "Everything `seq` accepts, sets, maps, strings and nil included."
   :domain/malli [:fn seqable?]
   :domain/gen any-seqable})

;; ------------------------------------------------------------------- laws

(defn- sig
  "The malli function signature a law lifts its kernel type signature from: the
  argument domains' own membership schemas, and the return schema."
  [arg-domains ret]
  [:=> (into [:cat] (map :domain/malli) arg-domains) ret])

(defn- defined-sum
  "The value of `(+ a b)`, or ::undefined when the width overflows."
  [a b]
  (try (+ a b) (catch ArithmeticException _ ::undefined)))

(defn- long-assoc-holds?
  [a b c]
  (let [l (let [ab (defined-sum a b)] (if (= ::undefined ab) ::undefined (defined-sum ab c)))
        r (let [bc (defined-sum b c)] (if (= ::undefined bc) ::undefined (defined-sum a bc)))]
    (or (= ::undefined l) (= ::undefined r) (= l r))))

(def plus-associative
  {:law/id :plus-associative
   :law/subject '[clojure.core/+]
   :law/statement '(= (+ (+ a b) c) (+ a (+ b c)))
   :law/domain exact-integers
   :law/binders [{:binder/sym 'a :binder/domain exact-integers}
                 {:binder/sym 'b :binder/domain exact-integers}
                 {:binder/sym 'c :binder/domain exact-integers}]
   :law/lifts-from (sig [exact-integers exact-integers] (:domain/malli exact-integers))
   :law/holds? (fn [a b c] (= (+ (+ a b) c) (+ a (+ b c))))
   :law/instances [[0N 0N 0N]
                   [1N 2N 3N]
                   [-7N 7N 1N]
                   [9223372036854775807N 1N -1N]
                   [12345678901234567890N -98765432109876543210N 5N]]
   :law/outside-domain
   [{:witness/doc "Doubles: the law fails outright — addition is not associative
     in floating point."
     :witness/expr '[(+ (+ 1.0 1e16) -1e16) (+ 1.0 (+ 1e16 -1e16))]
     :witness/observed [0.0 1.0]}
    {:witness/doc "Doubles again, at ordinary magnitudes."
     :witness/expr '[(+ (+ 0.1 0.2) 0.3) (+ 0.1 (+ 0.2 0.3))]
     :witness/observed [0.6000000000000001 0.6]}
    {:witness/doc "Longs: definedness is not grouping-invariant — the left
     grouping overflows and throws while the right one returns."
     :witness/expr '[(+ (+ Long/MAX_VALUE 1) -1) (+ Long/MAX_VALUE (+ 1 -1))]
     :witness/observed '[java.lang.ArithmeticException 9223372036854775807]}
    {:witness/doc "A single double contaminates an otherwise exact triple."
     :witness/expr '[(+ (+ 1 1e16) -1e16) (+ 1 (+ 1e16 -1e16))]
     :witness/observed [0.0 1.0]}]
   :law/refinement
   {:refinement/id :plus-associative-long-where-defined
    :refinement/doc
    "Over `long`, associativity holds wherever BOTH groupings are defined. It
    does not make `+` associative there: one grouping can throw while the other
    returns, which the third witness records."
    :refinement/statement
    '(=> (and (defined? (+ (+ a b) c)) (defined? (+ a (+ b c))))
         (= (+ (+ a b) c) (+ a (+ b c))))
    :refinement/domain fixed-width-longs
    :refinement/binders [{:binder/sym 'a :binder/domain fixed-width-longs}
                         {:binder/sym 'b :binder/domain fixed-width-longs}
                         {:binder/sym 'c :binder/domain fixed-width-longs}]
    :refinement/holds? long-assoc-holds?}
   :law/obligation
   {:obligation/name 'cljplus_assoc
    :obligation/params '[^Int a ^Int b ^Int c]
    :obligation/prop '(= Int (+ (+ a b) c) (+ a (+ b c)))
    :obligation/tactics '[(exact (Int.add_assoc a b c))]
    :obligation/needs '[]
    :obligation/covers
    "Int — the kernel's unbounded signed exact integers. Coextensive with the
    JVM domain; the double and fixed-width halves of the numeric tower are
    deliberately not modelled, because the law is false there."
    :obligation/refutation
    {:refutation/prop '(= Int (+ (+ a b) c) (+ a (+ b (+ c 1))))
     :refutation/tactics '[(exact (Int.add_assoc a b c))]}}})

(def count-conj
  {:law/id :count-conj
   :law/subject '[clojure.core/count clojure.core/conj]
   :law/statement '(= (count (conj c x)) (inc (count c)))
   :law/domain conjable-sequentials
   :law/binders [{:binder/sym 'c :binder/domain conjable-sequentials}
                 {:binder/sym 'x :binder/domain elements}]
   :law/lifts-from (sig [conjable-sequentials elements] :int)
   :law/holds? (fn [c x] (= (count (conj c x)) (inc (count c))))
   :law/instances [[[] 1]
                   [[1 2] 3]
                   [[1 1] 1]
                   ['(1 2) 3]
                   [nil 1]]
   :law/outside-domain
   [{:witness/doc "A set: conj of a value already present leaves count unchanged."
     :witness/expr '[(count (conj #{1 2} 2)) (inc (count #{1 2}))]
     :witness/observed [2 3]}
    {:witness/doc "A sorted set behaves the same."
     :witness/expr '[(count (conj (sorted-set 1 2) 2)) (inc (count (sorted-set 1 2)))]
     :witness/observed [2 3]}
    {:witness/doc "A map: conj of an entry whose key is present replaces it."
     :witness/expr '[(count (conj {:a 1} [:a 9])) (inc (count {:a 1}))]
     :witness/observed [1 2]}]
   :law/obligation
   {:obligation/name 'cljcount_conj
    :obligation/params '[^{:- (List Nat)} xs ^Nat x]
    :obligation/prop '(= Nat (cljcount (cljconj xs x)) (+ (cljcount xs) 1))
    :obligation/tactics '[(simp [cljcount cljconj]) (all_goals (try (omega)))]
    :obligation/needs '[cljcount cljconj]
    :obligation/covers
    "(List Nat) — cons-lists, the inductive core of the sequential domain.
    Queues and vectors share `conj`'s add-a-cell contract but are not modelled
    as separate inductives."
    :obligation/refutation
    {:refutation/prop '(= Nat (cljcount (cljconj xs x)) (cljcount xs))
     :refutation/tactics '[(simp [cljcount cljconj]) (all_goals (try (omega)))]}}})

(def distinct-idempotent
  {:law/id :distinct-idempotent
   :law/subject '[clojure.core/distinct]
   :law/statement '(= (distinct (distinct c)) (distinct c))
   :law/domain nth-able-seqables
   :law/binders [{:binder/sym 'c :binder/domain nth-able-seqables}]
   :law/lifts-from (sig [nth-able-seqables] [:fn seqable?])
   :law/holds? (fn [c] (= (distinct (distinct c)) (distinct c)))
   :law/instances [[[]]
                   [[1 1 2 3 2]]
                   [[nil nil 1]]
                   ["aabbc"]
                   [[[1] [1] #{1}]]
                   [nil]]
   :law/outside-domain
   [{:witness/doc "A set: `distinct` destructures its argument with `[f :as xs]`,
     which calls `nth`. It throws rather than returning the set's elements."
     :witness/expr '(distinct #{1 2 3})
     :witness/observed 'java.lang.UnsupportedOperationException}
    {:witness/doc "A map throws for the same reason."
     :witness/expr '(distinct {:a 1})
     :witness/observed 'java.lang.UnsupportedOperationException}
    {:witness/doc "NaN is never deduped: the seen-set test is `=`, which is not
     reflexive on NaN. Idempotence survives — a second pass drops nothing
     either — but `distinct` is not a duplicate-free postcondition."
     :witness/expr '(distinct [##NaN ##NaN])
     :witness/observed '(##NaN ##NaN)}
    {:witness/doc "0.0 and -0.0 ARE deduped, since `=` identifies them."
     :witness/expr '(distinct [0.0 -0.0 0])
     :witness/observed '(0.0 0)}]
   :law/obligation
   {:obligation/name 'cljdistinct_idem
    :obligation/params '[^{:- (List Nat)} xs]
    :obligation/prop '(= (List Nat) (cljdistinct (cljdistinct xs)) (cljdistinct xs))
    :obligation/tactics '[(exact (cljdaux_idem xs (List.nil Nat)))]
    :obligation/needs '[cljmember cljdaux cljdistinct]
    :obligation/covers
    "(List Nat) with decidable equality on Nat. The JVM domain's `=` is not
    reflexive on NaN; kernel equality on Nat is, so the NaN witness is outside
    what the obligation models."
    :obligation/lemmas
    [{:obligation/name 'cljdaux_idem
      :obligation/params '[^{:- (List Nat)} xs ^{:- (List Nat)} seen]
      :obligation/prop '(= (List Nat) (cljdaux (cljdaux xs seen) seen) (cljdaux xs seen))
      :obligation/tactics '[(induction xs generalizing seen)
                            (all_goals (try (simp [cljdaux])))
                            (all_goals (try (split)))
                            (all_goals (try (simp_all [cljdaux cljmember])))]
      :obligation/needs '[cljmember cljdaux]
      :obligation/covers "Generalized over the seen-set accumulator; the law is
      its instance at the empty accumulator."
      :obligation/refutation
      {:refutation/prop '(= (List Nat) (cljdaux xs seen) xs)
       :refutation/tactics '[(induction xs generalizing seen)
                             (all_goals (try (simp [cljdaux])))
                             (all_goals (try (split)))
                             (all_goals (try (simp_all [cljdaux cljmember])))]}}]
    :obligation/refutation
    {:refutation/prop '(= (List Nat) (cljdistinct xs) xs)
     :refutation/tactics '[(exact (cljdaux_idem xs (List.nil Nat)))]}}})

(def reverse-involutive
  {:law/id :reverse-involutive
   :law/subject '[clojure.core/reverse]
   :law/statement '(= (reverse (reverse c)) c)
   :law/domain sequential-collections
   :law/binders [{:binder/sym 'c :binder/domain sequential-collections}]
   :law/lifts-from (sig [sequential-collections] [:fn seqable?])
   :law/holds? (fn [c] (= (reverse (reverse c)) c))
   :law/instances [[[]]
                   [[1 2 3]]
                   ['(1 2 3)]
                   [(range 3)]]
   :law/outside-domain
   [{:witness/doc "nil: `reverse` returns a seq, and the empty seq is not `=` nil."
     :witness/expr '[(reverse (reverse nil)) nil]
     :witness/observed '[() nil]}
    {:witness/doc "A set: two reversals return the set's iteration order as a
     seq, which is never `=` the set."
     :witness/expr '(= (reverse (reverse #{1 2 3})) #{1 2 3})
     :witness/observed false}
    {:witness/doc "A map, for the same reason."
     :witness/expr '(= (reverse (reverse {:a 1 :b 2})) {:a 1 :b 2})
     :witness/observed false}
    {:witness/doc "A string: two reversals return a seq of characters."
     :witness/expr '(= (reverse (reverse "abc")) "abc")
     :witness/observed false}]
   :law/refinement
   {:refinement/id :reverse-involutive-on-seqs
    :refinement/doc
    "At the seq level the involution IS universal over every seqable — each
    witness above satisfies it. `reverse` is an involution on the seq of a
    collection, not on the collection."
    :refinement/statement '(= (seq (reverse (reverse c))) (seq c))
    :refinement/domain seqables
    :refinement/binders [{:binder/sym 'c :binder/domain seqables}]
    :refinement/holds? (fn [c] (= (seq (reverse (reverse c))) (seq c)))}
   :law/obligation
   {:obligation/name 'cljreverse_reverse
    :obligation/params '[^{:- (List Nat)} xs]
    :obligation/prop '(= (List Nat) (cljreverse (cljreverse xs)) xs)
    :obligation/tactics '[(induction xs) (rfl)
                          (simp_all [cljreverse cljreverse_append cljappend])]
    :obligation/needs '[cljappend cljreverse]
    :obligation/covers
    "(List Nat) — cons-lists. The seq-level refinement is not modelled: the
    kernel has no seq abstraction over non-sequential containers."
    :obligation/lemmas
    [{:obligation/name 'cljappend_nil
      :obligation/params '[^{:- (List Nat)} xs]
      :obligation/prop '(= (List Nat) (cljappend xs nil) xs)
      :obligation/tactics '[(induction xs) (rfl) (simp_all [cljappend])]
      :obligation/needs '[cljappend]
      :obligation/covers "(List Nat)."
      :obligation/refutation
      {:refutation/prop '(= (List Nat) (cljappend xs nil) nil)
       :refutation/tactics '[(induction xs) (rfl) (simp_all [cljappend])]}}
     {:obligation/name 'cljappend_assoc
      :obligation/params '[^{:- (List Nat)} xs ^{:- (List Nat)} ys ^{:- (List Nat)} zs]
      :obligation/prop '(= (List Nat) (cljappend (cljappend xs ys) zs)
                           (cljappend xs (cljappend ys zs)))
      :obligation/tactics '[(induction xs) (rfl) (simp_all [cljappend])]
      :obligation/needs '[cljappend]
      :obligation/covers "(List Nat)."
      :obligation/refutation
      {:refutation/prop '(= (List Nat) (cljappend (cljappend xs ys) zs)
                            (cljappend zs (cljappend ys xs)))
       :refutation/tactics '[(induction xs) (rfl) (simp_all [cljappend])]}}
     {:obligation/name 'cljreverse_append
      :obligation/params '[^{:- (List Nat)} xs ^{:- (List Nat)} ys]
      :obligation/prop '(= (List Nat) (cljreverse (cljappend xs ys))
                           (cljappend (cljreverse ys) (cljreverse xs)))
      :obligation/tactics '[(induction xs)
                            (all_goals (try (simp_all [cljreverse cljappend
                                                       cljappend_nil cljappend_assoc])))]
      :obligation/needs '[cljappend cljreverse]
      :obligation/covers "(List Nat)."
      :obligation/refutation
      {:refutation/prop '(= (List Nat) (cljreverse (cljappend xs ys))
                            (cljappend (cljreverse xs) (cljreverse ys)))
       :refutation/tactics '[(induction xs)
                             (all_goals (try (simp_all [cljreverse cljappend
                                                        cljappend_nil cljappend_assoc])))]}}]
    :obligation/refutation
    {:refutation/prop '(= (List Nat) (cljreverse xs) xs)
     :refutation/tactics '[(induction xs) (rfl)
                           (simp_all [cljreverse cljreverse_append cljappend])]}}})

(def laws
  "The law registry. Ordered; `:law/id` is the key."
  [plus-associative count-conj distinct-idempotent reverse-involutive])

(defn law
  "The law registered under `id`, or nil."
  [id]
  (first (filter #(= id (:law/id %)) laws)))

(m/=> law [:=> [:cat :keyword] [:maybe Law]])

;; ------------------------------------------------------- kernel model defs

(def model-defs
  "Kernel-side models of the `clojure.core` functions the laws quantify over,
  in dependency order. `:def/form` is an `ansatz.core/defn` form; nothing here
  is evaluated by this namespace."
  [{:def/name 'cljcount
    :def/models 'clojure.core/count
    :def/form '(a/defn ^Nat cljcount [^{:- (List Nat)} xs]
                 (match xs (List Nat) Nat
                   (nil 0)
                   (cons [h t] (+ 1 (cljcount t)))))}
   {:def/name 'cljconj
    :def/models 'clojure.core/conj
    :def/form '(a/defn ^{:- (List Nat)} cljconj [^{:- (List Nat)} xs ^Nat x]
                 (cons x xs))}
   {:def/name 'cljappend
    :def/models 'clojure.core/concat
    :def/form '(a/defn ^{:- (List Nat)} cljappend [^{:- (List Nat)} xs ^{:- (List Nat)} ys]
                 (match xs (List Nat) (List Nat)
                   (nil ys)
                   (cons [h t] (cons h (cljappend t ys)))))}
   {:def/name 'cljreverse
    :def/models 'clojure.core/reverse
    :def/form '(a/defn ^{:- (List Nat)} cljreverse [^{:- (List Nat)} xs]
                 (match xs (List Nat) (List Nat)
                   (nil xs)
                   (cons [h t] (cljappend (cljreverse t) (cons h nil)))))}
   {:def/name 'cljmember
    :def/models 'clojure.core/contains?
    :def/form '(a/defn ^Bool cljmember [^Nat x ^{:- (List Nat)} s]
                 (match s (List Nat) Bool
                   (nil false)
                   (cons [h t] (if (== x h) true (cljmember x t)))))}
   {:def/name 'cljdaux
    :def/models 'clojure.core/distinct
    :def/form '(a/defn ^{:- (List Nat)} cljdaux [^{:- (List Nat)} xs ^{:- (List Nat)} seen]
                 (match xs (List Nat) (List Nat)
                   (nil xs)
                   (cons [h t] (if (cljmember h seen)
                                 (cljdaux t seen)
                                 (cons h (cljdaux t (cons h seen)))))))}
   {:def/name 'cljdistinct
    :def/models 'clojure.core/distinct
    :def/form '(a/defn ^{:- (List Nat)} cljdistinct [^{:- (List Nat)} xs]
                 (cljdaux xs (List.nil Nat)))}])

(defn obligations
  "Every obligation the registry carries, lemmas before the law they support."
  []
  (vec (mapcat (fn [{:keys [law/obligation]}]
                 (concat (:obligation/lemmas obligation) [obligation]))
               laws)))
