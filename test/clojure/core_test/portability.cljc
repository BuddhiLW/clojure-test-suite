(ns clojure.core-test.portability
  #?(:lpy (:import time)
     :jank (:include "chrono"))
  (:require #?(:cljs [cljs.test :as t]
               :default [clojure.test :as t])))

(defmacro when-var-exists [var-sym & body]
  (let [cljs? (some? (:ns &env))
        exists? (boolean #?(:cljd
                            ;; cljd macros resolve against the compiler's own
                            ;; table, `(:nses &env)`: the same walk
                            ;; cljd.test/resolve-symbol makes (it is private).
                            (letfn [(res [sym]
                                      (let [{:keys [libs] :as nses} (:nses &env)
                                            {:keys [mappings aliases] :as cur} (nses (:current-ns nses))]
                                        (when-not (&env sym)
                                          (or (cur sym)
                                              (if-some [v (mappings sym)]
                                                (res v)
                                                (let [sym-ns (namespace sym)
                                                      lib-ns (if (= "clojure.core" sym-ns)
                                                               "cljd.core"
                                                               (some-> (get aliases sym-ns) libs :ns name))]
                                                  (if (some-> lib-ns (not= sym-ns))
                                                    (res (symbol lib-ns (name sym)))
                                                    (some-> sym-ns symbol nses (get (symbol (name sym)))))))))))]
                              (res var-sym))
                            :default
                            (if cljs?
                              ((resolve 'cljs.analyzer.api/resolve) &env var-sym)
                              (resolve var-sym))))]
    (if exists?
      `(do
         ~@body)
      `(println "SKIP -" '~var-sym))))

(defn big-int? [n]
  ;; In CLJS, all numbers are really doubles and integer? and int?
  ;; return true if the fractional part of the double is zero
  #?(:cljs (integer? n)
     :lpy (integer? n)
     :phel (integer? n)
     :jank (cpp/jank.runtime.is_big_integer n)
     ;; cljw's `int?` is true for its BigInt (it answers "integral", not
     ;; "fixed-precision"), so the `:default` test can never see a big int.
     :cljw (= "BigInt" (str (type n)))
     :default
     (and (integer? n)
          (not (int? n)))))

(defn sleep [ms]
  #?(:cljr (System.Threading.Thread/Sleep ms)
     :cljs (js/setTimeout identity ms)
     :clj (Thread/sleep ms)
     :lpy (time/sleep ms)
     :phel (phel.async/delay (/ ms 1000))
     ;; fully qualified: this fn is itself named `sleep`
     :rust (clojure.core/sleep ms)
     :jank (cpp/std.this_thread.sleep_for
            (cpp/std.chrono.milliseconds
             (cpp/jank.runtime.to_int ms)))))

(defn lazy-seq?
  "Is `x` a lazy seq. This requires a dialect-specific test of some
  sort, typically for whether `x` conforms to a specific object type."
  [x]
  #?(:cljs (instance? LazySeq x)
     :lpy (instance? basilisp.lang.seq/LazySeq x)
     :phel (phel.core/lazy-seq? x)
     :jank (cpp/== (.get_type x) cpp/jank.runtime.object_type.lazy_sequence)
     ;; cljw has no host-class syntax; `type` answers with the simple type
     ;; name, and a lazy seq is the only thing that names itself LazySeq.
     :cljw (= "LazySeq" (str (type x)))
     ;; clojurust reports every seq as `List`, so the type is no help. Its
     ;; `realized?` accepts an IPending and throws otherwise, which makes
     ;; "does realized? apply" the available lazy-seq test.
     :rust (and (seq? x)
                (try (do (realized? x) true)
                     (catch Throwable _ false)))
     ;; cljd.core's LazySeq is a Dart class; `instance?` takes no
     ;; class there, the Dart `is?` does.
     :cljd (dart/is? x cljd.core/LazySeq)
     :default (instance? clojure.lang.LazySeq x)))

;; --- Portable exception multimethod. ---

;; Tests that evaluating `form` throws an exception, without asserting the
;; type of exception thrown. Works across all supported Clojure dialects
;; (Clojure JVM, ClojureScript, ClojureCLR, Basilisp, bb, jank, etc.) and
;; integrates with each dialect's native test result reporting API.
;;
;; Prefer this multimethod over manually written reader conditionals, which
;; risk accidentally using dialect-specific symbols as `:default` cases.".

#?(:cljs nil   ; CLJS is special. See below.
   ;; cljd.test has no `assert-expr` multimethod: its `is` only knows a bare
   ;; `thrown?` head. Any other head that resolves to a MACRO falls to the
   ;; generic branch, which asserts the macro's value is truthy, so `p/thrown?`
   ;; is a macro answering "did the body throw".
   :cljd
   (defmacro thrown? [& body]
     `(try
        (do ~@body)
        false
        (catch ~'Object _# true)))

   ;; clojurust has no `assert-expr` multimethod: its `clojure.test/is`
   ;; recognises a `thrown?` head itself (by the symbol's name, so the
   ;; aliased `p/thrown?` is recognised too) and reports the pass/fail
   ;; directly. Nothing to register.
   :rust nil

   :lpy
   (defmethod t/assert-expr 'p/thrown?
     [form msg]
     (let [body (rest form)]
       `(try
          (let [result# (do ~@body)]
            (t/do-report {:type :fail
                          :message ~msg
                          :expected '~form
                          :actual result#}))
          (catch ~'Exception e#
            ;; don't do anything on success
            e#))))

   :jank
   (defmethod t/assert-expr 'p/thrown?
     [msg form]
     (let [body (rest form)]
       `(try
          (let [result# (do ~@body)]
            (t/do-report {:type :fail
                          :message ~msg
                          :expected '~form
                          :actual result#}))
          (catch ~'jank.runtime.object_ref e#
            (t/do-report {:type :pass
                          :message ~msg
                          :expected '~form
                          :actual e#})
            e#)
          (catch ~'std.exception e#
            (let [actual# (~'.what e#)]
              (t/do-report {:type :pass
                            :message ~msg
                            :expected '~form
                            :actual actual#})
              actual#))
          (catch ~'jank.error_ref e#
            (let [actual# (~'.-message (cpp/* e#))]
              (t/do-report {:type :pass
                            :message ~msg
                            :expected '~form
                            :actual actual#})
              actual#)))))

   :cljr
   (defmethod t/assert-expr 'p/thrown?
     [msg form]
     (let [body (rest form)]
       `(try
          (let [result# (do ~@body)]
            (t/do-report {:type :fail
                          :message ~msg
                          :expected '~form
                          :actual result#}))
          (catch ~'Exception e#
            (t/do-report {:type :pass
                          :message ~msg
                          :expected '~form
                          :actual e#})
            e#))))

   :phel
   ;; Phel's `phel.test/report` multimethod dispatches on `:type` and only
   ;; recognises `:pass`/`:failed`/`:error`, not Clojure's `:fail`. Use
   ;; phel-native event keys so the assertion counters update. Phel's
   ;; throwable hierarchy is rooted at PHP's `\Throwable`; the literal is
   ;; constructed via `symbol` because Clojure's reader treats a bare `\T`
   ;; as a character literal (other dialects still read this file).
   (defmethod t/assert-expr 'p/thrown?
     [msg form]
     (let [body (rest form)]
       `(try
          (let [result# (do ~@body)]
            (t/report {:type :failed
                       :message ~msg
                       :expected '~form
                       :actual result#}))
          (catch ~(symbol "\\Throwable") e#
            (t/report {:type :pass
                       :message ~msg
                       :expected '~form
                       :actual e#})
            e#))))

   :default  ; Clojure JVM, Babashka
   (defmethod t/assert-expr 'p/thrown?
     [msg form]
     (let [body (rest form)]
       `(try
          (let [result# (do ~@body)]
            (t/do-report {:type :fail
                          :message ~msg
                          :expected '~form
                          :actual result#}))
          (catch ~'Throwable e#
            (t/do-report {:type :pass
                          :message ~msg
                          :expected '~form
                          :actual e#})
            e#)))))

;; The ClojureScript implementation of the portable exception multimethod is
;; slightly special. The `cljs.test/assert-expr` is not a ClojureScript
;; runtime var. It's a Clojure-side compiler multimethod. It lives in the JVM
;; cljs.test namespace and is invoked during macro expansion of cljs.test/is,
;; not at runtime in the JS environment.
#?(:clj
   (try
     (require '[cljs.test])
     (eval '(defmethod cljs.test/assert-expr 'p/thrown?
              [_menv msg form]
              (let [body (rest form)]
                `(try
                   (let [result# (do ~@body)]
                     (cljs.test/report {:type :fail
                                        :message ~msg
                                        :expected '~form
                                        :actual result#}))
                   (catch ~'js/Error e#
                     (cljs.test/report {:type :pass
                                        :message ~msg
                                        :expected '~form
                                        :actual e#})
                     e#)))))
     (catch Exception  _)))

;; --- Portable exception multimethod. ---
