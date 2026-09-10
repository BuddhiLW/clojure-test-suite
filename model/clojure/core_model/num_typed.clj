(ns ^:typed.clojure clojure.core-model.num-typed
  "Static signatures for `num`, for dialects whose host is strongly typed.
  Checked with `typed.clojure/check-ns-clj`; nothing here is executed."
  (:require [typed.clojure :as t]))

(t/defalias NumOutcome
  "A total observation of one call: the boxed number, nil, or a marker naming
  the class that was thrown."
  (t/U nil t/Num (t/HMap :mandatory {:throws t/Str})))

(t/ann reference-num [t/Num :-> t/Num])
(defn reference-num
  "`num` over the numeric tower. The static domain is narrower than the run-time
  one, which also admits nil and returns it unchanged."
  [x]
  (num x))

(t/ann probe-num [t/Num :-> NumOutcome])
(defn probe-num
  "The same call observed totally, so a refusal is a value the snapshot can hold.
  Only a non-number reaches that branch."
  [x]
  (try (num x) (catch Throwable e {:throws (.getName (class e))})))

(t/ann nil-passes-through? Boolean)
(def nil-passes-through?
  "Whether nil is returned rather than refused. True: it is the one input `num`
  neither converts nor rejects, and the family's only silent nil hole."
  true)

(t/ann converts-on-a-dynamic-call? Boolean)
(def converts-on-a-dynamic-call?
  "Whether a boxed argument is converted. False: reached dynamically `num` hands
  back the very same object, so it is the identity on numbers."
  false)

(t/ann boxes-a-primitive-argument? Boolean)
(def boxes-a-primitive-argument?
  "Whether a primitive argument is boxed to the widest type. True for the long
  and double overloads, which is `num`'s entire purpose; a primitive float has
  no overload and boxes as itself."
  true)
