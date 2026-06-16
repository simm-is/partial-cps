(ns is.simm.partial-cps.runtime
  (:refer-clojure :exclude [bound-fn]))

(defn ^:no-doc bound-fn
  [f]
  #?(:clj
     (let [bound-frame (clojure.lang.Var/getThreadBindingFrame)]
       (fn [& args]
         (let [call-site-frame (clojure.lang.Var/getThreadBindingFrame)]
           (clojure.lang.Var/resetThreadBindingFrame bound-frame)
           (try
             (apply f args)
             (finally
               (clojure.lang.Var/resetThreadBindingFrame call-site-frame))))))
     ;; no dynamic binding support for async code in cljs (same for core.async)
     :cljs identity))

(deftype Thunk [f])

(defn ->thunk
  "Create a thunk for trampolining"
  [f]
  (Thunk. f))

(defn thunk?
  "Whether `x` is a trampoline Thunk.

   Emitted CPS code (the `async` macro + await-handler) MUST call this fn rather
   than inline `(instance? Thunk x)`: a fn is referenced by an ns-qualified VAR
   (`is.simm.partial-cps.runtime/thunk?`), which resolves under any macro
   re-scoping. The dotted TYPE symbol `is.simm.partial_cps.runtime.Thunk` is a
   property-access form on cljs that some macros (e.g. `cljs.test/async`)
   re-scope to `cljs.test.is.simm…` → `undefined` at runtime. spindel hit exactly
   this and worked around it locally (seq/core.cljc); centralizing it here lets
   that workaround go away."
  [x]
  (instance? Thunk x))

(defn force-thunk
  "Run a Thunk's deferred fn — one trampoline bounce. Companion to `thunk?`;
   keeps the `Thunk` type reference inside this namespace (see `thunk?`)."
  [t]
  ((.-f ^Thunk t)))
