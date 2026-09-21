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

;; -----------------------------------------------------------------------------
;; Which trampoline is running here?
;;
;; Host-specific, so it lives here: `async.cljc` is also interpreted inside
;; sandboxes (SCI with interop locked) that inject this namespace natively.
;;
;; `async/*in-trampoline*` alone cannot answer the question. It is a dynamic
;; var, and dynamic bindings travel: `future`, agents, `bound-fn` and
;; core.async go blocks restore the bindings of whoever created them, on
;; another thread or LATER ON THE SAME ONE, when the trampoline that bound the
;; flag has long unwound. A continuation that believes such a flag returns its
;; Thunk to a caller that is no trampoline, and is lost. So every activation
;; has its own token; the var carries it (and travels), this thread-local
;; carries it too (and does not), and a trampoline is running here exactly
;; when the two agree.
;; -----------------------------------------------------------------------------

(def ^:private active-trampoline
  #?(:clj (ThreadLocal.) :cljs (volatile! nil)))

(defn enter-trampoline!
  "Mark a new trampoline activation on this thread. Returns [token previous]:
   bind `async/*in-trampoline*` to `token`, pass `previous` to
   `leave-trampoline!` in a `finally`."
  []
  (let [token #?(:clj (Object.) :cljs (js-obj))
        previous #?(:clj (.get ^ThreadLocal active-trampoline) :cljs @active-trampoline)]
    #?(:clj (.set ^ThreadLocal active-trampoline token)
       :cljs (vreset! active-trampoline token))
    [token previous]))

(defn leave-trampoline!
  [previous]
  #?(:clj (if (nil? previous)
            (.remove ^ThreadLocal active-trampoline)
            (.set ^ThreadLocal active-trampoline previous))
     :cljs (vreset! active-trampoline previous))
  nil)

(defn owns-trampoline?
  "Whether `token`, the current value of `async/*in-trampoline*`, is the
   trampoline activation that is running on this thread right now."
  [token]
  (and (some? token)
       (identical? token #?(:clj (.get ^ThreadLocal active-trampoline)
                            :cljs @active-trampoline))))
