(ns is.simm.partial-cps.async
  (:refer-clojure :exclude [await])
  (:require [is.simm.partial-cps.runtime :as runtime]
            #?(:clj [is.simm.partial-cps.ioc :as ioc :refer [has-breakpoints? invert]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async async+sync]])))

(defn await
  "Awaits the asynchronous execution of continuation-passing style function
   async-cb, applying it to args and two extra callback functions: resolve and
   raise. cps-fn is expected to eventually either call resolve with the result,
   call raise with the exception or just throw in the calling thread. The
   return value of cps-fn is ignored. Effectively returns the value passed to
   resolve or throws the exception passed to raise (or thrown) but does not
   block the calling tread.

   Must be called in an asynchronous function. Note that any nested functions
   defined with fn, letfn, reify or deftype are considered outside of
   asynchronous scope."
  [async-cb]
  (throw (ex-info "await called outside of asynchronous scope" {:async-cb async-cb})))

(def ^:dynamic *in-trampoline* false)

(defn invoke-continuation
  "Invoke a CPS continuation, handling Thunk returns via trampoline.

  This is the universal continuation invocation wrapper that ensures
  Thunks are properly trampolined, preventing stack overflow in loops.

  Usage:
    (invoke-continuation resolve-fn value)
    (invoke-continuation reject-fn error)

  If the continuation returns a Thunk, it will be executed via trampoline.
  This is essential for loop/recur constructs in CPS-transformed code."
  [cont-fn & args]
  (let [result (apply cont-fn args)]
    (if (runtime/thunk? result)
      (if *in-trampoline*
        result  ; Already in trampoline, return Thunk
        ;; Not in trampoline, execute it
        (binding [*in-trampoline* true]
          (loop [r result]
            (if (runtime/thunk? r)
              (recur (runtime/force-thunk r))
              r))))
      result)))

(defn await-handler
  "Provides effect handler code for await.

  Receives ctx map containing:
  - :env - macro expansion environment (&env)
  - :r - success continuation
  - :e - error continuation
  - :breakpoints - registered breakpoints
  Plus any additional macro-specific context fields."
  [ctx r e]
  (fn [args]
    (assert (= (count args) 1) (str "Expected 1 argument, got " args))
    (let [env (:env ctx)]
      ;; `safe-r#` is a single-reference, non-self-recursive resume callback, emitted
      ;; once PER await. Bind it as a `let`-scoped ANONYMOUS fn rather than `letfn`:
      ;; cljs's analyzer runs a second analysis pass over *named* fns, which compounds
      ;; multiplicatively across the per-await nesting (O(2^N) compile). Anonymous fns
      ;; skip that pass → linear compile. (Semantically identical here.)
      `(let [safe-r# (fn [v#]
                       (try
                         (if *in-trampoline*
                           (~r v#)
                           (binding [*in-trampoline* true]
                             (loop [result# (~r v#)]
                               (if (runtime/thunk? result#)
                                  ;; If continuation returns a thunk, trampoline it
                                 (recur (runtime/force-thunk result#))
                                 result#))))
                         (catch ~(if (:js-globals env) :default `Throwable) t# (~e t#))))]
         (~(first args) safe-r# ~e)))))

(def ^:no-doc breakpoints
  {`await `await-handler
   ;; ClojureScript >= 1.12 added a core `await` MACRO (native JS async/await interop).
   ;; It is auto-referred into every ns and, being a macro, wins over a `:refer`'d
   ;; partial-cps `await` in call position. We register it as the SAME CPS breakpoint so
   ;; that inside a partial-cps `async`, a bare `(await x)` is CPS-transformed whether it
   ;; resolves to partial-cps's `await` or `cljs.core/await` — no per-ns
   ;; `(:refer-clojure :exclude [await])` needed. (On the JVM `cljs.core/await` never
   ;; resolves, so this key is simply inert there.)
   'cljs.core/await `await-handler})

(defn async-expr?
  "Is x an asynchronous expression produced by the `async` macro (or `all`)?
   Async expressions ARE plain 2-arity fns (fn? returns true — duck-typed
   consumers keep working); this predicate is the reliable discriminator for
   contracts where a fn-as-VALUE must be distinguished from an async return
   (e.g. a query engine awaiting user functions that may themselves be
   async). cljs: a property stamped on the emitted fn at creation.
   clj: metadata (async exprs are rarely hot on the JVM)."
  [x]
  #?(:clj  (boolean (and (fn? x) (:is.simm.partial-cps/async (meta x))))
     :cljs (boolean (and (fn? x) (true? (.-partial_cps_async_expr x))))))

#?(:clj
   (defmacro async
     "Defines a function that takes a successful and exceptional continuation,
   and runs the body, suspending execution whenever any of the breakpoints (await) is
   encountered, and eventually calling one of the continuations with the
   result.

   A call of the form (breakpoint args..) is forwarded to the corresponding handler
   (handler succ exc args..), which is expected to eventually call either succ
   with the value or exc with exception to substitute the original call result
   and resuming the execution.

   The returned fn is marked so `async-expr?` can identify it."
     [& body]
     (let [r (gensym) e (gensym)
           params {:r r :e e :env &env :breakpoints breakpoints}
           form (cons 'do body)
           fn-form `(fn [~r ~e]
                      (try
                        (if *in-trampoline*
                          ~(invert params form)
                          (binding [*in-trampoline* true]
                            (loop [result# ~(invert params form)]
                              (if (runtime/thunk? result#)
                                ;; If continuation returns a thunk, trampoline it
                                (recur (runtime/force-thunk result#))
                                result#))))
                        (catch ~(if (:js-globals &env) :default `Throwable) t# (~e t#))))]
       (if (:js-globals &env)
         `(let [f# ~fn-form]
            (set! (.-partial_cps_async_expr f#) true)
            f#)
         `(with-meta ~fn-form {:is.simm.partial-cps/async true})))))

#?(:clj
   (defmacro async+sync
     "One body, two execution modes — the dual-mode foundation.

   Emits `(if sync? <direct-sync-form> (async body…))` on BOTH platforms —
   a runtime dispatch amortized at whatever frequency the enclosing
   function is called. `sync?` is honored everywhere: the CPS arm (with
   its multi-shot, copyable continuations) is as available on the JVM as
   on cljs.

   When `sync?` is the LITERAL true or false the untaken arm is pruned at
   compile time — zero overhead and no dead code. Consumers with a
   platform-static mode (e.g. an engine that is always synchronous on the
   JVM) should express that policy in a wrapper macro passing the literal,
   not rely on the platform to imply it.

   The strip resolves symbols with the SAME resolution the async transform
   uses (an aliased or fully-qualified await strips; a breakpoint that is
   also a macro, e.g. cljs.core/await, is matched before expansion), so no
   suspension point can survive into the sync arm and throw at runtime.
   The walk is env-threaded through binding forms, fn literals are opaque
   in BOTH arms (a bare await inside one is a compile-time error — wrap the
   fn body in its own (async …) if it should produce an async expression),
   and locals named await/async inside the body are rejected at compile
   time (the async arm cannot resolve the shadowing)."
     [sync? & body]
     (let [form (cons 'do body)
           ctx {:breakpoints breakpoints :env &env}]
       (cond
         (true? sync?) (ioc/strip-breakpoints form ctx)
         (false? sync?) `(async ~@body)
         :else `(if ~sync?
                  ~(ioc/strip-breakpoints form ctx)
                  (async ~@body))))))

(defn all
  "Async expression resolving to a vector of the results of `exprs` — each
   an async expression — invoked IN ORDER, each driven to its first true
   suspension (or completion) before the next launches. All-warm inputs
   therefore complete synchronously and the whole `all` resolves before it
   returns, preserving the trampoline's sync-completion property. Rejects
   with the FIRST error; other branches keep running to completion (no
   cancellation — partial-cps has no cancellation primitive). Each expr is
   invoked exactly once. Empty input resolves to []."
  [exprs]
  (let [exprs (vec exprs)
        n (count exprs)
        f (fn [resolve reject]
            (if (zero? n)
              (invoke-continuation resolve [])
              (let [results #?(:clj (object-array n) :cljs (make-array n))
                    remaining (atom n)
                    rejected? (atom false)]
                (loop [i 0]
                  (when (< i n)
                    (let [t ((nth exprs i)
                             (fn [v]
                               (aset results i v)
                               (when (and (zero? (swap! remaining dec))
                                          (not @rejected?))
                                 (invoke-continuation resolve (vec results))))
                             (fn [err]
                               (when (compare-and-set! rejected? false true)
                                 (invoke-continuation reject err))))]
                      ;; Drive this branch's Thunk chain NOW: when invoked
                      ;; under an enclosing trampoline the branch RETURNS a
                      ;; Thunk instead of forcing it (only one Thunk can
                      ;; propagate to the enclosing loop), so `all` must run
                      ;; each branch to its first real suspension itself.
                      (loop [r t]
                        (when (runtime/thunk? r)
                          (recur (runtime/force-thunk r)))))
                    (recur (inc i))))
                nil)))]
    #?(:cljs (set! (.-partial_cps_async_expr f) true))
    #?(:clj (with-meta f {:is.simm.partial-cps/async true})
       :cljs f)))
