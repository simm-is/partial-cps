(ns is.simm.partial-cps.async
  (:refer-clojure :exclude [await])
  (:require [is.simm.partial-cps.runtime :as runtime]
            #?(:clj [is.simm.partial-cps.ioc :refer [has-breakpoints? invert]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async]])))

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
    (if (instance? is.simm.partial_cps.runtime.Thunk result)
      (if *in-trampoline*
        result  ; Already in trampoline, return Thunk
        ;; Not in trampoline, execute it
        (binding [*in-trampoline* true]
          (loop [r result]
            (if (instance? is.simm.partial_cps.runtime.Thunk r)
              (recur ((.-f ^is.simm.partial_cps.runtime.Thunk r)))
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
      `(letfn [(safe-r# [v#]
                 (try
                   (if *in-trampoline*
                     (~r v#)
                     (binding [*in-trampoline* true]
                       (loop [result# (~r v#)]
                         (if (instance? is.simm.partial_cps.runtime.Thunk result#)
                                  ;; If continuation returns a thunk, trampoline it
                           (recur ((.-f ^is.simm.partial_cps.runtime.Thunk result#)))
                           result#))))
                   (catch ~(if (:js-globals env) :default `Throwable) t# (~e t#))))]
         (~(first args) safe-r# ~e)))))

(def ^:no-doc breakpoints
  {`await `await-handler})

#?(:clj
   (defmacro async
     "Defines a function that takes a successful and exceptional continuation,
   and runs the body, suspending execution whenever any of the breakpoints (await) is
   encountered, and eventually calling one of the continuations with the
   result.

   A call of the form (breakpoint args..) is forwarded to the corresponding handler
   (handler succ exc args..), which is expected to eventually call either succ
   with the value or exc with exception to substitute the original call result
   and resuming the execution."
     [& body]
     (let [r (gensym) e (gensym)
           params {:r r :e e :env &env :breakpoints breakpoints}
           form (cons 'do body)]
       `(fn [~r ~e]
          (try
            (if *in-trampoline*
              ~(invert params form)
              (binding [*in-trampoline* true]
                (loop [result# ~(invert params form)]
                  (if (instance? is.simm.partial_cps.runtime.Thunk result#)
                    ;; If continuation returns a thunk, trampoline it
                    (recur ((.-f ^is.simm.partial_cps.runtime.Thunk result#)))
                    result#))))
            (catch ~(if (:js-globals &env) :default `Throwable) t# (~e t#)))))))
