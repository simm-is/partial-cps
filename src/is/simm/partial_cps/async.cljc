(ns is.simm.partial-cps.async
  (:refer-clojure :exclude [await])
  (:require [is.simm.partial-cps.runtime :as runtime]
            #?(:clj [riddley.walk :refer [macroexpand-all]])
            #?(:clj [is.simm.partial-cps.ioc :refer [has-breakpoints? invert]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async doseq-async dotimes-async]])))

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
           expanded (try
                      (macroexpand-all (cons 'do body))
                      (catch Exception e
                        (throw e)))]
       `(fn [~r ~e]
          (try
            (if *in-trampoline*
              ~(invert params expanded)
              (binding [*in-trampoline* true]
                (loop [result# ~(invert params expanded)]
                  (if (instance? is.simm.partial_cps.runtime.Thunk result#)
                    ;; If continuation returns a thunk, trampoline it
                    (recur ((.-f ^is.simm.partial_cps.runtime.Thunk result#)))
                    result#))))
            (catch ~(if (:js-globals &env) :default `Throwable) t# (~e t#)))))))

;; experimental macros

#?(:clj
   (defmacro doseq-async
     "Asynchronous doseq that properly handles await operations in bindings or body.
   Processes items sequentially, waiting for each async operation to complete
   before moving to the next item. Must be used within an async block."
     [bindings & body]
     (let [binding-pairs (partition 2 bindings)
           ctx {:breakpoints breakpoints :env &env}]
       (letfn [(expand-doseq [pairs]
                 (if (seq pairs)
                   (let [[sym coll] (first pairs)
                         rest-pairs (rest pairs)]
                     `(let [coll# (seq ~coll)]
                        (loop [items# coll#]
                          (when items#
                            (let [~sym (first items#)]
                              ~(if (seq rest-pairs)
                                 (expand-doseq rest-pairs)
                                 `(do ~@body))
                              (recur (next items#)))))))
                   `(do ~@body)))]

         ;; Check if bindings contain await operations
         (if (has-breakpoints? bindings ctx)
           ;; Bindings contain await - expand to nested let/doseq structure that CPS will transform
           (let [[syncs [[sym asn] & others]] (split-with #(not (has-breakpoints? (second %) ctx)) binding-pairs)]
             (if asn
               ;; First async binding found - await the collection, then continue processing
               (let [async-coll-sym (gensym "async-coll")]
                 (if (seq others)
                   ;; More bindings to handle - use recursive doseq-async call
                   `(let [~async-coll-sym ~asn]
                      ~(if (seq syncs)
                         `(doseq [~@(mapcat identity syncs) ~sym ~async-coll-sym]
                            (doseq-async [~@(mapcat identity others)]
                              ~@body))
                         `(doseq [~sym ~async-coll-sym]
                            (doseq-async [~@(mapcat identity others)]
                              ~@body))))
                   ;; No more bindings - check if body has async operations
                   (if (has-breakpoints? `(do ~@body) ctx)
                     ;; Body has async - expand to loop/recur structure  
                     `(let [~async-coll-sym ~asn]
                        ~(expand-doseq (concat syncs [[sym async-coll-sym]])))
                     ;; No more async operations - use regular doseq
                     `(let [~async-coll-sym ~asn]
                        ~(if (seq syncs)
                           `(doseq [~@(mapcat identity syncs) ~sym ~async-coll-sym]
                              ~@body)
                           `(doseq [~sym ~async-coll-sym]
                              ~@body))))))
               ;; No async bindings after all - check body
               (if (has-breakpoints? `(do ~@body) ctx)
                 (expand-doseq binding-pairs)
                 `(doseq ~bindings ~@body))))
           ;; No async in bindings - check body
           (if (has-breakpoints? `(do ~@body) ctx)
             ;; Body contains await - convert to explicit loop/recur that will be CPS-transformed
             (expand-doseq binding-pairs)
             ;; No async operations - use regular doseq
             `(doseq ~bindings ~@body)))))))

#?(:clj
   (defmacro dotimes-async
     "Asynchronous dotimes that properly handles await operations in the body.
   Processes iterations sequentially, waiting for each async operation to complete
   before moving to the next iteration."
     [[sym init-form] & body]
     (let [ctx {:breakpoints breakpoints :env &env}]
       (if (has-breakpoints? `(do ~@body) ctx)
         ;; Body contains await - generate loop that will be CPS-transformed
         `(let [max# ~init-form]
            (loop [~sym 0]
              (when (< ~sym max#)
                ~@body
                (recur (inc ~sym)))))
         ;; No await in body, pass through unchanged
         `(dotimes [~sym ~init-form] ~@body)))))
