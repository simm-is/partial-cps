(ns is.simm.partial-cps.sequence
  (:refer-clojure :exclude [await first rest sequence transduce into for])
  (:require [is.simm.partial-cps.async :as async :refer [#?(:clj async) await]]
            #?(:clj [is.simm.partial-cps.ioc :as ioc]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async]]
                            [is.simm.partial-cps.sequence :refer [for for-with]])))

(defprotocol PAsyncSeq
  "Protocol for asynchronous sequences.

  Async sequences are lazy, pull-based sequences of values that may require
  asynchronous computation. Unlike regular Clojure seqs, each step returns
  an async expression that must be awaited."
  (anext [this]
    "Returns async expression yielding [value rest-seq] or nil if sequence is exhausted.

    Example:
      (async
        (when-let [[value rest-seq] (await (anext aseq))]
          (process value)
          (recur rest-seq)))"))

(extend-type nil
  PAsyncSeq
  (anext [_] (async nil)))

;; =============================================================================
;; Extend PAsyncSeq to regular Clojure/ClojureScript sequences
;; This allows mixing sync and async sources in for comprehensions
;; =============================================================================

#?(:clj
   (extend-protocol PAsyncSeq
     ;; Vectors - very common, handle directly
     clojure.lang.IPersistentVector
     (anext [v]
       (fn [resolve _reject]
         (if (seq v)
           (resolve [(clojure.core/first v) (subvec v 1)])
           (resolve nil))))

     ;; Lazy sequences and lists
     clojure.lang.ISeq
     (anext [s]
       (fn [resolve _reject]
         (if-let [s (seq s)]
           (resolve [(clojure.core/first s) (clojure.core/rest s)])
           (resolve nil))))

     ;; General seqable things (sets, maps as seq of entries, etc.)
     clojure.lang.Seqable
     (anext [coll]
       (fn [resolve _reject]
         (if-let [s (seq coll)]
           (resolve [(clojure.core/first s) (clojure.core/rest s)])
           (resolve nil)))))

   :cljs
   (do
     (extend-type cljs.core/PersistentVector
       PAsyncSeq
       (anext [v]
         (fn [resolve _reject]
           (if (seq v)
             (resolve [(clojure.core/first v) (subvec v 1)])
             (resolve nil)))))

     (extend-type cljs.core/List
       PAsyncSeq
       (anext [s]
         (fn [resolve _reject]
           (if-let [s (seq s)]
             (resolve [(clojure.core/first s) (clojure.core/rest s)])
             (resolve nil)))))

     (extend-type cljs.core/EmptyList
       PAsyncSeq
       (anext [s]
         (fn [resolve _reject]
           (resolve nil))))

     (extend-type cljs.core/LazySeq
       PAsyncSeq
       (anext [s]
         (fn [resolve _reject]
           (if-let [s (seq s)]
             (resolve [(clojure.core/first s) (clojure.core/rest s)])
             (resolve nil)))))

     (extend-type cljs.core/IndexedSeq
       PAsyncSeq
       (anext [s]
         (fn [resolve _reject]
           (if-let [s (seq s)]
             (resolve [(clojure.core/first s) (clojure.core/rest s)])
             (resolve nil)))))

     (extend-type cljs.core/Range
       PAsyncSeq
       (anext [r]
         (fn [resolve _reject]
           (if (seq r)
             (resolve [(clojure.core/first r) (clojure.core/rest r)])
             (resolve nil)))))))

(defn first
  "Returns async expression yielding first element, or nil if empty.

  Convenience wrapper around anext."
  [async-seq]
  (async
   (when-let [[v _] (await (anext async-seq))]
     v)))

(defn rest
  "Returns async expression yielding rest of sequence after first element.

  Returns nil if sequence is exhausted.

  Convenience wrapper around anext."
  [async-seq]
  (async
   (when-let [[_ rest-seq] (await (anext async-seq))]
     rest-seq)))

;; Transducers

(defn transduce
  "Transduce over an AsyncSeq eagerly, returning a Promise of the final result."
  [xform f init async-seq]
  (async
   (let [rf (xform f)]
     (loop [result init
            seq async-seq]
       (if seq
         (if-let [[v rest-seq] (await (anext seq))]
           (let [result' (rf result v)]
             (if (reduced? result')
               (rf (unreduced result'))
               (recur result' rest-seq)))
           (rf result))
         (rf result))))))

(defn into
  ([to xform async-seq]
   (transduce xform conj to async-seq))
  ([to async-seq]
   (transduce identity conj to async-seq)))

(defprotocol PTransducerState
  "Protocol for managing shared transducer state"
  (-ensure-buffer! [this idx] "Ensure buffer has element at idx"))

(deftype TransducerState [xf source-ref buffer completed?]
  PTransducerState
  (-ensure-buffer! [_ idx]
    (async
      ;; First check if we already have the element
     (if (> (count @buffer) idx)
       true
        ;; Need more elements or completion already called
       (if @completed?
         false  ; No more elements available
          ;; Try to pull more from source
         (loop []
           (if (> (count @buffer) idx)
              ;; We have enough
             true
              ;; Need to pull from source
             (if-let [source @source-ref]
               (if-let [[v next-source] (await (anext source))]
                  ;; Got value - feed through transducer
                 (let [result (xf nil v)
                       _ (vreset! source-ref next-source)]
                   (if (reduced? result)
                      ;; Reduced - complete and check buffer
                     (do
                       (vreset! completed? true)
                        ;; Call completion
                       (xf (unreduced result))
                        ;; Check if we have element after completion
                       (> (count @buffer) idx))
                      ;; Check if we have more source
                     (if next-source
                        ;; Continue pulling
                       (recur)
                        ;; No more source - need to call completion
                       (do
                         (vreset! completed? true)
                         (xf nil)
                         (> (count @buffer) idx)))))
                  ;; Source exhausted - call completion
                 (do
                   (vreset! completed? true)
                   (vreset! source-ref nil)
                    ;; Call completion - this may add more elements (e.g., partition-all)
                   (xf nil)
                    ;; Check if completion added the element we need
                   (> (count @buffer) idx)))
                ;; No source available
               (do
                 (vreset! completed? true)
                 false)))))))))

(deftype TransducedAsyncSeq [state idx]
  PAsyncSeq
  (anext [_]
    (async
      ;; Ensure buffer has element at idx
     (when (await (-ensure-buffer! state idx))
       [(nth @(.-buffer state) idx)
        (TransducedAsyncSeq. state (inc idx))]))))

(defn sequence
  "Transform an AsyncSeq with a transducer, returning a new lazy AsyncSeq.
   The transducer is applied lazily as elements are consumed.

   Example:
   (sequence (map inc) async-seq)
   (sequence (filter even?) async-seq)
   (sequence (partition-all 3) async-seq)"
  [xform source-seq]
  (when source-seq
    (let [;; Shared state
          buffer (volatile! [])
          source-ref (volatile! source-seq)
          completed? (volatile! false)

          ;; Create the step function that collects into buffer
          step (fn
                 ([] nil)  ; Init (not used)
                 ([result]   ; Completion - just return result
                  result)
                 ([result input]  ; Step - collect input and return result
                  (vswap! buffer conj input)
                  result))

          ;; Apply transducer to step function
          xf (xform step)

          ;; Create the shared state object
          state (->TransducerState xf source-ref buffer completed?)]

      (TransducedAsyncSeq. state 0))))

;; For-async comprehension

(deftype GeneratorSeq [generator-fn current-state]
  PAsyncSeq
  (anext [_]
    (async
     (when current-state
       (when-let [[value next-state] (await (generator-fn current-state))]
         [value (GeneratorSeq. generator-fn next-state)])))))

(defn make-generator-seq
  "Create async sequence from generator function and initial state.

  The generator-fn should take state and return async expression yielding
  [value next-state] or nil.

  The sequence is purely functional - each anext returns a new sequence
  with advanced state, leaving the original unchanged. This allows safe
  sharing and multiple independent consumers."
  [generator-fn initial-state]
  (->GeneratorSeq generator-fn initial-state))

#?(:clj
   (defmacro for
     "Async sequence comprehension. Takes a vector of one or more
  binding-form/collection-expr pairs, each followed by zero or more
  modifiers, and yields a lazy async sequence of evaluations of expr.

  Collections are iterated in a nested fashion, rightmost fastest,
  and nested coll-exprs can refer to bindings created in prior
  binding-forms. Supported modifiers are: :let [binding-form expr ...],
  :while test, :when test.

  Unlike clojure.core/for, allows await in body and returns PAsyncSeq.
  Use namespace aliasing to distinguish from standard for:
    (require '[is.simm.partial-cps.sequence :as seq])
    (seq/for [x xs] (await (fetch x)))

  Examples:
    (for [x [1 2 3]]
      (await (fetch x)))

    (for [x (range 10)
          :when (even? x)
          :let [y (* x 2)]]
      (await (process y)))

    (for [x [1 2 3]
          y [:a :b]]
      [x y])  ; Cross-product: [1 :a] [1 :b] [2 :a] ..."
     [seq-exprs body-expr]
     (assert (even? (clojure.core/count seq-exprs)) "for requires an even number of forms in binding vector")

     (let [;; Group bindings with their modifiers
           ;; e.g., [x xs :when (even? x) y ys] => [[x xs :when (even? x)] [y ys]]
           to-groups (fn [seq-exprs]
                       (clojure.core/reduce
                        (fn [groups [k v]]
                          (if (keyword? k)
                            ;; Modifier - add k and v to last group
                            (let [last-group (clojure.core/peek groups)
                                  rest-groups (clojure.core/pop groups)]
                              (clojure.core/conj rest-groups
                                                 (clojure.core/concat last-group [k v])))
                            ;; New binding - start new group
                            (clojure.core/conj groups [k v])))
                        [] (clojure.core/partition 2 seq-exprs)))

           groups (to-groups seq-exprs)]
       (let [emit-nested
             (fn emit-nested [remaining-groups]
               (let [[bind expr & mod-pairs :as group] (clojure.core/first remaining-groups)
                     next-groups (clojure.core/rest remaining-groups)
                     items-sym (gensym "items__")
                     xs-sym (gensym "xs__")
                     result-sym (gensym "result__")

                     process-modifiers
                     (fn process-modifiers [remaining-mods continuation skip-continuation]
                       (if (seq remaining-mods)
                         (let [k (clojure.core/first remaining-mods)
                               v (clojure.core/second remaining-mods)
                               more-mods (clojure.core/drop 2 remaining-mods)]
                           (case k
                             :let `(let ~v ~(process-modifiers more-mods continuation skip-continuation))
                             :while `(when ~v ~(process-modifiers more-mods continuation skip-continuation))
                             :when `(if ~v
                                      ~(process-modifiers more-mods continuation skip-continuation)
                                      ~skip-continuation)  ; Use skip-continuation when filter fails
                             (throw (ex-info (str "Invalid 'for' keyword: " k) {:keyword k}))))
                         continuation))]

                 (if (seq next-groups)
                   ;; Nested case - have more bindings to process, uses anext for async sources
                   ;; State is [rest-of-outer inner-seq] where rest-of-outer is what remains
                   ;; after consuming the current outer element
                   (let [state-sym (gensym "state__")
                         inner-s-sym (gensym "inner__")
                         v-sym (gensym "v__")
                         next-inner-sym (gensym "next_inner__")
                         rest-xs-sym (gensym "rest_xs__")]
                     `(make-generator-seq
                       (fn [~state-sym]
                         (async
                          (loop [[~xs-sym ~inner-s-sym] ~state-sym]
                            (cond
                              ;; Case 1: Have active inner sequence - consume it
                              ~inner-s-sym
                              (if-let [[~v-sym ~next-inner-sym] (await (anext ~inner-s-sym))]
                                ;; Yield from inner, keep outer position for when inner exhausts
                                [~v-sym [~xs-sym ~next-inner-sym]]
                                ;; Inner exhausted, continue with next outer element
                                ;; xs-sym is already the rest after current outer, just need new inner
                                (recur [~xs-sym nil]))

                              ;; Case 2: No inner - get next element from outer using anext
                              :else
                              (if-let [[~bind ~rest-xs-sym] (await (anext ~xs-sym))]
                                (let [new-inner# ~(process-modifiers mod-pairs
                                                                     ;; Create inner sequence for current binding
                                                                     (emit-nested next-groups)
                                                                     ;; Skip to next outer element if filter fails
                                                                     nil)]
                                  (if new-inner#
                                    ;; Store rest-xs for when inner exhausts
                                    (recur [~rest-xs-sym new-inner#])
                                    ;; Modifier filtered, try next
                                    (recur [~rest-xs-sym nil])))
                                nil)))))
                       [~expr nil]))

                   ;; Base case - single binding (innermost), uses anext for async sources
                   (let [rest-xs-sym (gensym "rest_xs__")]
                     `(make-generator-seq
                       (fn [~items-sym]
                         (async
                          (loop [~xs-sym ~items-sym]
                            (if-let [[~bind ~rest-xs-sym] (await (anext ~xs-sym))]
                              (let [~result-sym ~(process-modifiers mod-pairs
                                                                    (vector body-expr rest-xs-sym)
                                                                    nil)]
                                (if ~result-sym
                                  ~result-sym
                                  (recur ~rest-xs-sym)))
                              nil))))
                       ~expr)))))]

         (emit-nested groups)))))

#?(:clj
   (defmacro for-with
     "Like for, but with additional custom breakpoints and optional dynamic bindings.

  The first argument can be either:
  - A map of breakpoints: {`my-ns/yield `my-ns/yield-handler}
  - An options map with :breakpoints and/or :bindings keys

  Options:
    :breakpoints - Map of qualified symbol to handler var symbol
    :bindings    - Vector of [var-symbol value-expr] pairs to capture and restore
                   during CPS execution. Values are captured at sequence creation.

  The breakpoints are merged with async's await breakpoint, so you can use
  both await and your custom breakpoints in the body.

  Example with breakpoints only (backward compatible):
    (for-with {`my-ns/yield `my-ns/yield-handler}
      [x [1 2 3]]
      (do
        (yield x)
        (await (fetch x))))

  Example with bindings (for preserving dynamic context):
    (for-with {:breakpoints (build-breakpoints)
               :bindings [[*my-context* (current-context)]]}
      [x [1 2 3]]
      (do-something-with-context x))"
     [opts seq-exprs body-expr]
     (assert (even? (clojure.core/count seq-exprs)) "for-with requires an even number of forms in binding vector")

     (let [;; Parse options - support both plain breakpoints map and options map
           has-options-keys? (and (map? opts)
                                  (or (contains? opts :breakpoints)
                                      (contains? opts :bindings)))
           raw-breakpoints (if has-options-keys?
                             (:breakpoints opts {})
                             opts)
           bindings-spec (if has-options-keys?
                           (:bindings opts [])
                           [])

           ;; Evaluate breakpoints if it's not already a map (allows passing function calls)
           evaluated-breakpoints (if (map? raw-breakpoints)
                                   raw-breakpoints
                                   (eval raw-breakpoints))
           ;; Merge user breakpoints with async's breakpoints
           merged-breakpoints (merge async/breakpoints evaluated-breakpoints)

           ;; Generate symbols for captured binding values
           ;; bindings-spec is [[var1 expr1] [var2 expr2] ...]
           binding-captures (mapv (fn [[var-sym _]]
                                    [(gensym (str (name var-sym) "__captured__"))
                                     var-sym])
                                  bindings-spec)
           ;; Build the let bindings to capture values: [captured-sym1 expr1, ...]
           capture-bindings (vec (mapcat (fn [[var-sym expr] [captured-sym _]]
                                           [captured-sym expr])
                                         bindings-spec binding-captures))
           ;; Build the binding vector for restoration: [var1 captured1, var2 captured2, ...]
           restore-bindings (vec (mapcat (fn [[captured-sym var-sym]]
                                           [var-sym captured-sym])
                                         binding-captures))

           ;; Helper to emit CPS-wrapped generator function
           ;; This replicates what the async macro does, but with custom breakpoints
           emit-cps-fn
           (fn [body-form]
             (let [r (gensym "r__")
                   e (gensym "e__")
                   params {:r r :e e :env &env :breakpoints merged-breakpoints}
                   ;; Core CPS execution with trampoline
                   cps-execution `(if async/*in-trampoline*
                                    ~(ioc/invert params body-form)
                                    (binding [async/*in-trampoline* true]
                                      (loop [result# ~(ioc/invert params body-form)]
                                        (if (instance? is.simm.partial_cps.runtime.Thunk result#)
                                          (recur ((.-f ^is.simm.partial_cps.runtime.Thunk result#)))
                                          result#))))
                   ;; Wrap with user bindings if any
                   wrapped-execution (if (seq restore-bindings)
                                       `(binding ~restore-bindings ~cps-execution)
                                       cps-execution)]
               `(fn [~r ~e]
                  (try
                    ~wrapped-execution
                    (catch ~(if (:js-globals &env) :default `Throwable) t# (~e t#))))))

           ;; Group bindings with their modifiers
           to-groups (fn [seq-exprs]
                       (clojure.core/reduce
                        (fn [groups [k v]]
                          (if (keyword? k)
                            (let [last-group (clojure.core/peek groups)
                                  rest-groups (clojure.core/pop groups)]
                              (clojure.core/conj rest-groups
                                                 (clojure.core/concat last-group [k v])))
                            (clojure.core/conj groups [k v])))
                        [] (clojure.core/partition 2 seq-exprs)))

           groups (to-groups seq-exprs)]

       (let [emit-nested
             (fn emit-nested [remaining-groups]
               (let [[bind expr & mod-pairs] (clojure.core/first remaining-groups)
                     next-groups (clojure.core/rest remaining-groups)
                     items-sym (gensym "items__")
                     xs-sym (gensym "xs__")
                     result-sym (gensym "result__")

                     process-modifiers
                     (fn process-modifiers [remaining-mods continuation skip-continuation]
                       (if (seq remaining-mods)
                         (let [k (clojure.core/first remaining-mods)
                               v (clojure.core/second remaining-mods)
                               more-mods (clojure.core/drop 2 remaining-mods)]
                           (case k
                             :let `(let ~v ~(process-modifiers more-mods continuation skip-continuation))
                             :while `(when ~v ~(process-modifiers more-mods continuation skip-continuation))
                             :when `(if ~v
                                      ~(process-modifiers more-mods continuation skip-continuation)
                                      ~skip-continuation)
                             (throw (ex-info (str "Invalid 'for' keyword: " k) {:keyword k}))))
                         continuation))]

                 (if (seq next-groups)
                   ;; Nested case - uses anext for async-compatible iteration
                   ;; State is [rest-of-outer inner-seq] where rest-of-outer is what remains
                   ;; after consuming the current outer element
                   (let [state-sym (gensym "state__")
                         inner-s-sym (gensym "inner__")
                         v-sym (gensym "v__")
                         next-inner-sym (gensym "next_inner__")
                         rest-xs-sym (gensym "rest_xs__")]
                     `(make-generator-seq
                       (fn [~state-sym]
                         ~(emit-cps-fn
                           `(loop [[~xs-sym ~inner-s-sym] ~state-sym]
                              (cond
                                ;; Have inner sequence - consume from it
                                ~inner-s-sym
                                (if-let [[~v-sym ~next-inner-sym] (await (anext ~inner-s-sym))]
                                  ;; Yield from inner, keep outer position for when inner exhausts
                                  [~v-sym [~xs-sym ~next-inner-sym]]
                                  ;; Inner exhausted, continue with next outer element
                                  ;; xs-sym is already the rest after current outer, just need new inner
                                  (recur [~xs-sym nil]))

                                ;; No inner - get next element from outer using anext
                                :else
                                (if-let [[~bind ~rest-xs-sym] (await (anext ~xs-sym))]
                                  (let [new-inner# ~(process-modifiers mod-pairs
                                                                       (emit-nested next-groups)
                                                                       nil)]
                                    (if new-inner#
                                      ;; Store rest-xs for when inner exhausts
                                      (recur [~rest-xs-sym new-inner#])
                                      ;; Modifier filtered, try next
                                      (recur [~rest-xs-sym nil])))
                                  nil)))))
                       [~expr nil]))

                   ;; Base case - single binding (innermost), uses anext
                   (let [rest-xs-sym (gensym "rest_xs__")]
                     `(make-generator-seq
                       (fn [~items-sym]
                         ~(emit-cps-fn
                           `(loop [~xs-sym ~items-sym]
                              (if-let [[~bind ~rest-xs-sym] (await (anext ~xs-sym))]
                                (let [~result-sym ~(process-modifiers mod-pairs
                                                                      (vector body-expr rest-xs-sym)
                                                                      nil)]
                                  (if ~result-sym
                                    ~result-sym
                                    (recur ~rest-xs-sym)))
                                nil))))
                       ~expr)))))]

         ;; Wrap with capture bindings if any
         (if (seq capture-bindings)
           `(let ~capture-bindings
              ~(emit-nested groups))
           (emit-nested groups))))))

