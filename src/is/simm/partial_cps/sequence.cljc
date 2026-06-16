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
             (resolve nil)))))

     ;; Subvec: the PersistentVector anext returns `(subvec v 1)` as the rest,
     ;; which is a Subvec — so consuming any multi-element vector source hits
     ;; this type on the second step. `(subvec subvec 1)` stays a Subvec, so
     ;; this is closed (no further unextended rest type).
     (extend-type cljs.core/Subvec
       PAsyncSeq
       (anext [v]
         (fn [resolve _reject]
           (if (seq v)
             (resolve [(clojure.core/first v) (subvec v 1)])
             (resolve nil)))))

     ;; IntegerRange: `(range n)` with integer bounds produces an IntegerRange
     ;; (not Range); `(rest …)` yields a further IntegerRange.
     (extend-type cljs.core/IntegerRange
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

;; Empty FIFO queue for staging transducer outputs (PersistentQueue).
(def ^:private empty-output-queue
  #?(:clj clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(defn- pull-one!
  "Advance the shared transducer state by exactly enough source pulls to yield
  one more transduced output, and return (async) that value — or ::done when
  the stream is exhausted.

  The transducer's step appends outputs to `pending` (a FIFO queue), so a
  single source pull may yield zero (filter), one (map), or several (mapcat /
  a completion flush) outputs; surplus outputs stay queued and are returned by
  later calls without re-pulling the source. `pull-one!` is the ONLY place the
  source is advanced — nodes call it in strict realization order, so the shared
  state is consumed monotonically and never re-read."
  [{:keys [xf source-ref pending completed?]}]
  (async
   (loop []
     (cond
       (seq @pending)
       (let [v (peek @pending)]
         (vswap! pending pop)
         v)

       @completed?
       ::done

       :else
       (if-let [source @source-ref]
         (if-let [[v next-source] (await (anext source))]
           (let [result (xf nil v)]
             ;; `next-source` may be nil here (this was the source's last
             ;; element); we still recur to drain `pending`, and the
             ;; nil-source-ref branch below runs completion afterwards.
             (vreset! source-ref next-source)
             (when (reduced? result)
               ;; A reducing transducer (e.g. take) emitted its final value
               ;; AND signalled stop on the same element: run completion (may
               ;; flush, e.g. partition-all) and mark done. The emitted value
               ;; is already queued in `pending`, so the loop picks it up.
               (xf (unreduced result))
               (vreset! completed? true))
             (recur))
           ;; anext returned nil: source exhausted. Run completion (may flush,
           ;; e.g. partition-all) then mark done.
           (do
             (vreset! source-ref nil)
             (xf nil)
             (vreset! completed? true)
             (recur)))
         ;; source-ref is nil but not yet completed — exhausted via a
         ;; nil `next-source` above; run the completion flush now.
         (do
           (xf nil)
           (vreset! completed? true)
           (recur)))))))

;; A node in a lazy, transduced async sequence. `cell` memoizes this node's
;; single step — `::unrealized`, or the realized `[value next-node]` / `nil`.
;; Re-reading a node (e.g. `first` then `rest` on the same node) returns the
;; memoized result without re-pulling the source, so `anext` is idempotent.
;;
;; Crucially, a node references only its successor, never its predecessor: once
;; the consumer advances past a node and drops it, that node and its value
;; become collectable. Holding a mid-stream node pins only the realized chain
;; *forward* from it — earlier elements are released. (Holding the HEAD retains
;; the whole realized chain, exactly like clojure.core lazy-seqs.) This is the
;; fix for the prior shared-monotonic-buffer design, where every node shared
;; one ever-growing buffer and thus pinned every element ever consumed.
(deftype LazyTransducedSeq [state cell]
  PAsyncSeq
  (anext [_]
    (async
     (let [c @cell]
       (if (not= ::unrealized c)
         c
         (let [v (await (pull-one! state))
               result (if (= ::done v)
                        nil
                        [v (LazyTransducedSeq. state (volatile! ::unrealized))])]
           (vreset! cell result)
           result))))))

(defn sequence
  "Transform an AsyncSeq with a transducer, returning a new lazy AsyncSeq.
   The transducer is applied lazily as elements are consumed.

   Example:
   (sequence (map inc) async-seq)
   (sequence (filter even?) async-seq)
   (sequence (partition-all 3) async-seq)"
  [xform source-seq]
  (when source-seq
    (let [;; Shared, monotonically-consumed transducer state.
          pending (volatile! empty-output-queue)
          source-ref (volatile! source-seq)
          completed? (volatile! false)

          ;; Step appends outputs to the pending FIFO; pull-one! drains it.
          step (fn
                 ([] nil)
                 ([result] result)
                 ([result input]
                  (vswap! pending conj input)
                  result))

          xf (xform step)
          state {:xf xf
                 :source-ref source-ref
                 :pending pending
                 :completed? completed?}]

      (->LazyTransducedSeq state (volatile! ::unrealized)))))

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
                                        (if (is.simm.partial-cps.runtime/thunk? result#)
                                          (recur (is.simm.partial-cps.runtime/force-thunk result#))
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

