(ns is.simm.partial-cps.sequence
  (:refer-clojure :exclude [await first rest sequence transduce into for])
  (:require [is.simm.partial-cps.async :refer [async await]]))

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
  (assert (vector? seq-exprs) "for requires a vector for its binding")
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

    (if (= 1 (clojure.core/count groups))
      ;; Single binding - can have modifiers
      (let [[bind expr & mod-pairs] (clojure.core/first groups)
            items-sym (gensym "items__")
            xs-sym (gensym "xs__")
            result-sym (gensym "result__")
            process-modifiers
            (fn process-modifiers [remaining-mods continuation]
              (if (seq remaining-mods)
                (let [k (clojure.core/first remaining-mods)
                      v (clojure.core/second remaining-mods)
                      more-mods (clojure.core/drop 2 remaining-mods)]
                  (case k
                    :let `(let ~v ~(process-modifiers more-mods continuation))
                    :while `(when ~v ~(process-modifiers more-mods continuation))
                    :when `(if ~v
                             ~(process-modifiers more-mods continuation)
                             nil)  ; Skip this iteration
                    (throw (ex-info (str "Invalid 'for' keyword: " k) {:keyword k}))))
                continuation))]
        `(make-generator-seq
          (fn [~items-sym]
            (async
              (loop [~xs-sym ~items-sym]
                (when-let [~xs-sym (seq ~xs-sym)]
                  (let [~bind (clojure.core/first ~xs-sym)
                        ~result-sym ~(process-modifiers mod-pairs `[~body-expr (clojure.core/rest ~xs-sym)])]
                    (if ~result-sym
                      ~result-sym
                      (recur (clojure.core/rest ~xs-sym))))))))  ; :when filtered, try next
          ~expr))

      ;; Multiple bindings - implement recursive nested iteration
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
                  ;; Nested case - have more bindings to process
                  (let [state-sym (gensym "state__")
                        inner-s-sym (gensym "inner__")
                        v-sym (gensym "v__")
                        next-inner-sym (gensym "next_inner__")]
                    `(make-generator-seq
                      (fn [~state-sym]
                        (async
                          (loop [[~xs-sym ~inner-s-sym] ~state-sym]
                            (cond
                              ;; Case 1: Have active inner sequence - consume it
                              ~inner-s-sym
                              (if-let [[~v-sym ~next-inner-sym] (await (anext ~inner-s-sym))]
                                ;; Yield from inner, keep outer position
                                [~v-sym [~xs-sym ~next-inner-sym]]
                                ;; Inner exhausted, advance outer
                                (recur [(clojure.core/rest ~xs-sym) nil]))

                              ;; Case 2: Need to create new inner sequence for current outer element
                              (seq ~xs-sym)
                              (let [~bind (clojure.core/first ~xs-sym)
                                    new-inner# ~(process-modifiers mod-pairs
                                                  ;; Create inner sequence for current binding
                                                  (emit-nested next-groups)
                                                  ;; Skip to next outer element if filter fails
                                                  nil)]
                                (if new-inner#
                                  (recur [~xs-sym new-inner#])
                                  ;; Modifier filtered, try next outer
                                  (recur [(clojure.core/rest ~xs-sym) nil])))

                              ;; Case 3: All exhausted
                              :else nil))))
                      [~expr nil]))

                  ;; Base case - single binding (innermost)
                  `(make-generator-seq
                    (fn [~items-sym]
                      (async
                        (loop [~xs-sym ~items-sym]
                          (when-let [~xs-sym (seq ~xs-sym)]
                            (let [~bind (clojure.core/first ~xs-sym)
                                  ~result-sym ~(process-modifiers mod-pairs
                                                 `[~body-expr (clojure.core/rest ~xs-sym)]
                                                 nil)]
                              (if ~result-sym
                                ~result-sym
                                (recur (clojure.core/rest ~xs-sym))))))))
                    ~expr))))]

        (emit-nested groups)))))

