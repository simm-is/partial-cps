(ns is.simm.partial-cps.sequence
  (:refer-clojure :exclude [await first rest sequence transduce into])
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

