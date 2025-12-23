(ns is.simm.partial-cps.for-async-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.partial-cps.async :refer [async await *in-trampoline*]]
            [is.simm.partial-cps.sequence :as seq]))

(defn async-callback
  "Wraps a continuation for re-entry from async context.
   Sets *in-trampoline* to false to ensure fresh trampoline establishment."
  [cont]
  (fn [& args]
    (binding [*in-trampoline* false]
      (apply cont args))))

(defn blocking-test
  "Helper to run async test with blocking"
  [async-fn timeout-ms]
  (let [result (promise)
        error (promise)]
    (async-fn
     #(deliver result %)
     #(deliver error %))
    (let [res (deref (future
                       (try
                         (or (deref result timeout-ms nil)
                             (deref error timeout-ms :timeout))
                         (catch Exception e e)))
                     (+ timeout-ms 100)
                     :timeout)]
      (cond
        (= res :timeout) (throw (Exception. "Test timed out"))
        (instance? Exception res) (throw res)
        :else res))))

(deftest test-for-async-basic
  (testing "Basic seq/for with single binding"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3]]
                                (* x 2))]
                      ;; Consume the sequence
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      (is (= [2 4 6] result)))))

(deftest test-for-async-with-await
  (testing "seq/for with await in body"
    (let [async-double (fn [x]
                         (fn [resolve _reject]
                           (future
                             (Thread/sleep 1)  ; Simulate async work
                             ((async-callback resolve) (* x 2)))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3]]
                                (await (async-double x)))]
                      ;; Consume the sequence
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  2000)]
      (is (= [2 4 6] result)))))

(deftest test-for-async-empty-seq
  (testing "seq/for with empty sequence"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x []]
                                (* x 2))]
                     (if-let [[v _] (await (seq/anext aseq))]
                       :got-value
                       :empty)))
                  1000)]
      (is (= :empty result)))))

(deftest test-for-async-lazy
  (testing "seq/for is lazy - doesn't execute until consumed"
    (let [executed (atom [])
          aseq (seq/for [x [1 2 3]]
                 (do
                   (swap! executed conj x)
                   (* x 2)))]
      ;; Just creating the sequence shouldn't execute
      (is (= [] @executed))

      ;; Consume first element
      (let [[_v rest-s1] (blocking-test (async (await (seq/anext aseq))) 1000)]
        (is (= [1] @executed))

        ;; Consume second element
        (let [[_v2 _rest-s2] (blocking-test (async (await (seq/anext rest-s1))) 1000)]
          (is (= [1 2] @executed)))))))

(deftest test-for-async-with-first-helper
  (testing "seq/for with seq/first helper"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [10 20 30]]
                                (+ x 5))]
                     (await (seq/first aseq))))
                  1000)]
      (is (= 15 result)))))

(deftest test-for-async-with-rest-helper
  (testing "seq/for with seq/rest helper"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [10 20 30]]
                                (+ x 5))
                         rest-s (await (seq/rest aseq))]
                     (await (seq/first rest-s))))
                  1000)]
      (is (= 25 result)))))

;; Modifier tests

(deftest test-for-when-modifier
  (testing "seq/for with :when modifier"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x (range 10)
                                        :when (even? x)]
                                x)]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      (is (= [0 2 4 6 8] result)))))

(deftest test-for-let-modifier
  (testing "seq/for with :let modifier"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3]
                                        :let [y (* x 2)
                                              z (+ y 1)]]
                                [x y z])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      (is (= [[1 2 3] [2 4 5] [3 6 7]] result)))))

(deftest test-for-while-modifier
  (testing "seq/for with :while modifier"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x (range 10)
                                        :while (< x 5)]
                                x)]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      (is (= [0 1 2 3 4] result)))))

(deftest test-for-combined-modifiers
  (testing "seq/for with combined modifiers"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x (range 20)
                                        :when (odd? x)
                                        :let [y (* x 10)]
                                        :while (< y 100)]
                                [x y])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      (is (= [[1 10] [3 30] [5 50] [7 70] [9 90]] result)))))

(deftest test-for-modifier-with-await
  (testing "seq/for with :let and await"
    (let [async-inc (fn [x]
                      (fn [resolve _reject]
                        (future ((async-callback resolve) (inc x)))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3]
                                        :let [y (await (async-inc x))]]
                                [x y])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  2000)]
      (is (= [[1 2] [2 3] [3 4]] result)))))

;; Multiple binding tests

(deftest test-for-two-bindings
  (testing "seq/for with two bindings (cross-product)"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2]
                                        y [:a :b]]
                                [x y])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      (is (= [[1 :a] [1 :b] [2 :a] [2 :b]] result)))))

(deftest test-for-two-bindings-with-modifiers
  (testing "seq/for with two bindings and :when modifiers"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3 4]
                                        :when (odd? x)
                                        y [:a :b :c]
                                        :when (not= y :b)]
                                [x y])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      ;; Only odd x (1, 3) and only y != :b (:a, :c)
      (is (= [[1 :a] [1 :c] [3 :a] [3 :c]] result)))))

(deftest test-for-three-bindings
  (testing "seq/for with three bindings"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2]
                                        y [:a :b]
                                        z [:x :y]]
                                [x y z])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      (is (= [[1 :a :x] [1 :a :y] [1 :b :x] [1 :b :y]
              [2 :a :x] [2 :a :y] [2 :b :x] [2 :b :y]]
             result)))))

(deftest test-for-multiple-bindings-with-await
  (testing "seq/for with multiple bindings and await in body"
    (let [async-sum (fn [x y]
                      (fn [resolve _reject]
                        (future ((async-callback resolve) (+ x y)))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2]
                                        y [10 20]]
                                (await (async-sum x y)))]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  2000)]
      (is (= [11 21 12 22] result)))))

(deftest test-for-two-bindings-empty-inner
  (testing "seq/for with empty inner sequence"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2]
                                        y []]
                                [x y])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  1000)]
      ;; Empty inner means no cross-product results
      (is (= [] result)))))

;; Tests with await in various positions

(deftest test-for-await-in-when-modifier
  (testing "seq/for with await in :when condition"
    (let [async-odd? (fn [x]
                       (fn [resolve _reject]
                         (future ((async-callback resolve) (odd? x)))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3 4 5]
                                        :when (await (async-odd? x))]
                                x)]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  2000)]
      (is (= [1 3 5] result)))))

(deftest test-for-await-in-sequence-expr
  (testing "seq/for with await in sequence expression"
    (let [async-range (fn [n]
                        (fn [resolve _reject]
                          (future ((async-callback resolve) (range n)))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x (await (async-range 5))
                                        :when (even? x)]
                                (* x 10))]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  2000)]
      (is (= [0 20 40] result)))))

(deftest test-for-await-in-multiple-binding-sequences
  (testing "seq/for with await in both sequence expressions"
    (let [async-vec (fn [v]
                      (fn [resolve _reject]
                        (future ((async-callback resolve) v))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x (await (async-vec [1 2]))
                                        y (await (async-vec [:a :b]))]
                                [x y])]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  2000)]
      (is (= [[1 :a] [1 :b] [2 :a] [2 :b]] result)))))

(deftest test-for-await-in-while-modifier
  (testing "seq/for with await in :while condition"
    (let [async-less-than (fn [x limit]
                            (fn [resolve _reject]
                              (future ((async-callback resolve) (< x limit)))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x (range 10)
                                        :while (await (async-less-than x 5))]
                                x)]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  2000)]
      (is (= [0 1 2 3 4] result)))))

(deftest test-for-complex-nested-awaits
  (testing "seq/for with awaits in multiple positions"
    (let [async-filter (fn [x] (fn [r _] (future ((async-callback r) (odd? x)))))
          async-transform (fn [x y] (fn [r _] (future ((async-callback r) (+ x y)))))
          async-items (fn [] (fn [r _] (future ((async-callback r) [1 2 3]))))
          result (blocking-test
                  (async
                   (let [aseq (seq/for [x (await (async-items))
                                        :when (await (async-filter x))
                                        y [10 20]
                                        :let [z (await (async-transform x y))]]
                                z)]
                     (loop [s aseq
                            acc []]
                       (if-let [[v rest-s] (await (seq/anext s))]
                         (recur rest-s (conj acc v))
                         acc))))
                  3000)]
      ;; Only odd x (1, 3), each combined with [10 20]
      (is (= [11 21 13 23] result)))))

;; Test immutability and sharing

(deftest test-for-sequence-sharing
  (testing "seq/for sequences can be safely shared between multiple consumers"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3]] (* x 10))
                          ;; First consumer takes first element
                         first-val (await (seq/first aseq))
                          ;; Second consumer (from same original seq) should still get first element
                         second-val (await (seq/first aseq))
                          ;; Consume rest from original
                         rest-seq (await (seq/rest aseq))
                         rest-first (await (seq/first rest-seq))]
                     [first-val second-val rest-first]))
                  1000)]
      ;; Both consumers see same original sequence (immutable)
      (is (= [10 10 20] result)))))

(deftest test-for-independent-consumption
  (testing "Multiple independent consumptions of shared seq/for"
    (let [result (blocking-test
                  (async
                   (let [aseq (seq/for [x [1 2 3]] (* x 2))
                          ;; Consumer 1: take all
                         consumer1 (loop [s aseq acc []]
                                     (if-let [[v rest-s] (await (seq/anext s))]
                                       (recur rest-s (conj acc v))
                                       acc))
                          ;; Consumer 2: take all from same original seq
                         consumer2 (loop [s aseq acc []]
                                     (if-let [[v rest-s] (await (seq/anext s))]
                                       (recur rest-s (conj acc v))
                                       acc))]
                     [consumer1 consumer2]))
                  1000)]
      ;; Both consumers see full sequence independently
      (is (= [[2 4 6] [2 4 6]] result)))))
