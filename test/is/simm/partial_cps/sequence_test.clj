(ns is.simm.partial-cps.sequence-test
  (:refer-clojure :exclude [first rest sequence transduce into])
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [is.simm.partial-cps.sequence :as seq]
            [is.simm.partial-cps.async :refer [await async *in-trampoline*]]))

;; Test helpers
(defn future-delay
  "Returns a function that simulates async operation using future"
  [ms value]
  (fn [resolve reject]
    (future
      (binding [*in-trampoline* false]
        (try
          (Thread/sleep ms)
          (resolve value)
          (catch Exception e
            (reject e)))))))

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

;; Simple async sequence implementation for testing
(defrecord SimpleAsyncSeq [items]
  is.simm.partial-cps.sequence/PAsyncSeq
  (anext [_]
    (async
      (when-let [s (clojure.core/seq items)]
        [(clojure.core/first s)
         (when-let [rst (clojure.core/seq (clojure.core/rest s))]
           (->SimpleAsyncSeq rst))]))))

;; Slow async sequence for testing lazy evaluation
(defrecord SlowAsyncSeq [items delay-ms processed-count]
  is.simm.partial-cps.sequence/PAsyncSeq
  (anext [_]
    (async
      (when-let [s (clojure.core/seq items)]
        (swap! processed-count inc)
        (let [v (await (future-delay delay-ms (clojure.core/first s)))]
          [v
           (when-let [rst (clojure.core/seq (clojure.core/rest s))]
             (->SlowAsyncSeq rst delay-ms processed-count))])))))

(defn make-slow-seq [items delay-ms]
  (->SlowAsyncSeq items delay-ms (atom 0)))

;; Basic IAsyncSeq Protocol Tests
(deftest test-basic-async-seq-operations
  (testing "Basic first and rest operations on async sequence"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])
          result (blocking-test
                  (async
                    (let [first-val (await (seq/first async-seq))
                          rest-seq (await (seq/rest async-seq))
                          second-val (await (seq/first rest-seq))
                          rest2-seq (await (seq/rest rest-seq))
                          third-val (await (seq/first rest2-seq))]
                      [first-val second-val third-val]))
                  1000)]
      (is (= [1 2 3] result)))))

(deftest test-empty-async-seq
  (testing "Empty async sequence operations"
    (let [empty-seq (->SimpleAsyncSeq [])
          result (blocking-test
                  (async
                    (let [first-val (await (seq/first empty-seq))
                          rest-val (await (seq/rest empty-seq))]
                      [first-val rest-val]))
                  1000)]
      (is (= [nil nil] result)))))

(deftest test-single-element-seq
  (testing "Single element async sequence"
    (let [single-seq (->SimpleAsyncSeq [:only])
          result (blocking-test
                  (async
                    (let [first-val (await (seq/first single-seq))
                          rest-seq (await (seq/rest single-seq))
                          rest-first (when rest-seq (await (seq/first rest-seq)))]
                      [first-val (some? rest-seq) rest-first]))
                  1000)]
      (is (= [:only false nil] result)))))

;; Transduce Tests
(deftest test-transduce-basic-map
  (testing "Transduce with map transducer"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])
          result (blocking-test
                  (async
                    (await (seq/transduce (map #(* % 10)) + 0 async-seq)))
                  1000)]
      (is (= 150 result)))))  ; [10 20 30 40 50] sum = 150

(deftest test-transduce-filter
  (testing "Transduce with filter transducer"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])
          result (blocking-test
                  (async
                    (await (seq/transduce (filter even?) conj [] async-seq)))
                  1000)]
      (is (= [2 4 6 8 10] result)))))

(deftest test-transduce-composed
  (testing "Transduce with composed transducers"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])
          result (blocking-test
                  (async
                    (await (seq/transduce (comp (filter even?)
                                                (map #(* % 10))
                                                (take 3))
                                          conj [] async-seq)))
                  1000)]
      (is (= [20 40 60] result)))))

(deftest test-transduce-partition-all
  (testing "Transduce with partition-all"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7])
          result (blocking-test
                  (async
                    (await (seq/transduce (partition-all 3) conj [] async-seq)))
                  1000)]
      (is (= [[1 2 3] [4 5 6] [7]] result)))))

(deftest test-transduce-with-early-termination
  (testing "Transduce with early termination (reduced)"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])
          result (blocking-test
                  (async
                    (await (seq/transduce (take 4) conj [] async-seq)))
                  1000)]
      (is (= [1 2 3 4] result)))))

(deftest test-transduce-empty-seq
  (testing "Transduce on empty sequence"
    (let [empty-seq (->SimpleAsyncSeq [])
          result (blocking-test
                  (async
                    (await (seq/transduce (map inc) + 42 empty-seq)))
                  1000)]
      (is (= 42 result)))))  ; Should return init value

;; Into Tests
(deftest test-into-basic
  (testing "Into with identity transducer"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])
          result (blocking-test
                  (async
                    (await (seq/into [] async-seq)))
                  1000)]
      (is (= [1 2 3 4 5] result)))))

(deftest test-into-with-transducer
  (testing "Into with map transducer"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])
          result (blocking-test
                  (async
                    (await (seq/into [] (map #(* % % )) async-seq)))
                  1000)]
      (is (= [1 4 9 16 25] result)))))

(deftest test-into-different-collections
  (testing "Into various collection types"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 2 1])]
      ;; Into set
      (let [set-result (blocking-test
                        (async
                          (await (seq/into #{} async-seq)))
                        1000)]
        (is (= #{1 2 3} set-result)))
      
      ;; Into map
      (let [map-result (blocking-test
                        (async
                          (await (seq/into {} (map #(vector % (* % 10))) async-seq)))
                        1000)]
        (is (= {1 10, 2 20, 3 30} map-result))))))

;; Sequence (lazy transformation) Tests
(deftest test-sequence-basic-map
  (testing "Lazy sequence with map transformation"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])
          mapped-seq (seq/sequence (map #(* % 10)) async-seq)
          result (blocking-test
                  (async
                    (let [first-val (await (seq/first mapped-seq))
                          rest-seq (await (seq/rest mapped-seq))
                          second-val (await (seq/first rest-seq))]
                      [first-val second-val]))
                  1000)]
      (is (= [10 20] result)))))

(deftest test-sequence-filter
  (testing "Lazy sequence with filter transformation"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])
          filtered-seq (seq/sequence (filter even?) async-seq)
          result (blocking-test
                  (async
                    ;; Collect first 3 even numbers
                    (let [first-val (await (seq/first filtered-seq))
                          rest1 (await (seq/rest filtered-seq))
                          second-val (await (seq/first rest1))
                          rest2 (await (seq/rest rest1))
                          third-val (await (seq/first rest2))]
                      [first-val second-val third-val]))
                  1000)]
      (is (= [2 4 6] result)))))

(deftest test-sequence-partition-all
  (testing "Lazy sequence with partition-all"
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7])
          partitioned-seq (seq/sequence (partition-all 3) async-seq)
          result (blocking-test
                  (async
                    (let [first-partition (await (seq/first partitioned-seq))
                          rest-seq (await (seq/rest partitioned-seq))
                          second-partition (await (seq/first rest-seq))
                          rest2-seq (await (seq/rest rest-seq))
                          third-partition (await (seq/first rest2-seq))]
                      [first-partition second-partition third-partition]))
                  1000)]
      (is (= [[1 2 3] [4 5 6] [7]] result)))))

(deftest test-sequence-composed-transducers
  (testing "Lazy sequence with composed transducers"
    (let [async-seq (->SimpleAsyncSeq (range 1 20))
          transformed-seq (seq/sequence (comp (filter even?)
                                              (map #(* % 10))
                                              (take 3)) 
                                       async-seq)
          result (blocking-test
                  (async
                    (await (seq/into [] transformed-seq)))
                  1000)]
      (is (= [20 40 60] result)))))

(deftest test-sequence-empty-result
  (testing "Sequence that produces empty result - test with transduce"
    ;; Test empty result using transduce which handles completion properly
    (let [async-seq (->SimpleAsyncSeq [1 3 5 7 9])  ; All odd numbers
          result (blocking-test
                  (async
                    (await (seq/transduce (filter even?) conj [] async-seq)))
                  1000)]
      (is (= [] result)))))

;; Lazy Evaluation Tests
(deftest test-lazy-evaluation-basic
  (testing "Lazy evaluation - only processes needed elements"
    (let [slow-seq (make-slow-seq [1 2 3 4 5 6 7 8 9 10] 10)
          filtered-seq (seq/sequence (filter even?) slow-seq)
          result (blocking-test
                  (async
                    (await (seq/first filtered-seq)))
                  1000)]
      (is (= 2 result))
      ;; Should have processed exactly 2 elements (1 and 2)
      (is (= 2 @(.-processed_count slow-seq))))))

(deftest test-lazy-evaluation-with-take
  (testing "Lazy evaluation with take - stops early"
    (let [slow-seq (make-slow-seq (range 1 100) 5)  ; Large range
          taken-seq (seq/sequence (take 3) slow-seq)
          result (blocking-test
                  (async
                    (await (seq/into [] taken-seq)))
                  1000)]
      (is (= [1 2 3] result))
      ;; Should have processed exactly 3 elements due to take
      (is (= 3 @(.-processed_count slow-seq))))))

(deftest test-lazy-evaluation-filter-early-stop
  (testing "Lazy evaluation - filter stops when enough found"
    (let [slow-seq (make-slow-seq [1 2 3 4 5 6 7 8 9 10] 10)
          transformed-seq (seq/sequence (comp (filter even?)
                                              (take 2))
                                        slow-seq)
          result (blocking-test
                  (async
                    (await (seq/into [] transformed-seq)))
                  1000)]
      (is (= [2 4] result))
      ;; Should have processed 1,2,3,4 to get 2 even numbers
      (is (= 4 @(.-processed_count slow-seq))))))

;; Error Handling Tests
(deftest test-sequence-with-error-in-source
  (testing "Error handling in async sequence operations"
    (let [failing-seq (reify is.simm.partial-cps.sequence/PAsyncSeq
                        (anext [_]
                          (async (throw (Exception. "Source error")))))
          result (try
                   (blocking-test
                    (async
                      (await (seq/first failing-seq)))
                    1000)
                   :should-not-reach
                   (catch Exception e
                     (.getMessage e)))]
      (is (= "Source error" result)))))

;; Performance and Edge Cases
(deftest test-large-sequence-performance
  (testing "Performance with large sequences"
    (let [large-seq (->SimpleAsyncSeq (range 10000))
          result (blocking-test
                  (async
                    ;; Just take first few to verify it doesn't load everything
                    (await (seq/transduce (take 5) conj [] large-seq)))
                  2000)]
      (is (= [0 1 2 3 4] result)))))

(deftest test-nil-sequence-handling
  (testing "Nil sequence handling"
    (let [nil-seq (seq/sequence (map inc) nil)]
      (is (nil? nil-seq)))))

(deftest test-nested-async-sequences
  (testing "Nested async sequence operations"
    (let [async-seq (->SimpleAsyncSeq [1 2 3])
          result (blocking-test
                  (async
                    ;; Create sequence of sequences
                    (let [mapped-seq (seq/sequence (map #(->SimpleAsyncSeq [% (* % 10)])) async-seq)
                          first-inner-seq (await (seq/first mapped-seq))
                          first-inner-val (await (seq/first first-inner-seq))
                          rest-inner (await (seq/rest first-inner-seq))
                          second-inner-val (await (seq/first rest-inner))]
                      [first-inner-val second-inner-val]))
                  1000)]
      (is (= [1 10] result)))))

(defn run-all-sequence-tests []
  (run-tests 'is.simm.partial-cps.sequence-test))