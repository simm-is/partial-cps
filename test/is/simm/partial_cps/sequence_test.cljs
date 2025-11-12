(ns is.simm.partial-cps.sequence-test
  (:refer-clojure :exclude [first rest sequence transduce into])
  (:require [cljs.test :as test :refer-macros [deftest testing is]]
            [is.simm.partial-cps.sequence :as seq]
            [is.simm.partial-cps.async :refer [await]])
  (:require-macros [is.simm.partial-cps.async :refer [async doseq-async dotimes-async]]))

;; Test helpers for ClojureScript
(defn async-cb-delay
  "Returns a function that simulates async operation using setTimeout"
  [ms value]
  (fn [resolve reject]
    (js/setTimeout
     (fn []
       (try
         (resolve value)
         (catch :default e
           (reject e))))
     ms)))

(declare ->SimpleAsyncSeq ->SlowAsyncSeq)

;; Simple async sequence implementation for testing
(defrecord SimpleAsyncSeq [items]
  is.simm.partial-cps.sequence/PAsyncSeq
  (anext [_]
    (async
      (when-let [s (cljs.core/seq items)]
        [(cljs.core/first s)
         (when-let [rst (cljs.core/seq (cljs.core/rest s))]
           (->SimpleAsyncSeq rst))]))))

;; Slow async sequence for testing lazy evaluation
(defrecord SlowAsyncSeq [items delay-ms processed-count]
  is.simm.partial-cps.sequence/PAsyncSeq
  (anext [_]
    (async
      (when-let [s (cljs.core/seq items)]
        (swap! processed-count inc)
        (let [v (await (async-cb-delay delay-ms (cljs.core/first s)))]
          [v
           (when-let [rst (cljs.core/seq (cljs.core/rest s))]
             (->SlowAsyncSeq rst delay-ms processed-count))])))))

(defn make-slow-seq [items delay-ms]
  (->SlowAsyncSeq items delay-ms (atom 0)))

;; Basic IAsyncSeq Protocol Tests  
(deftest test-basic-async-seq-operations
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])]
      ((async
          (let [first-val (await (seq/first async-seq))
                rest-seq (await (seq/rest async-seq))
                second-val (await (seq/first rest-seq))
                rest2-seq (await (seq/rest rest-seq))
                third-val (await (seq/first rest2-seq))]
            [first-val second-val third-val]))
        (fn [result]
          (is (= [1 2 3] result))
          (done))
        (fn [error]
          (is false (str "Should not fail: " error))
          (done))))))

(deftest test-empty-async-seq
  (test/async done
    (let [empty-seq (->SimpleAsyncSeq [])]
      ((async
          (let [first-val (await (seq/first empty-seq))
                rest-val (await (seq/rest empty-seq))]
            [first-val rest-val]))
        (fn [result]
          (is (= [nil nil] result))
          (done))
        (fn [error]
          (is false (str "Should not fail: " error))
          (done))))))

(deftest test-single-element-seq
  (test/async done
    (let [single-seq (->SimpleAsyncSeq [:only])]
      ((async
          (let [first-val (await (seq/first single-seq))
                rest-seq (await (seq/rest single-seq))
                rest-first (when rest-seq (await (seq/first rest-seq)))]
            [first-val (some? rest-seq) rest-first]))
        (fn [result]
          (is (= [:only false nil] result))
          (done))
        (fn [error]
          (is false (str "Should not fail: " error))
          (done))))))

;; Transduce Tests
(deftest test-transduce-basic-map
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])]
      ((async
        (await (seq/transduce (map #(* % 10)) + 0 async-seq)))
       (fn [result]
         (is (= 150 result))  ; [10 20 30 40 50] sum = 150
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-transduce-filter
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])]
      ((async
        (await (seq/transduce (filter even?) conj [] async-seq)))
       (fn [result]
         (is (= [2 4 6 8 10] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-transduce-composed
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])]
      ((async
        (await (seq/transduce (comp (filter even?)
                                    (map #(* % 10))
                                    (take 3))
                              conj [] async-seq)))
       (fn [result]
         (is (= [20 40 60] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-transduce-partition-all
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7])]
      ((async
        (await (seq/transduce (partition-all 3) conj [] async-seq)))
       (fn [result]
         (is (= [[1 2 3] [4 5 6] [7]] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-transduce-with-early-termination
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])]
      ((async
        (await (seq/transduce (take 4) conj [] async-seq)))
       (fn [result]
         (is (= [1 2 3 4] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-transduce-empty-seq
  (test/async done
    (let [empty-seq (->SimpleAsyncSeq [])]
      ((async
        (await (seq/transduce (map inc) + 42 empty-seq)))
       (fn [result]
         (is (= 42 result))  ; Should return init value
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

;; Into Tests
(deftest test-into-basic
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])]
      ((async
        (await (seq/into [] async-seq)))
       (fn [result]
         (is (= [1 2 3 4 5] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-into-with-transducer
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])]
      ((async
        (await (seq/into [] (map #(* % %)) async-seq)))
       (fn [result]
         (is (= [1 4 9 16 25] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-into-set
  (test/async done
              (let [async-seq (->SimpleAsyncSeq [1 2 3 2 1])]
                ((async
                  (await (seq/into #{} async-seq)))
                 (fn [result]
                   (is (= #{1 2 3} result))
                   (done))
                 (fn [error]
                   (is false (str "Should not fail: " error))
                   (done))))))

;; Sequence (lazy transformation) Tests
(deftest test-sequence-basic-map
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5])
          mapped-seq (seq/sequence (map #(* % 10)) async-seq)]
      ((async
        (let [first-val (await (seq/first mapped-seq))
              rest-seq (await (seq/rest mapped-seq))
              second-val (await (seq/first rest-seq))]
          [first-val second-val]))
       (fn [result]
         (is (= [10 20] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-sequence-filter
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7 8 9 10])
          filtered-seq (seq/sequence (filter even?) async-seq)]
      ((async
        ;; Collect first 3 even numbers
        (let [first-val (await (seq/first filtered-seq))
              rest1 (await (seq/rest filtered-seq))
              second-val (await (seq/first rest1))
              rest2 (await (seq/rest rest1))
              third-val (await (seq/first rest2))]
          [first-val second-val third-val]))
       (fn [result]
         (is (= [2 4 6] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-sequence-partition-all
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3 4 5 6 7])
          partitioned-seq (seq/sequence (partition-all 3) async-seq)]
      ((async
        (let [first-partition (await (seq/first partitioned-seq))
              rest-seq (await (seq/rest partitioned-seq))
              second-partition (await (seq/first rest-seq))
              rest2-seq (await (seq/rest rest-seq))
              third-partition (await (seq/first rest2-seq))]
          [first-partition second-partition third-partition]))
       (fn [result]
         (is (= [[1 2 3] [4 5 6] [7]] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-sequence-composed-transducers
  (test/async done
    (let [async-seq (->SimpleAsyncSeq (range 1 20))
          transformed-seq (seq/sequence (comp (filter even?)
                                              (map #(* % 10))
                                              (take 3))
                                        async-seq)]
      ((async
        (await (seq/into [] transformed-seq)))
       (fn [result]
         (is (= [20 40 60] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-sequence-empty-result-transduce
  (test/async done
    ;; Test empty result using transduce which handles completion properly
    (let [async-seq (->SimpleAsyncSeq [1 3 5 7 9])]  ; All odd numbers
      ((async
        (await (seq/transduce (filter even?) conj [] async-seq)))
       (fn [result]
         (is (= [] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

;; Lazy Evaluation Tests
(deftest test-lazy-evaluation-basic
  (test/async done
    (let [slow-seq (make-slow-seq [1 2 3 4 5 6 7 8 9 10] 10)
          filtered-seq (seq/sequence (filter even?) slow-seq)]
      ((async
        (await (seq/first filtered-seq)))
       (fn [result]
         (is (= 2 result))
         ;; Should have processed exactly 2 elements (1 and 2)
         (is (= 2 @(.-processed_count slow-seq)))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-lazy-evaluation-with-take
  (test/async done
    (let [slow-seq (make-slow-seq (range 1 100) 5)  ; Large range
          taken-seq (seq/sequence (take 3) slow-seq)]
      ((async
        (await (seq/into [] taken-seq)))
       (fn [result]
         (is (= [1 2 3] result))
         ;; Should have processed exactly 3 elements due to take
         (is (= 3 @(.-processed_count slow-seq)))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-lazy-evaluation-filter-early-stop
  (test/async done
    (let [slow-seq (make-slow-seq [1 2 3 4 5 6 7 8 9 10] 10)
          transformed-seq (seq/sequence (comp (filter even?)
                                              (take 2))
                                        slow-seq)]
      ((async
        (await (seq/into [] transformed-seq)))
       (fn [result]
         (is (= [2 4] result))
         ;; Should have processed 1,2,3,4 to get 2 even numbers
         (is (= 4 @(.-processed_count slow-seq)))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

;; Error Handling Tests
(deftest test-sequence-with-error-in-source
  (test/async done
    (let [failing-seq (reify is.simm.partial-cps.sequence/PAsyncSeq
                        (anext [_]
                          (async (throw (js/Error. "Source error")))))]
      ((async
        (await (seq/first failing-seq)))
       (fn [result]
         (is false "Should not succeed")
         (done))
       (fn [error]
         (is (= "Source error" (.-message error)))
         (done))))))

;; Performance and Edge Cases
(deftest test-large-sequence-performance
  (test/async done
    (let [large-seq (->SimpleAsyncSeq (range 10000))]
      ((async
        ;; Just take first few to verify it doesn't load everything
        (await (seq/transduce (take 5) conj [] large-seq)))
       (fn [result]
         (is (= [0 1 2 3 4] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))

(deftest test-nil-sequence-handling
  (testing "Nil sequence handling"
    (let [nil-seq (seq/sequence (map inc) nil)]
      (is (nil? nil-seq)))))

(deftest test-nested-async-sequences
  (test/async done
    (let [async-seq (->SimpleAsyncSeq [1 2 3])]
      ((async
        ;; Create sequence of sequences
        (let [mapped-seq (seq/sequence (map #(->SimpleAsyncSeq [% (* % 10)])) async-seq)
              first-inner-seq (await (seq/first mapped-seq))
              first-inner-val (await (seq/first first-inner-seq))
              rest-inner (await (seq/rest first-inner-seq))
              second-inner-val (await (seq/first rest-inner))]
          [first-inner-val second-inner-val]))
       (fn [result]
         (is (= [1 10] result))
         (done))
       (fn [error]
         (is false (str "Should not fail: " error))
         (done))))))


;; Test runner
(defn ^:export run-sequence-tests []
  (cljs.test/run-tests 'is.simm.partial-cps.sequence-test))
