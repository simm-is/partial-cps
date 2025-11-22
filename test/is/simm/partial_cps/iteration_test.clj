(ns is.simm.partial-cps.iteration-test
  "Tests for transparent iteration macro support (doseq, dotimes, while) in CPS-transformed code.

  Standard Clojure iteration macros work transparently with CPS transformation.
  The previously separate -async macros (doseq-async, dotimes-async) have been removed
  since they are no longer necessary."
  (:require [clojure.test :refer :all]
            [is.simm.partial-cps.async :refer [await async *in-trampoline*]]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn async-callback
  "Wraps a continuation for re-entry from async context.
   Sets *in-trampoline* to false to ensure fresh trampoline establishment."
  [cont]
  (fn [& args]
    (binding [*in-trampoline* false]
      (apply cont args))))

(defn async-cb-delay
  "Creates an async callback that resolves after a delay."
  [ms value]
  (fn [resolve reject]
    (future
      (try
        (Thread/sleep ms)
        ((async-callback resolve) value)
        (catch Throwable t
          ((async-callback reject) t))))))

(defn future-delay
  "Wraps a value in a future with delay, for testing await."
  [ms value]
  (async-cb-delay ms value))

(defn blocking-test
  "Helper to block on async result with timeout."
  [async-fn timeout-ms]
  (let [result (promise)]
    (async-fn
      #(deliver result [:ok %])
      #(deliver result [:error %]))
    (let [[status value] (deref result timeout-ms [:timeout nil])]
      (case status
        :ok value
        :error (throw value)
        :timeout (throw (ex-info "Test timed out" {:timeout-ms timeout-ms}))))))

;; =============================================================================
;; Tests for Transparent `doseq` Support
;; =============================================================================

(deftest test-doseq-basic
  (testing "Basic doseq with sequential await operations"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i [1 2 3]]
                      (let [value (await (future-delay 10 (* i 10)))]
                        (swap! results conj value)))
                    :completed)
                  1000)]
      (is (= :completed result)) ; async block completed
      (is (= [10 20 30] @results)))))

(deftest test-doseq-nested
  (testing "Nested doseq with multiple bindings"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i [1 2] j [:a :b]]
                      (let [value (await (future-delay 5 [i j]))]
                        (swap! results conj value)))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [[1 :a] [1 :b] [2 :a] [2 :b]] @results)))))

(deftest test-doseq-no-await
  (testing "doseq falls through to regular doseq when no await"
    (let [results (atom [])]
      (doseq [i [1 2 3]]
        (swap! results conj (* i 10)))
      (is (= [10 20 30] @results)))))

(deftest test-doseq-with-await-in-binding
  (testing "doseq with await in collection binding"
    (let [results (atom [])
          result (blocking-test
                  (async
                    ;; Await a collection in the binding itself
                    (doseq [i (await (future-delay 20 [1 2 3]))]
                      (let [value (await (future-delay 10 (* i 10)))]
                        (swap! results conj value)))
                    :completed)
                  2000)]
      (is (= :completed result))
      (is (= [10 20 30] @results)))))

(deftest test-doseq-mixed-sync-async-bindings
  (testing "doseq with mix of sync and async bindings"
    (let [results (atom [])
          result (blocking-test
                  (async
                    ;; Mix sync collection with async collection
                    (doseq [letter [:x :y]
                                  i (await (future-delay 20 [1 2]))]
                      (let [value (await (future-delay 10 [letter i]))]
                        (swap! results conj value)))
                    :completed)
                  3000)]
      (is (= :completed result))
      (is (= [[:x 1] [:x 2] [:y 1] [:y 2]] @results)))))

(deftest test-doseq-empty-collection
  (testing "doseq with empty collection"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i []]
                      (let [value (await (future-delay 10 i))]
                        (swap! results conj value)))
                    :completed)
                  500)]
      (is (= :completed result))
      (is (= [] @results)))))

(deftest test-dotimes-basic
  (testing "Basic dotimes with sequential await operations"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (dotimes [i 3]
                      (let [value (await (future-delay 10 (* i 100)))]
                        (swap! results conj value)))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [0 100 200] @results)))))

(deftest test-dotimes-no-await
  (testing "dotimes falls through to regular dotimes when no await"
    (let [results (atom [])]
      (dotimes [i 3]
        (swap! results conj (* i 100)))
      (is (= [0 100 200] @results)))))

(deftest test-dotimes-zero-iterations
  (testing "dotimes with zero iterations"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (dotimes [i 0]
                      (let [value (await (future-delay 10 i))]
                        (swap! results conj value)))
                    :completed)
                  500)]
      (is (= :completed result))
      (is (= [] @results)))))

(deftest test-mixed-async-loops
  (testing "Combining doseq and dotimes"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [letter [:a :b]]
                      (dotimes [i 2]
                        (let [value (await (future-delay 5 [letter i]))]
                          (swap! results conj value))))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [[:a 0] [:a 1] [:b 0] [:b 1]] @results)))))

;; =============================================================================
;; Tests for Transparent `while` Support
;; =============================================================================

(deftest test-while-basic
  (testing "Basic while with await in body"
    (let [counter (atom 0)
          results (atom [])
          result (blocking-test
                  (async
                    (while (< @counter 3)
                      (let [value (await (future-delay 10 @counter))]
                        (swap! results conj value)
                        (swap! counter inc)))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [0 1 2] @results)))))

(deftest test-while-with-await-in-test
  (testing "while with await in test expression"
    (let [results (atom [])
          get-next (fn []
                     (async-cb-delay 10
                       (if (< (count @results) 3)
                         (inc (count @results))
                         nil)))
          result (blocking-test
                  (async
                    (while (await (get-next))
                      (swap! results conj :item))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [:item :item :item] @results)))))

(deftest test-while-early-exit
  (testing "while loop with early exit"
    (let [counter (atom 0)
          results (atom [])
          result (blocking-test
                  (async
                    (while (< @counter 10)
                      (let [value (await (future-delay 10 @counter))]
                        (swap! results conj value)
                        (swap! counter inc)
                        (when (= @counter 3)
                          (swap! counter (constantly 100))))) ; Force exit
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [0 1 2] @results)))))

(deftest test-dotimes-nested
  (testing "Nested dotimes loops"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (dotimes [i 2]
                      (dotimes [j 2]
                        (let [value (await (future-delay 5 [i j]))]
                          (swap! results conj value))))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [[0 0] [0 1] [1 0] [1 1]] @results)))))

;; =============================================================================
;; Additional doseq Tests with Modifiers
;; =============================================================================

(deftest test-doseq-with-let-modifier
  (testing "doseq with :let modifier"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i [1 2 3]
                            :let [doubled (* i 2)]]
                      (let [value (await (future-delay 10 doubled))]
                        (swap! results conj value)))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [2 4 6] @results)))))

(deftest test-doseq-with-when-modifier
  (testing "doseq with :when filter"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i [1 2 3 4 5]
                            :when (even? i)]
                      (let [value (await (future-delay 10 i))]
                        (swap! results conj value)))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [2 4] @results)))))

(deftest test-doseq-with-while-modifier
  (testing "doseq with :while early termination"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i [1 2 3 4 5]
                            :while (< i 4)]
                      (let [value (await (future-delay 10 i))]
                        (swap! results conj value)))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [1 2 3] @results)))))

(deftest test-doseq-with-combined-modifiers
  (testing "doseq with multiple modifiers"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i [1 2 3 4 5]
                            :when (odd? i)
                            :let [squared (* i i)]
                            :while (< squared 20)]
                      (let [value (await (future-delay 10 squared))]
                        (swap! results conj value)))
                    :completed)
                  1000)]
      (is (= :completed result))
      (is (= [1 9] @results))))) ; 1^2=1, 3^2=9, 5^2=25 (stopped by :while)

(deftest test-doseq-triple-nested
  (testing "doseq with three binding pairs"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [i [1 2]
                            j [:a :b]
                            k [:x :y]]
                      (let [value (await (future-delay 5 [i j k]))]
                        (swap! results conj value)))
                    :completed)
                  2000)]
      (is (= :completed result))
      (is (= 8 (count @results))) ; 2 * 2 * 2 = 8
      (is (= [1 :a :x] (first @results)))
      (is (= [2 :b :y] (last @results))))))


;; =============================================================================
;; Performance and Stress Tests
;; =============================================================================

(deftest test-doseq-large-collection
  (testing "doseq with large collection (performance)"
    (let [results (atom [])
          n 100
          result (blocking-test
                  (async
                    (doseq [i (range n)]
                      (let [value (await (future-delay 1 i))]
                        (swap! results conj value)))
                    :completed)
                  10000)]
      (is (= :completed result))
      (is (= (range n) @results)))))

(deftest test-dotimes-large-count
  (testing "dotimes with large iteration count (performance)"
    (let [results (atom [])
          n 100
          result (blocking-test
                  (async
                    (dotimes [i n]
                      (let [value (await (future-delay 1 i))]
                        (swap! results conj value)))
                    :completed)
                  10000)]
      (is (= :completed result))
      (is (= (range n) @results)))))

(deftest test-deeply-nested-loops
  (testing "Deeply nested iteration macros (stress test)"
    (let [results (atom [])
          result (blocking-test
                  (async
                    (doseq [a [1 2]]
                      (dotimes [b 2]
                        (doseq [c [:x :y]]
                          (let [value (await (future-delay 2 [a b c]))]
                            (swap! results conj value)))))
                    :completed)
                  5000)]
      (is (= :completed result))
      (is (= 8 (count @results)))))) ; 2 * 2 * 2 = 8

;; =============================================================================
;; Error Handling Tests
;; =============================================================================

(deftest test-doseq-with-exception-in-body
  (testing "doseq propagates exceptions from body"
    (is (thrown-with-msg?
          Exception
          #"Test exception"
          (blocking-test
            (async
              (doseq [i [1 2 3]]
                (await (future-delay 10 i))
                (when (= i 2)
                  (throw (Exception. "Test exception"))))
              :completed)
            1000)))))

(deftest test-dotimes-with-exception-in-body
  (testing "dotimes propagates exceptions from body"
    (is (thrown-with-msg?
          Exception
          #"Test exception"
          (blocking-test
            (async
              (dotimes [i 5]
                (await (future-delay 10 i))
                (when (= i 3)
                  (throw (Exception. "Test exception"))))
              :completed)
            1000)))))

(deftest test-doseq-with-exception-in-await
  (testing "doseq propagates exceptions from await"
    (is (thrown-with-msg?
          Exception
          #"Async exception"
          (blocking-test
            (async
              (doseq [i [1 2 3]]
                (await (fn [resolve reject]
                         (future
                           (Thread/sleep 10)
                           (if (= i 2)
                             ((async-callback reject) (Exception. "Async exception"))
                             ((async-callback resolve) i))))))
              :completed)
            1000)))))
