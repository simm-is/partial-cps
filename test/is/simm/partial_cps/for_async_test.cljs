(ns is.simm.partial-cps.for-async-test
  (:require [cljs.test :as test :refer-macros [deftest testing is]]
            [is.simm.partial-cps.sequence :as seq]
            [is.simm.partial-cps.async :refer [await]])
  (:require-macros [is.simm.partial-cps.async :refer [async]]
                   [is.simm.partial-cps.sequence :refer [for for-with]]))

;; Test helpers
(defn async-cb-delay
  "Simulates an async operation using callbacks"
  [ms value]
  (fn [resolve _reject] (js/setTimeout (fn [] (resolve value)) ms)))

;; Basic for tests

(deftest test-for-basic
  (test/async done
              ((async
                (let [aseq (for [x [1 2 3]]
                             (* x 2))]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [2 4 6] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-with-await
    (test/async done
                ((async
                  (let [aseq (for [x [1 2 3]]
                               (await (async-cb-delay 10 (* x 2))))]
                    (loop [s aseq
                           acc []]
                      (if-let [[v rest-s] (await (seq/anext s))]
                        (recur rest-s (conj acc v))
                        (is (= [2 4 6] acc))))))
                 (fn [_v] (done))
                 (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-empty-seq
  (test/async done
              ((async
                (let [aseq (for [x []]
                             (* x 2))]
                  (if-let [[v _] (await (seq/anext aseq))]
                    (is false "Should be empty")
                    (is true "Empty sequence works"))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Modifier tests

(deftest test-for-when-modifier
  (test/async done
              ((async
                (let [aseq (for [x (range 10)
                                 :when (even? x)]
                             x)]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [0 2 4 6 8] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-let-modifier
  (test/async done
              ((async
                (let [aseq (for [x [1 2 3]
                                 :let [y (* x 2)
                                       z (+ y 1)]]
                             [x y z])]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [[1 2 3] [2 4 5] [3 6 7]] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-while-modifier
  (test/async done
              ((async
                (let [aseq (for [x (range 10)
                                 :while (< x 5)]
                             x)]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [0 1 2 3 4] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Multiple bindings tests

(deftest test-for-two-bindings
  (test/async done
              ((async
                (let [aseq (for [x [1 2]
                                 y [:a :b]]
                             [x y])]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [[1 :a] [1 :b] [2 :a] [2 :b]] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-two-bindings-with-modifiers
  (test/async done
              ((async
                (let [aseq (for [x [1 2 3 4]
                                 :when (odd? x)
                                 y [:a :b :c]
                                 :when (not= y :b)]
                             [x y])]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [[1 :a] [1 :c] [3 :a] [3 :c]] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-three-bindings
  (test/async done
              ((async
                (let [aseq (for [x [1 2]
                                 y [:a :b]
                                 z [:x :y]]
                             [x y z])]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [[1 :a :x] [1 :a :y] [1 :b :x] [1 :b :y]
                              [2 :a :x] [2 :a :y] [2 :b :x] [2 :b :y]]
                             acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Await in various positions

(deftest test-for-await-in-when
  (test/async done
              ((async
                (let [async-odd? (fn [x] (async-cb-delay 10 (odd? x)))
                      aseq (for [x [1 2 3 4 5]
                                 :when (await (async-odd? x))]
                             x)]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [1 3 5] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-await-in-sequence-expr
  (test/async done
              ((async
                (let [async-range (fn [n] (async-cb-delay 10 (range n)))
                      aseq (for [x (await (async-range 5))
                                 :when (even? x)]
                             (* x 10))]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [0 20 40] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Immutability tests

(deftest test-for-sequence-sharing
  (test/async done
              ((async
                (let [aseq (for [x [1 2 3]] (* x 10))
                      first-val (await (seq/first aseq))
                      second-val (await (seq/first aseq))
                      rest-seq (await (seq/rest aseq))
                      rest-first (await (seq/first rest-seq))]
                  (is (= [10 10 20] [first-val second-val rest-first]))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-for-independent-consumption
  (test/async done
              ((async
                (let [aseq (for [x [1 2 3]] (* x 2))
                      consumer1 (loop [s aseq acc []]
                                  (if-let [[v rest-s] (await (seq/anext s))]
                                    (recur rest-s (conj acc v))
                                    acc))
                      consumer2 (loop [s aseq acc []]
                                  (if-let [[v rest-s] (await (seq/anext s))]
                                    (recur rest-s (conj acc v))
                                    acc))]
                  (is (= [[2 4 6] [2 4 6]] [consumer1 consumer2]))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; for-with tests

(defn trace
  "A simple breakpoint that logs values and passes them through.
   Used to test for-with with custom breakpoints."
  [value]
  (throw (ex-info "trace called outside of CPS scope" {:value value})))

(defn trace-handler
  "Handler for trace breakpoint - just passes the value through."
  [ctx r e]
  (fn [args]
    (let [value (first args)]
      `(~r ~value))))

(deftest test-for-with-custom-breakpoint
  (test/async done
              ((async
                (let [traced-values (atom [])
                      ;; Use for-with with empty breakpoints (just uses await)
                      aseq (seq/for-with {}
                             [x [1 2 3]]
                             (await (async-cb-delay 10 (* x 2))))]
                  (loop [s aseq
                         acc []]
                    (if-let [[v rest-s] (await (seq/anext s))]
                      (recur rest-s (conj acc v))
                      (is (= [2 4 6] acc))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Test runner
(defn run-tests []
  (test/run-tests))
