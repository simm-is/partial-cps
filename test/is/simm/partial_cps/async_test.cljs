(ns is.simm.partial-cps.async-test
  (:require [cljs.test :as test :refer-macros [deftest testing is]]
            [is.simm.partial-cps.async :refer [await]]
            [clojure.pprint :refer [pprint]]
            [is.simm.partial-cps.sequence-test :as sequence]
            [is.simm.partial-cps.for-async-test :as for-async]
            [is.simm.partial-cps.core-test :as core-test])
  (:require-macros [is.simm.partial-cps.async :refer [async]]))

;; Test helpers
(defn promise-delay
  "Returns a promise that resolves after ms milliseconds"
  [ms value]
  (js/Promise. (fn [resolve _] (js/setTimeout #(resolve value) ms))))

(defn async-cb-delay
  "Simulates an async operation using callbacks"
  [ms value]
  (fn [resolve _reject] (js/setTimeout (fn [] (resolve value)) ms)))

(defn failing-async
  "An async operation that always fails"
  [error-msg]
  (fn [_resolve reject]
    (js/setTimeout #(reject (js/Error. error-msg)) 10)))

;; Basic async/await tests
(deftest test-simple-async-await
  (test/async done
              ((async
                (is (= "hello" (await (async-cb-delay 10 "hello")))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-multiple-awaits
  (test/async done
              ((async
                (let [a (await (async-cb-delay 10 1))
                      b (await (async-cb-delay 10 2))
                      c (await (async-cb-delay 10 3))]
                  (is (= [1 2 3] [a b c]))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-await-in-conditional
  (test/async done
              ((async
                (let [result (if true
                               (await (async-cb-delay 10 "true-branch"))
                               (await (async-cb-delay 10 "false-branch")))]
                  (is (= "true-branch" result))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-cond-expansion
  (test/async done
              ((async
                (let [result (atom [])]
                  (cond
                    (await (async-cb-delay 10 true)) (swap! result conj :first-branch)
                    (and false true) (await (async-cb-delay 10 :second-branch))
                    (await (async-cb-delay 10 false)) (swap! result conj :third-branch)
                    :else (swap! result conj :fourth-branch))
                  (is (= [:first-branch] @result))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-await-in-let-binding
  (test/async done
              ((async
                (let [x (await (async-cb-delay 10 5))
                      y (* x 2)
                      z (await (async-cb-delay 10 y))]
                  (is (= [5 10 10] [x y z]))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Error handling tests
(deftest test-error-handling
  (test/async done
              ((async
                (try
                  (await (failing-async "Test error"))
                  (is false "Should have failed")
                  (catch :default err
                    (is (= "Test error" (.-message err))))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

(deftest test-try-catch-in-async
  (test/async done
              ((async
                (let [result (try
                               (await (failing-async "Caught error"))
                               :should-not-reach
                               (catch :default e
                                 (.-message e)))]
                  (is (= "Caught error" result))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Loop and recur tests
(deftest test-loop-with-await
  (test/async done
              ((async
                (let [results (loop [i 0
                                     acc []]
                                (if (< i 2000)
                                  (let [value (await (async i) #_(async-cb-delay 10 i))]
                                    (recur (inc i) (conj acc value)))
                                  acc))]
                  (is (= (vec (range 2000)) results))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Nested async functions
(deftest test-nested-async-functions
  (test/async done
              ((async
                (let [inner-async (async
                                   (let [x (await (async-cb-delay 10 5))]
                                     (* x 2)))
                      result (await inner-async)]
                  (is (= 10 result))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Complex control flow
(deftest test-complex-control-flow
  (test/async done
              ((async
                (let [result (atom [])]
                  (let [a (await (async-cb-delay 10 1))]
                    (swap! result conj a)
                    (when (= a 1)
                      (let [b (await (async-cb-delay 10 2))]
                        (swap! result conj b)
                        (if (> b 1)
                          (swap! result conj (await (async-cb-delay 10 3)))
                          (swap! result conj :else)))))
                  (is (= [1 2 3] @result))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Promise interop test
(deftest test-promise-interop
  (test/async done
              ((async
                (let [value (await (fn [resolve reject]
                                     (.then (promise-delay 10 "from-promise")
                                            resolve
                                            reject)))]
                  (is (= "from-promise" value))))
               (fn [_v] (done))
               (fn [err] (is false (str "Unexpected error: " err)) (done)))))

;; Test runner
(defn ^:export run-tests []
  (test/run-tests 'is.simm.partial-cps.async-test)
  (test/run-tests 'is.simm.partial-cps.sequence-test)
  (test/run-tests 'is.simm.partial-cps.for-async-test)
  (test/run-tests 'is.simm.partial-cps.core-test))
