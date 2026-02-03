(ns is.simm.partial-cps.core-test
  "Cross-platform tests for partial-cps core functionality.

   These tests run on both Clojure and ClojureScript."
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [is.simm.partial-cps.async :refer [await async]]
               :cljs [is.simm.partial-cps.async :refer [await]])
            #?(:clj [is.simm.partial-cps.test-helpers :refer [async-delay
                                                              failing-async
                                                              test-async
                                                              test-async-error
                                                              test-async-fn]]
               :cljs [is.simm.partial-cps.test-helpers :refer [async-delay
                                                               failing-async]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async]]
                            [is.simm.partial-cps.test-helpers :refer [test-async
                                                                      test-async-error
                                                                      test-async-fn]])))

;; =============================================================================
;; Basic Async Tests
;; =============================================================================

(deftest test-simple-async
  (testing "Basic async without await returns value"
    (test-async (async "hello") "hello")))

(deftest test-simple-await
  (testing "Basic async with await"
    (test-async
     (async
      (let [value (await (async-delay 20 "hello"))]
        value))
     "hello")))

(deftest test-multiple-awaits
  (testing "Multiple await calls in sequence"
    (test-async
     (async
      (let [a (await (async-delay 10 1))
            b (await (async-delay 10 2))
            c (await (async-delay 10 3))]
        [a b c]))
     [1 2 3])))

(deftest test-await-in-let-binding
  (testing "Await in let bindings with computation"
    (test-async
     (async
      (let [x (await (async-delay 10 5))
            y (* x 2)
            z (await (async-delay 10 y))]
        [x y z]))
     [5 10 10])))

(deftest test-conditional-await
  (testing "Await in conditional branches"
    (test-async
     (async
      (if true
        (await (async-delay 10 "true-branch"))
        (await (async-delay 10 "false-branch"))))
     "true-branch")))

;; =============================================================================
;; Case Expression Tests
;; =============================================================================

(deftest test-case-with-await-in-branches
  (testing "case with await in matching branches"
    (test-async
     (async
      (case :b
        :a (await (async-delay 20 "matched-a"))
        :b (await (async-delay 20 "matched-b"))
        :c (await (async-delay 20 "matched-c"))
        (await (async-delay 20 "default"))))
     "matched-b")))

(deftest test-case-with-await-in-default
  (testing "case with await in default branch"
    (test-async
     (async
      (case :unknown
        :a "matched-a"
        :b "matched-b"
        (await (async-delay 20 "default-async"))))
     "default-async")))

(deftest test-case-with-await-in-test-expr
  (testing "case where test expression contains await"
    (test-async
     (async
      (case (await (async-delay 20 :c))
        :a "matched-a"
        :b "matched-b"
        :c "matched-c"
        "default"))
     "matched-c")))

(deftest test-case-with-mixed-sync-async-branches
  (testing "case with some sync and some async branches"
    (test-async
     (async
      (case :b
        :a "sync-a"
        :b (await (async-delay 20 "async-b"))
        :c "sync-c"
        "default"))
     "async-b")))

(deftest test-case-with-multiple-constants
  (testing "case with multiple constants matching same branch"
    (test-async
     (async
      (case :y
        (:a :b :c) (await (async-delay 20 "first-group"))
        (:x :y :z) (await (async-delay 20 "second-group"))
        (await (async-delay 20 "default"))))
     "second-group")))

(deftest test-case-integer-dispatch
  (testing "case with integer dispatch and await"
    (test-async
     (async
      (case 2
        1 (await (async-delay 20 "one"))
        2 (await (async-delay 20 "two"))
        3 (await (async-delay 20 "three"))
        (await (async-delay 20 "other"))))
     "two")))

(deftest test-case-string-dispatch
  (testing "case with string dispatch and await"
    (test-async
     (async
      (case "hello"
        "hi" (await (async-delay 20 "greeting-hi"))
        "hello" (await (async-delay 20 "greeting-hello"))
        "hey" (await (async-delay 20 "greeting-hey"))
        (await (async-delay 20 "unknown-greeting"))))
     "greeting-hello")))

(deftest test-case-in-let-binding
  (testing "case result with await used in let binding"
    (test-async
     (async
      (let [x (case :multiply
                :add (await (async-delay 20 (+ 2 3)))
                :multiply (await (async-delay 20 (* 2 3)))
                :subtract (await (async-delay 20 (- 5 2)))
                0)]
        (* x 10)))
     60)))

(deftest test-nested-case-with-await
  (testing "nested case expressions with await"
    (test-async
     (async
      (case :outer-b
        :outer-a (case :inner-x
                   :inner-x (await (async-delay 20 "a-x"))
                   :inner-y (await (async-delay 20 "a-y"))
                   "a-default")
        :outer-b (case :inner-y
                   :inner-x (await (async-delay 20 "b-x"))
                   :inner-y (await (async-delay 20 "b-y"))
                   "b-default")
        "outer-default"))
     "b-y")))

;; =============================================================================
;; Error Handling Tests
;; =============================================================================

(deftest test-async-error-handling
  (testing "Async operation that throws is caught by error handler"
    (test-async-error
     (async
      (await (failing-async "Test error")))
     "Test error")))

(deftest test-exception-in-async-block
  (testing "Exception thrown in async block"
    (test-async-error
     (async
      (throw (#?(:clj Exception. :cljs js/Error.) "Sync error")))
     "Sync error")))

;; =============================================================================
;; Loop and Recur Tests
;; =============================================================================

(deftest test-loop-with-await
  (testing "Loop with await inside"
    (test-async
     (async
      (loop [i 0
             acc []]
        (if (< i 3)
          (let [value (await (async-delay 10 i))]
            (recur (inc i) (conj acc value)))
          acc)))
     [0 1 2])))

;; =============================================================================
;; Complex Control Flow Tests
;; =============================================================================

(deftest test-complex-control-flow
  (testing "Complex mix of control structures with await"
    (test-async-fn
     (async
      (let [results (atom [])]
        (let [a (await (async-delay 10 1))]
          (swap! results conj a)
          (when (= a 1)
            (let [b (await (async-delay 10 2))]
              (swap! results conj b)
              (if (> b 1)
                (swap! results conj (await (async-delay 10 3)))
                (swap! results conj :else))))
          @results)))
     (fn [result]
       (is (= [1 2 3] result))))))

;; =============================================================================
;; Nested Async Tests
;; =============================================================================

(deftest test-nested-async
  (testing "Async function calling another async function"
    (test-async
     (async
      (let [inner-async (async
                         (let [x (await (async-delay 10 5))]
                           (* x 2)))
            inner-result (await inner-async)]
        (+ inner-result 5)))
     15)))

;; =============================================================================
;; Various Data Types Tests
;; =============================================================================

(deftest test-various-data-types
  (testing "Await works with different data types"
    (test-async
     (async
      {:number (await (async-delay 10 42))
       :string (await (async-delay 10 "test"))
       :vector (await (async-delay 10 [1 2 3]))
       :map (await (async-delay 10 {:key "value"}))})
     {:number 42
      :string "test"
      :vector [1 2 3]
      :map {:key "value"}})))

;; =============================================================================
;; Interop Tests - Method Calls (.)
;; =============================================================================

(deftest test-method-call-with-await-args
  (testing "Method call with async arguments"
    (test-async
     (async
      (let [s (await (async-delay 10 "hello world"))]
        #?(:clj (.substring s 0 5)
           :cljs (.substring s 0 5))))
     "hello")))

(deftest test-method-call-chain-with-await
  (testing "Method call chain with async values"
    (test-async
     (async
      (let [s (await (async-delay 10 "  hello  "))]
        #?(:clj (-> s .trim .toUpperCase)
           :cljs (-> s .trim .toUpperCase))))
     "HELLO")))

(deftest test-method-with-multiple-async-args
  (testing "Method call with multiple async arguments"
    (test-async
     (async
      (let [s (await (async-delay 10 "hello world"))
            start (await (async-delay 10 6))
            end (await (async-delay 10 11))]
        #?(:clj (.substring s start end)
           :cljs (.substring s start end))))
     "world")))

;; =============================================================================
;; Interop Tests - Constructor (new)
;; =============================================================================

#?(:clj
   (deftest test-new-with-async-args-clj
     (testing "Java constructor with async arguments"
       (test-async
        (async
         (let [msg (await (async-delay 10 "test message"))]
           (.getMessage (Exception. msg))))
        "test message"))))

#?(:cljs
   (deftest test-new-with-async-args-cljs
     (testing "JS constructor with async arguments"
       (test-async
        (async
         (let [msg (await (async-delay 10 "test message"))]
           (.-message (js/Error. msg))))
        "test message"))))

(deftest test-new-with-computed-async-args
  (testing "Constructor with computed async arguments"
    (test-async
     (async
      (let [a (await (async-delay 10 5))
            b (await (async-delay 10 3))]
        #?(:clj (let [sb (StringBuilder. (str (* a b)))]
                  (.toString sb))
           :cljs (str (* a b)))))
     "15")))

;; =============================================================================
;; Interop Tests - set! (mutation)
;; =============================================================================

(deftest test-set-atom-with-async-value
  (testing "Setting atom with async value"
    (test-async-fn
     (async
      (let [a (atom 0)
            v (await (async-delay 10 42))]
        (reset! a v)
        @a))
     (fn [result]
       (is (= 42 result))))))

(deftest test-set-volatile-with-async-value
  (testing "Setting volatile with async value"
    (test-async-fn
     (async
      (let [v (volatile! 0)
            new-val (await (async-delay 10 99))]
        (vreset! v new-val)
        @v))
     (fn [result]
       (is (= 99 result))))))

#?(:clj
   (deftest test-set-field-with-async-value-clj
     (testing "Setting Java field with async value"
       (test-async-fn
        (async
         (let [sb (StringBuilder.)
               s (await (async-delay 10 "async-content"))]
           (.append sb s)
           (.toString sb)))
        (fn [result]
          (is (= "async-content" result)))))))

#?(:cljs
   (deftest test-set-field-with-async-value-cljs
     (testing "Setting JS object property with async value"
       (test-async-fn
        (async
         (let [obj #js {}
               v (await (async-delay 10 "async-value"))]
           (set! (.-prop obj) v)
           (.-prop obj)))
        (fn [result]
          (is (= "async-value" result)))))))

;; =============================================================================
;; Interop Tests - Combined patterns
;; =============================================================================

(deftest test-interop-in-loop
  (testing "Interop within loop with await"
    (test-async
     (async
      (loop [i 0
             acc #?(:clj (StringBuilder.) :cljs "")]
        (if (< i 3)
          (let [s (await (async-delay 5 (str i)))]
            (recur (inc i)
                   #?(:clj (doto acc (.append s))
                      :cljs (str acc s))))
          #?(:clj (.toString acc) :cljs acc))))
     "012")))

(deftest test-interop-in-conditional
  (testing "Interop in conditional with await"
    (test-async
     (async
      (let [condition (await (async-delay 10 true))
            value (await (async-delay 10 "TEST"))]
        (if condition
          #?(:clj (.toLowerCase value)
             :cljs (.toLowerCase value))
          value)))
     "test")))
