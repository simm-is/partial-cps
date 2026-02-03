(ns is.simm.partial-cps.test-helpers
  "Cross-platform test utilities for partial-cps.

  Provides a unified `test-async` macro that works identically on CLJ and CLJS,
  allowing tests to be written once and run on both platforms.

  Key features:
  - `test-async`: Runs a partial-cps async block and asserts the result
  - `async-delay`: Cross-platform async delay helper
  - `failing-async`: Cross-platform failing async helper

  Example usage:

    (deftest my-test
      (test-async
        (async
          (let [x (await (async-delay 10 5))]
            (* x 2)))
        10))"
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :as ct]
               :cljs [cljs.test :as ct])
            [is.simm.partial-cps.async :refer [await *in-trampoline*]]
            #?(:clj [clojure.string]
               :cljs [clojure.string])
            #?(:cljs [is.simm.partial-cps.async]))
  #?(:cljs (:require-macros [is.simm.partial-cps.test-helpers]
                            [is.simm.partial-cps.async :refer [async]])))

;; =============================================================================
;; Cross-Platform Async Delay Helpers
;; =============================================================================

(defn async-delay
  "Returns an async operation that resolves after ms milliseconds with value.
   Works on both CLJ (using future/Thread/sleep) and CLJS (using setTimeout)."
  [ms value]
  #?(:clj
     (fn [resolve reject]
       (future
         (binding [*in-trampoline* false]
           (try
             (Thread/sleep ms)
             (resolve value)
             (catch Exception e
               (reject e))))))
     :cljs
     (fn [resolve _reject]
       (js/setTimeout #(resolve value) ms))))

(defn failing-async
  "Returns an async operation that always fails with the given error message.
   Works on both CLJ and CLJS."
  [error-msg]
  #?(:clj
     (fn [_resolve reject]
       (future
         (binding [*in-trampoline* false]
           (reject (Exception. error-msg)))))
     :cljs
     (fn [_resolve reject]
       (js/setTimeout #(reject (js/Error. error-msg)) 10))))

;; =============================================================================
;; Cross-Platform Test Async Macro
;; =============================================================================

#?(:clj
   (defmacro test-async
     "Cross-platform async test that runs a partial-cps async block and asserts the result.

     Takes an async expression and an expected value. Handles the platform-specific
     details of blocking (CLJ) or using cljs.test/async (CLJS).

     Usage:
       (deftest my-test
         (test-async
           (async
             (let [x (await (async-delay 10 5))]
               (* x 2)))
           10
           1000))  ; optional timeout in ms, defaults to 2000

     For tests that expect errors, use test-async-error instead."
     ([async-expr expected]
      `(test-async ~async-expr ~expected 2000))
     ([async-expr expected timeout-ms]
      (if (:js-globals &env)
        ;; CLJS - use cljs.test/async
        `(cljs.test/async done#
                          (~async-expr
                           (fn [result#]
                             (cljs.test/is (= ~expected result#))
                             (done#))
                           (fn [err#]
                             (cljs.test/is false (str "Unexpected error: " err#))
                             (done#))))
        ;; CLJ - use promise-based blocking
        `(let [result# (promise)
               error# (promise)]
           (~async-expr
            #(deliver result# %)
            #(deliver error# %))
           (let [res# (deref (future
                               (try
                                 (or (deref result# ~timeout-ms nil)
                                     (deref error# ~timeout-ms :timeout))
                                 (catch Exception e# e#)))
                             (+ ~timeout-ms 100)
                             :timeout)]
             (cond
               (= res# :timeout) (throw (Exception. "Test timed out"))
               (instance? Exception res#) (throw res#)
               :else (clojure.test/is (= ~expected res#)))))))))

#?(:clj
   (defmacro test-async-error
     "Cross-platform async test that expects an error.

     Takes an async expression and an optional error message pattern to match.

     Usage:
       (deftest my-error-test
         (test-async-error
           (async
             (await (failing-async \"oops\")))
           \"oops\"))  ; optional error message to match"
     ([async-expr]
      `(test-async-error ~async-expr nil 2000))
     ([async-expr error-pattern]
      `(test-async-error ~async-expr ~error-pattern 2000))
     ([async-expr error-pattern timeout-ms]
      (let [err-sym (gensym "err")]
        (if (:js-globals &env)
          ;; CLJS
          `(cljs.test/async done#
                            (~async-expr
                             (fn [result#]
                               (cljs.test/is false (str "Expected error, got: " result#))
                               (done#))
                             (fn [~err-sym]
                               (cljs.test/is true "Got expected error")
                               ~(when error-pattern
                                  `(cljs.test/is (clojure.string/includes?
                                                  (or (.-message ~err-sym) (str ~err-sym))
                                                  ~error-pattern)))
                               (done#))))
          ;; CLJ
          `(let [result# (promise)
                 error# (promise)]
             (~async-expr
              #(deliver result# %)
              #(deliver error# %))
             (let [~err-sym (deref error# ~timeout-ms nil)
                   res# (deref result# 10 nil)]
               (if ~err-sym
                 (do
                   (clojure.test/is true "Got expected error")
                   ~(when error-pattern
                      `(clojure.test/is (clojure.string/includes?
                                         (.getMessage ~err-sym)
                                         ~error-pattern))))
                 (clojure.test/is false (str "Expected error, got: " res#))))))))))

#?(:clj
   (defmacro test-async-fn
     "Cross-platform async test with custom assertion function.

     Takes an async expression and a function that receives the result.
     The function should contain assertions.

     Usage:
       (deftest my-test
         (test-async-fn
           (async
             (let [x (await (async-delay 10 5))]
               {:value (* x 2)}))
           (fn [result]
             (is (map? result))
             (is (= 10 (:value result))))))"
     ([async-expr assert-fn]
      `(test-async-fn ~async-expr ~assert-fn 2000))
     ([async-expr assert-fn timeout-ms]
      (if (:js-globals &env)
        ;; CLJS
        `(cljs.test/async done#
                          (~async-expr
                           (fn [result#]
                             (~assert-fn result#)
                             (done#))
                           (fn [err#]
                             (cljs.test/is false (str "Unexpected error: " err#))
                             (done#))))
        ;; CLJ
        `(let [result# (promise)
               error# (promise)]
           (~async-expr
            #(deliver result# %)
            #(deliver error# %))
           (let [res# (deref (future
                               (try
                                 (or (deref result# ~timeout-ms nil)
                                     (deref error# ~timeout-ms :timeout))
                                 (catch Exception e# e#)))
                             (+ ~timeout-ms 100)
                             :timeout)]
             (cond
               (= res# :timeout) (throw (Exception. "Test timed out"))
               (instance? Exception res#) (throw res#)
               :else (~assert-fn res#))))))))
