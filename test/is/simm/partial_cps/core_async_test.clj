(ns is.simm.partial-cps.core-async-test
  "Unit coverage for the optional core.async↔partial-cps adapter. Run under the
   :test (or :core-async) alias, which provides core.async. The cljs side is
   exercised downstream (yggdrasil + spindel suites + the cross-runtime e2es)."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.partial-cps.core-async :as ca]
            [is.simm.partial-cps.async :refer [async await]]
            [clojure.core.async :refer [chan put! close! <!!]]))

(defn- closed-chan [v]
  (let [c (chan 1)] (when (some? v) (put! c v)) (close! c) c))

(deftest ->chan-normalizes-the-three-shapes
  (testing "plain value (incl. nil) → channel yielding the datum"
    (is (= 42 (ca/unwrap-result (<!! (ca/->chan 42)))))
    (is (nil? (ca/unwrap-result (<!! (ca/->chan nil)))) "genuine nil survives the sentinel"))
  (testing "core.async channel → passed through"
    (is (= 7 (ca/unwrap-result (<!! (ca/->chan (closed-chan 7)))))))
  (testing "partial-cps CPS fn → invoked, datum delivered"
    (is (= 99 (ca/unwrap-result (<!! (ca/->chan (fn [resolve _] (resolve 99)))))))
    (is (nil? (ca/unwrap-result (<!! (ca/->chan (fn [resolve _] (resolve nil)))))))))

(deftest ->chan-carries-errors
  (testing "a CPS raise delivers the error so unwrap-result re-throws"
    (is (thrown-with-msg? Exception #"boom"
                          (ca/unwrap-result (<!! (ca/->chan (fn [_ raise] (raise (ex-info "boom" {})))))))))
  (testing "a CPS that throws synchronously is caught + re-thrown"
    (is (thrown-with-msg? Exception #"sync-throw"
                          (ca/unwrap-result (<!! (ca/->chan (fn [_ _] (throw (ex-info "sync-throw" {}))))))))))

(deftest rewrap-preserves-message-ex-data-and-cause
  (testing "the superv.async boundary convention: re-throw wraps in a fresh ex-info
            with the original message + ex-data, and the original as cause"
    (let [orig    (ex-info "original" {:code 42})
          thrown  (try (ca/unwrap-result (<!! (ca/->chan (fn [_ raise] (raise orig)))))
                       (catch clojure.lang.ExceptionInfo e e))]
      (is (= "original" (.getMessage thrown)) "message preserved")
      (is (= {:code 42} (ex-data thrown)) "ex-data preserved")
      (is (identical? orig (.getCause thrown)) "original kept as cause (full trace)"))))

(deftest ->cps-and-await-roundtrip
  (testing "->cps makes value/chan/cps await-able inside a partial-cps async block"
    (is (= [42 7 99]
           (<!! (ca/->chan
                 (async
                  [(await (ca/->cps 42))
                   (await (ca/->cps (closed-chan 7)))
                   (await (ca/->cps (fn [resolve _] (resolve 99))))])))))))

(deftest channel-error-propagates-into-partial-cps-error-path
  (testing "an error VALUE taken off a channel surfaces via partial-cps's RAISE
            (the error continuation), not resolve — awaiting it inside an `async`
            block routes to the block's error path, so unwrap-result re-throws it"
    (is (thrown-with-msg? Exception #"chan-err"
                          (ca/unwrap-result
                           (<!! (ca/->chan
                                 (async (await (ca/->cps (closed-chan (ex-info "chan-err" {:x 1})))))))))
        "awaiting a channel-carried error inside an async block re-throws it")
    ;; and the error reaches the result channel AS an error value (the async error
    ;; path fired) — never silently resolved as a normal datum.
    (let [v (<!! (ca/->chan
                  (async (await (ca/->cps (closed-chan (ex-info "chan-err" {})))))))]
      (is (ca/error? v) "the bridged error arrives as an error value, never a datum")))
  (testing "a SUCCESS value still resolves through the very same async/await path"
    (is (= 5 (ca/unwrap-result
              (<!! (ca/->chan (async (inc (await (ca/->cps (closed-chan 4))))))))))))

(deftest sync-or-cps-passthrough-vs-wrap
  (testing "{:sync? true} passes the value straight through"
    (is (= 5 (ca/sync-or-cps 5 {:sync? true}))))
  (testing "{:sync? false} wraps a channel result as an await-able CPS"
    (is (= 5 (<!! (ca/->chan (ca/sync-or-cps (closed-chan 5) {:sync? false})))))))

(deftest chan?-predicate
  (is (true? (ca/chan? (chan))))
  (is (false? (ca/chan? 42)))
  (is (false? (ca/chan? (fn [_ _])))))
