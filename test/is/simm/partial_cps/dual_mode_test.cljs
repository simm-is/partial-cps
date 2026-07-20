(ns is.simm.partial-cps.dual-mode-test
  "JS side of the dual-mode foundation: async+sync dispatches at runtime
   (sync? true → stripped form runs on the calling stack and RETURNS the
   value; false → async expr), all-warm `all` resolves before returning,
   deferred branches gather concurrently, first-error rejects, and warm
   awaits inside deep loops keep the stack constant (trampoline stress)."
  (:require [cljs.test :as test :refer-macros [is deftest testing]]
            [is.simm.partial-cps.async :as pa :refer [all async-expr?]
             :refer-macros [async async+sync]]))

(defn- resolve-now!
  "Invoke an async expr; return the value if it resolved synchronously,
   ::pending otherwise."
  [expr]
  (let [r (atom ::pending)]
    (expr (fn [v] (reset! r v)) (fn [e] (reset! r [::err e])))
    @r))

(deftest dual-dispatch
  (testing "sync? true: stripped form, plain value returned"
    (let [f (fn [x sync?] (async+sync sync? (+ 1 (pa/await (async (* 2 x))))))]
      (is (= 7 (f 3 true)))))
  (testing "sync? false: async expr, warm → resolves on the calling stack"
    (let [f (fn [x sync?] (async+sync sync? (+ 1 (pa/await (async (* 2 x))))))
          expr (f 3 false)]
      (is (async-expr? expr))
      (is (= 7 (resolve-now! expr)))))
  (testing "aliased await strips in the sync arm (hygiene)"
    (is (= 42 (async+sync true (pa/await (async 42)))))))

(deftest all-warm-sync-completion
  (testing "all-warm all resolves BEFORE returning, in order"
    (is (= [1 2 3] (resolve-now! (all [(async 1) (async 2) (async 3)])))))
  (testing "empty"
    (is (= [] (resolve-now! (all [])))))
  (testing "first error rejects"
    (let [r (resolve-now! (all [(async 1) (async (throw (ex-info "boom" {})))]))]
      (is (vector? r))
      (is (= ::err (first r))))))

(deftest all-deferred-gather
  (test/async done
              (let [deferred (fn [v ms]
                               (let [f (fn [resolve _reject]
                                         (js/setTimeout #(pa/invoke-continuation resolve v) ms)
                                         nil)]
                                 (set! (.-partial_cps_async_expr f) true)
                                 f))]
                ((all [(deferred :a 20) (async :warm) (deferred :b 5)])
                 (fn [v]
                   (is (= [:a :warm :b] v) "order preserved regardless of completion order")
                   (done))
                 (fn [e]
                   (is (nil? e) "all rejected unexpectedly")
                   (done))))))

(deftest trampoline-depth-stress
  (testing "100k warm awaits inside one async loop keep the stack constant"
    (let [expr (async
                (loop [i 0 acc 0]
                  (if (< i 100000)
                    (recur (inc i) (+ acc (pa/await (async 1))))
                    acc)))]
      (is (= 100000 (resolve-now! expr)))))
  (testing "all over 10k warm branches completes synchronously"
    (let [r (resolve-now! (all (vec (for [i (range 10000)] (async i)))))]
      (is (= 10000 (count r)))
      (is (= 49995000 (reduce + r))))))

(defn ^:export run []
  (test/run-tests 'is.simm.partial-cps.dual-mode-test))
