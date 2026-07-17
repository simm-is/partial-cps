(ns is.simm.partial-cps.dual-mode-test
  "JVM side of the dual-mode foundation: async+sync emits ONLY the stripped
   synchronous form on :clj (no dead async arm, sync? not evaluated), the
   strip is hygienic (aliased/qualified await strips; shadowing rejected at
   compile time; quotes untouched), and `all`/`async-expr?` behave."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.partial-cps.async :as pa :refer [async async+sync all async-expr? await]]))

(deftest clj-emits-sync-only
  (testing "dual body returns the plain value on the JVM"
    (let [f (fn [x sync?] (async+sync sync? (+ 1 (await (async (* 2 x))))))]
      ;; sync? is irrelevant on :clj — both calls run the stripped form
      (is (= 7 (f 3 true)))
      (is (= 7 (f 3 false)))))
  (testing "sync? expression is NOT evaluated on :clj"
    (let [evals (atom 0)]
      (is (= 5 (async+sync (do (swap! evals inc) true) (+ 2 3))))
      (is (zero? @evals)))))

(deftest strip-hygiene
  (testing "ALIASED await strips (the konserve/PSS bare-symbol strip missed this)"
    (is (= 42 (async+sync true (pa/await (async 42))))))
  (testing "fully-qualified await strips"
    (is (= 42 (async+sync true (is.simm.partial-cps.async/await (async 42))))))
  (testing "nested async+sync strips as same-mode"
    (is (= 10 (async+sync true (+ 1 (async+sync true (pa/await (async 9))))))))
  (testing "quoted forms are untouched"
    (is (= '(await x) (async+sync true (quote (await x))))))
  (testing "await inside a threading macro strips (macroexpansion parity)"
    (is (= 43 (async+sync true (-> (await (async 42)) inc))))))

(deftest shadowing-rejected
  (testing "locally binding await inside a dual body fails at compile time"
    (is (thrown? Exception
                 (eval '(is.simm.partial-cps.async/async+sync true
                                                              (let [await inc] (await 1))))))))

(deftest all-on-jvm
  (testing "empty input resolves []"
    (let [r (atom ::pending)]
      ((all []) (fn [v] (reset! r v)) (fn [e] (reset! r [:err e])))
      (is (= [] @r))))
  (testing "warm branches resolve synchronously, in order"
    (let [r (atom ::pending)]
      ((all [(async 1) (async 2) (async 3)])
       (fn [v] (reset! r v)) (fn [e] (reset! r [:err e])))
      (is (= [1 2 3] @r))))
  (testing "first error rejects; result never resolves"
    (let [r (atom ::pending)]
      ((all [(async 1) (async (throw (ex-info "boom" {}))) (async 3)])
       (fn [v] (reset! r [:resolved v]))
       (fn [e] (reset! r [:err (ex-message e)])))
      (is (= [:err "boom"] @r))))
  (testing "each expr invoked exactly once"
    (let [invocations (atom 0)
          expr (fn [resolve _reject] (swap! invocations inc) (resolve 1) nil)
          r (atom ::pending)]
      ((all [expr expr]) (fn [v] (reset! r v)) (fn [_] nil))
      (is (= [1 1] @r))
      (is (= 2 @invocations)))))

(deftest marker
  (is (async-expr? (async 42)))
  (is (async-expr? (all [(async 1)])))
  (is (not (async-expr? (fn [a b] (a 1)))))
  (is (not (async-expr? 42)))
  (is (not (async-expr? nil))))
