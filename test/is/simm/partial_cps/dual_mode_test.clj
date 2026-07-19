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
  (testing "shadowing that COLLIDES with an in-scope breakpoint fails at
            compile time (the async arm would still treat the call as a
            suspension point)"
    (is (thrown? Exception
                 (eval '(is.simm.partial-cps.async/async+sync true
                                                              (let [await inc]
                                                                (is.simm.partial-cps.async/await 1)))))))
  (testing "shadowing that does NOT collide (await resolves elsewhere, e.g.
            clojure.core/await) is site-free: emitted verbatim and correct"
    (is (= 2 (eval '(is.simm.partial-cps.async/async+sync true
                                                          (let [await inc] (await 1))))))))

(deftest closures-are-opaque
  (testing "a bare breakpoint inside a fn literal is a compile-time error in
            the dual macro (it could never suspend — closures are opaque)"
    (is (thrown? Exception
                 (eval '(is.simm.partial-cps.async/async+sync true
                                                              ((fn [x] (is.simm.partial-cps.async/await x)) 1))))))
  (testing "…and in the plain async macro too"
    (is (thrown? Exception
                 (eval '(is.simm.partial-cps.async/async
                         ((fn [x] (is.simm.partial-cps.async/await x)) 1))))))
  (testing "a fn CONSTRUCTING an async expression is a value and legal"
    (let [make (async+sync true (fn [x] (async (inc (await (async x))))))
          r (atom nil)]
      ((make 41) #(reset! r %) #(reset! r [:err %]))
      (is (= 42 @r))))
  (testing "a fn self-named await does not false-positive the scan"
    (is (fn? (async+sync true (fn await [x] x))))))

(deftest special-form-edges
  (testing "(. obj (method args)) member position is not call position"
    (is (= "bc" (async+sync true (. "abc" (substring (await (async 1))))))))
  (testing "case with seq-shaped test constants compiles and strips"
    (is (= :a (async+sync true (case (await (async 1)) (1 2) :a :b)))))
  (testing "KEYWORD-dispatch case keeps its imap type through the walk
            (a plain {} rebuild misdispatches the compiled hash case*)"
    (is (= [:add :retract :other]
           (async+sync true
                       (mapv (fn [op] (case op
                                        :db/add :add
                                        :db/retract :retract
                                        :other))
                             [:db/add :db/retract :db/x]))))
    (is (= :add (async+sync true (case (await (async :db/add))
                                   :db/add :add
                                   :db/retract :retract
                                   :other)))))
  (testing "a catch binding shadowing await is fine while unused (site-free,
            verbatim) …"
    (is (= 1 (eval '(is.simm.partial-cps.async/async+sync true
                                                          (try (is.simm.partial-cps.async/await
                                                                (is.simm.partial-cps.async/async 1))
                                                               (catch Exception await 2)))))))
  (testing "…but rejected when the shadowed name is called as a breakpoint"
    (is (thrown? Exception
                 (eval '(is.simm.partial-cps.async/async+sync true
                                                              (try (is.simm.partial-cps.async/async 1)
                                                                   (catch Exception await
                                                                          (is.simm.partial-cps.async/await
                                                                           (is.simm.partial-cps.async/async 2))))))))))

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
