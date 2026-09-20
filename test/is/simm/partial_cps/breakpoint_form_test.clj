(ns is.simm.partial-cps.breakpoint-form-test
  "A breakpoint handler receives the call form, with its source position."
  (:require [clojure.test :refer [deftest is]]
            [is.simm.partial-cps.ioc :as ioc]
            [is.simm.partial-cps.runtime :as runtime]))

(defn here
  "Breakpoint: evaluates to [line column] of its own call site."
  [& _]
  (throw (ex-info "here called outside of cps" {})))

(defn here-handler [ctx r _e]
  (let [{:keys [line column]} (meta (:form ctx))]
    (fn [_args] `(~r [~line ~column]))))

(defmacro positions [& body]
  (let [r (gensym) e (gensym)
        params {:r r :e e :env &env :breakpoints {`here `here-handler}}]
    `(let [result# (volatile! nil)
           ~r (fn [v#] (vreset! result# v#))
           ~e (fn [t#] (throw t#))]
       (loop [x# ~(ioc/invert params (cons 'do body))]
         (when (instance? is.simm.partial_cps.runtime.Thunk x#)
           (recur ((.-f ^is.simm.partial_cps.runtime.Thunk x#)))))
       @result#)))

(deftest the-handler-sees-where-each-call-is
  (let [[a b c] (positions
                 (let [first-site (here)
                       branch (if (odd? 1)
                                (here)
                                (here))]
                   [first-site branch (when true (here))]))]
    (is (every? (fn [[line column]] (and (pos-int? line) (pos-int? column))) [a b c]))
    (is (= 3 (count (distinct [a b c]))) "three call sites, three positions")))
