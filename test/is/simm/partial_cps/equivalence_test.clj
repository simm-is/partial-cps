(ns is.simm.partial-cps.equivalence-test
  "The dual-mode law as a generative property: for any body in the supported
   grammar, the stripped sync arm and the CPS async arm run all-warm must
   agree on (1) result value, (2) side-effect order, (3) thrown exception
   class. Deterministic: seeded generator, identical corpus every run.

   This is the library-owned form of the law its consumers rely on
   (sync ≡ async-all-warm); it gates any refactor of the transform."
  (:require [clojure.test :refer [deftest is]]
            [is.simm.partial-cps.async :as pa :refer [async async+sync await]]))

;; ---------------------------------------------------------------------------
;; deterministic generator

(defn- pick [^java.util.Random r coll]
  (let [v (vec coll)] (nth v (.nextInt r (count v)))))

(defn- rint [^java.util.Random r n] (.nextInt r (int n)))

(defn gen-expr
  "Generate an expression form over in-scope `locals`. `log` is captured by
   the runner. Await sites wrap all-warm (async …) sub-expressions so the
   async arm never truly suspends. Numeric leaves keep arithmetic contexts
   total; nil/string/keyword leaves exercise value plumbing elsewhere."
  [^java.util.Random r locals depth]
  (let [num-leaf (fn [] (if (and (seq locals) (zero? (rint r 3)))
                          (pick r locals)
                          (rint r 100)))
        any-leaf (fn [] (pick r (concat [1 42 nil true "s" :k] locals)))
        sub (fn [ls] (gen-expr r ls (dec depth)))
        choice (if (zero? depth)
                 (pick r [:leaf :leaf :leaf :effect])
                 (pick r [:leaf :effect :await :await :let :if :do :loop
                          :case :try :vec :map-lit :thread :when :cond-arrow]))]
    (case choice
      :leaf (any-leaf)
      :effect `(do (swap! ~'log conj ~(rint r 1000)) ~(any-leaf))
      :await `(await (async ~(sub locals)))
      :let (let [sym (gensym "x")]
             `(let [~sym ~(sub locals)]
                ~(sub (conj locals sym))))
      :if `(if ~(sub locals) ~(sub locals) ~(sub locals))
      :do `(do ~(sub locals) ~(sub locals))
      :loop (let [i (gensym "i") acc (gensym "acc") v (gensym "v")]
              `(loop [~i ~(inc (rint r 3)) ~acc 0]
                 (if (pos? ~i)
                   (let [~v ~(sub (conj locals acc))]
                     (recur (dec ~i) (+ ~acc ~i)))
                   [~acc ~(sub locals)])))
      :case `(case ~(rint r 3)
               0 ~(sub locals)
               (1 2) ~(sub locals)
               ~(sub locals))
      :try `(try
              ~(if (zero? (rint r 3))
                 `(do (swap! ~'log conj :pre-throw)
                      (throw (ex-info "boom" {:i ~(rint r 10)}))
                      ~(any-leaf))
                 (sub locals))
              (~'catch clojure.lang.ExceptionInfo e#
                       [:caught ~(sub locals)]))
      :vec [(sub locals) (sub locals)]
      :map-lit {:a (sub locals) :b (sub locals)}
      :thread `(-> ~(num-leaf) (+ ~(rint r 10)) (vector ~(sub locals)))
      :when `(when ~(sub locals) ~(sub locals))
      :cond-arrow `(cond-> ~(num-leaf)
                     ~(sub locals) (+ 1)))))

;; ---------------------------------------------------------------------------
;; runners — both arms compiled per generated body via eval

(defn- compile-sync [form]
  (eval `(fn [~'log] (async+sync true ~form))))

(defn- compile-async-all-warm [form]
  ;; run the CPS arm and require synchronous completion (all awaits warm)
  (eval `(fn [~'log]
           (let [res# (volatile! ::none) err# (volatile! ::none)]
             ((async ~form) #(vreset! res# %) #(vreset! err# %))
             (when-not (= ::none @err#) (throw @err#))
             (when (= ::none @res#)
               (throw (ex-info "async arm did not complete synchronously all-warm"
                               {:form '~form})))
             @res#))))

(defn- outcome [f]
  (let [log (atom [])]
    (try {:value (f log) :log @log}
         (catch Throwable t
           {:error (class t) :log @log}))))

(deftest sync≡async-all-warm-generative
  (let [r (java.util.Random. 42)]
    (dotimes [i 300]
      (let [form (gen-expr r [] 4)
            s (outcome (compile-sync form))
            a (outcome (compile-async-all-warm form))]
        (is (= s a)
            (str "case " i " diverged:\n" (pr-str form)
                 "\nsync:  " (pr-str s)
                 "\nasync: " (pr-str a)))))))

(deftest exception-equality-generative
  ;; bodies biased toward throwing: uncaught exceptions must reach the error
  ;; continuation with the same class as the sync throw
  (let [r (java.util.Random. 4242)]
    (dotimes [i 100]
      (let [form `(do ~(gen-expr r [] 3)
                      (when (zero? ~(rint r 2))
                        (throw (ex-info "unhandled" {})))
                      ~(gen-expr r [] 2))
            s (outcome (compile-sync form))
            a (outcome (compile-async-all-warm form))]
        (is (= s a)
            (str "exception case " i " diverged:\n" (pr-str form)
                 "\nsync:  " (pr-str s)
                 "\nasync: " (pr-str a)))))))
