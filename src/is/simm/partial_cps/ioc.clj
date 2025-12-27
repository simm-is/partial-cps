(ns is.simm.partial-cps.ioc
  "Inversion of control, i.e. transformation into continuation-passing style (CPS)."
  #_(:require [cljs.analyzer :refer [resolve-var resolve-macro-var]]))

(defn resolve-var-cljs [env sym]
  ;; in cljs compilation
  (require 'cljs.analyzer)
  ((resolve 'cljs.analyzer/resolve-var) env sym))

(defn resolve-macro-var-cljs [env sym]
  ;; in cljs compilation  
  (require 'cljs.analyzer)
  ((resolve 'cljs.analyzer/resolve-macro-var) env sym))

(defn var-name [env sym]
  (when (symbol? sym)
    ;; Don't qualify special forms or core symbols that shouldn't be qualified
    (when-not (special-symbol? sym)
      (if (:js-globals env)
        ;; In Clojurescript use cljs.analyzer
        (:name (resolve-var-cljs env sym))
        ;; In Clojure, use the existing resolution logic
        (when-let [v (resolve env sym)]
          (let [nm (:name (meta v))
                nsp (.getName ^clojure.lang.Namespace (:ns (meta v)))]
            (symbol (name nsp) (name nm))))))))

(defn expand-macro
  "Expand a macro form if it's actually a macro. Returns [expanded changed?]"
  [form env]
  (when (and (seq? form) (symbol? (first form)))
    (let [sym (first form)]
      (if (:js-globals env)
        ;; ClojureScript
        (when (:macro (resolve-macro-var-cljs env sym))
          (let [expanded (apply (resolve (:name (resolve-macro-var-cljs env sym)))
                                form env (rest form))]
            [expanded true]))
        ;; Clojure
        (when-let [resolved (resolve env sym)]
          (when (.isMacro resolved)
            (let [expanded (apply resolved form env (rest form))]
              [expanded true])))))))

(defn has-breakpoints?
  [form {:keys [breakpoints recur-target env expansion-cache] :as ctx}]
  ;; Check cache first, then expand if needed
  (let [[form-to-check ctx'] (if-let [cached (when expansion-cache (get @expansion-cache form))]
                               [cached ctx]
                                ;; Not in cache, try to expand
                               (if-let [[expanded _] (expand-macro form env)]
                                 (do
                                   (when expansion-cache
                                     (swap! expansion-cache assoc form expanded))
                                    ;; Recursively check the expansion
                                   [expanded ctx])
                                  ;; Not a macro, use form as-is
                                 [form ctx]))
        sym (when (seq? form-to-check) (first form-to-check))
        resolved-sym (var-name env sym)
        has-term? (contains? breakpoints resolved-sym)
        is-nested-async? (or (= resolved-sym 'is.simm.partial-cps.async/async)
                             (= sym 'is.simm.partial-cps.async/async))]
    (cond
      has-term? true

      (and recur-target (= 'recur sym)) true

      ;; Don't recurse into nested async blocks - they handle their own breakpoints
      is-nested-async? false

      ;; If we just expanded this form and it's different, recurse to check the expansion
      (and (not= form form-to-check) (not has-term?) (not is-nested-async?))
      (has-breakpoints? form-to-check ctx')

      (= 'loop* sym) (some #(has-breakpoints? % (dissoc ctx' :recur-target)) (rest form-to-check))

      (coll? form-to-check) (some #(has-breakpoints? % ctx') form-to-check)

      :else false)))

(defn can-inline?
  [form]
  (or (not (coll? form)) ; inline non-collection literals and symbols
      ; can't inline sets or maps as they throw 'Duplicate key'
      ; but inlining vectors is fine
      (and (vector? form) (every? can-inline? form))))

(declare invert)

(defn resolve-sequentially [ctx coll then]
  (let [[syncs [asn & others]] (split-with #(not (has-breakpoints? % ctx)) coll)]
    (if asn
      (let [syncs (map #(if (can-inline? %) [%] [(gensym) %]) syncs)
            sync-bindings (->> syncs (filter second) (mapcat identity))
            async-binding (with-meta (gensym) (meta asn))
            cont (gensym "cont")]
        `(let [~@sync-bindings]
           (letfn [(~cont [~async-binding]
                     ~(resolve-sequentially
                       (dissoc ctx :sync-recur?) others
                       #(then `[~@(map first syncs) ~async-binding ~@%])))]
             ~(invert (assoc ctx :r cont) asn))))
      (then coll))))

(defn add-env-syms [ctx syms]
  ;; This adds mappings to "true" into the environment map. This doesn't quite
  ;; match what the Clojure compiler does, but I think it's already recommended
  ;; that macros don't depend on the values in the &env map.
  (update ctx :env (fnil into {}) (map (fn [sym] [sym true])) syms))

(defn handle-binding-form
  "Handle binding/with-redefs forms to restore bindings in continuations.

  Intercepts binding forms BEFORE macro expansion to wrap continuations with
  binding restoration. This ensures that when continuations fire after the
  binding scope exits, they restore the outer bindings that were active before
  the binding form was entered."
  [{:keys [r e env] :as ctx} form]
  (let [[macro-sym bindings & body] form
        binding-pairs (partition 2 bindings)
        var-syms (map first binding-pairs)
        ;; Generate symbols to save current values
        saved-syms (map #(gensym (str (name %) "-saved__")) var-syms)
        ;; Generate wrapped continuation symbols
        wrapped-r (gensym "binding-restore-r__")
        wrapped-e (gensym "binding-restore-e__")
        ;; Use plain 'binding' symbol for restoration (works in both CLJ and CLJS)
        binding-sym 'binding]
    (if (has-breakpoints? `(do ~@body) ctx)
      ;; Body has breakpoints - need to wrap continuations
      `(let [~@(interleave saved-syms var-syms)
             ;; Wrapped resolve - restores outer bindings before calling original r
             ~wrapped-r (fn [val#]
                          (~binding-sym [~@(interleave var-syms saved-syms)]
                                        (~r val#)))
             ;; Wrapped reject - restores outer bindings before calling original e
             ~wrapped-e (fn [err#]
                          (~binding-sym [~@(interleave var-syms saved-syms)]
                                        (~e err#)))]
         ;; Transform the EXPANDED binding form with wrapped continuations
         ~(invert (assoc ctx :r wrapped-r :e wrapped-e)
                  ;; Expand the binding macro and transform the expansion
                  (apply (if (:js-globals env)
                           (resolve (:name (resolve-macro-var-cljs env macro-sym)))
                           (resolve env macro-sym))
                         form env (rest form))))
      ;; No breakpoints in body - just expand normally
      (recur ctx
             (apply (if (:js-globals env)
                      (resolve (:name (resolve-macro-var-cljs env macro-sym)))
                      (resolve env macro-sym))
                    form env (rest form))))))

(defn invert-impl
  "Internal implementation of invert. Assumes expansion-map is already in ctx."
  [{:keys [r             ; symbol of continuation function (resolve)
           e             ; symbol of error handling function (raise)
           sync-recur?   ; indicates when synchronous recur is possible
           recur-target  ; symbol of asynchronous recur function if any
           breakpoints   ; map of symbols that break flow to symbols of handlers
           env           ; the current macroexpansion environment
           expansion-map] ; map of original forms to their expansions
    :as ctx}
   form]
  (let [[head & tail] (when (seq? form) form)
        all-ex (if (:js-globals env) :default `Throwable)]
    (cond
      (not (has-breakpoints? form ctx))
      `(~r ~form)

      ;; Special handling for binding/with-redefs BEFORE macro expansion
      (and (symbol? head)
           (let [head-name (name head)
                 head-ns (namespace head)]
             (or (and (= head-name "binding")
                      (or (= head-ns "clojure.core")
                          (= head-ns "cljs.core")
                          (nil? head-ns)))
                 (and (= head-name "with-redefs")
                      (or (= head-ns "cljs.core")
                          (nil? head-ns))))))
      (handle-binding-form ctx form)

      ;; Check if this is a macro and expand it
      (if (and head (:js-globals env))
        ;; use cljs.analyzer to find macro var info
        (:macro (resolve-macro-var-cljs env head))
        ;; use normal Clojure resolve
        (let [resolved (when (symbol? head) (resolve env head))]
          (and resolved (.isMacro resolved))))
      (recur ctx
             (apply (if (:js-globals env)
                      (resolve (:name (resolve-macro-var-cljs env head)))
                      (resolve env head))
                    form env tail))

      (or (special-symbol? head) (= head 'let) (= head 'letfn) (= head 'loop) (= head 'fn))
      (case head

        (quote var fn* fn deftype* reify* clojure.core/import*)
        `(~r ~form)

        if
        (let [[con left right & unexpected-others] tail
              cont (gensym "cont")]
          (if (has-breakpoints? con ctx)
            (let [ctx' (dissoc ctx :sync-recur?)]
              `(letfn [(~cont [con#] (if con# ~(invert-impl ctx' left)
                                         ~(invert-impl ctx' right)
                                         ~@unexpected-others))]
                 ~(invert-impl (assoc ctx :r cont) con)))
            `(if ~con ~(invert-impl ctx left) ~(invert-impl ctx right))))

        case*
        ;; Handle both CLJ and CLJS case* formats
        ;; CLJ: (case* ge shift mask default imap ...)
        ;;      where imap is {hash [test-constant result-expr], ...}
        ;; CLJS: (case* test-expr [[k1] [k2] ...] [v1 v2 ...] default)
        ;;       where keys are vectors of matching values, vals are corresponding expressions
        (if (:js-globals env)
          ;; CLJS format: (case* test-expr [[k1] [k2] ...] [v1 v2 ...] default)
          ;; Only 4 args: test-expr, keys-vec, vals-vec, default
          (let [[test-expr keys-vec vals-vec default-expr] tail
                ;; Transform each value expression to call r
                inverted-vals (mapv #(invert-impl ctx %) vals-vec)
                ;; Transform default expression
                inverted-default (invert-impl ctx default-expr)]
            `(case* ~test-expr ~keys-vec ~inverted-vals ~inverted-default))
          ;; CLJ format
          (let [[ge shift mask default imap & args] tail
                imap (reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] (invert-impl ctx v))))
                                {} imap)]
            `(case* ~ge ~shift ~mask ~(invert-impl ctx default) ~imap ~@args)))

        let*
        (let [bindings-vec (first tail)
              bindings-pairs (partition 2 bindings-vec)
              [syncs [[sym asn] & others]] (split-with #(not (has-breakpoints? (second %) ctx)) bindings-pairs)
              cont (gensym "cont")
              updated-ctx (add-env-syms ctx (map first syncs))
              generated-form (if asn
                               ;; We have an async binding
                               `(let* [~@(mapcat identity syncs)]
                                      (letfn [(~cont [async-value#]
                                                (let* [~sym async-value#]
                                                      ~(invert-impl (add-env-syms (dissoc updated-ctx :sync-recur?) [sym])
                                                                    (if (seq others)
                                                                      `(let* [~@(mapcat identity others)]
                                                                             ~@(rest tail))
                                                                      `(do ~@(rest tail))))))]
                                        ~(invert-impl (assoc updated-ctx :r cont) asn)))
                               ;; No async bindings
                               `(let* [~@(mapcat identity syncs)]
                                      ~(invert-impl updated-ctx `(do ~@(rest tail)))))]
          generated-form)

        letfn*
        `(letfn* ~(first tail)
                 ~(invert-impl (add-env-syms ctx (->> tail first (partition 2) (map first)))
                               `(do ~@(rest tail))))

        do
        (let [[syncs [asn & others]] (split-with #(not (has-breakpoints? % ctx)) tail)
              cont (gensym "cont")]
          (if asn
            `(do ~@syncs
                 ~(if others
                    `(letfn [(~cont [_#] ~(invert-impl (dissoc ctx :sync-recur?)
                                                       `(do ~@others)))]
                       ~(invert-impl (assoc ctx :r cont) asn))
                    (invert-impl ctx asn)))
            `(~r ~form)))

        loop*
        (let [[binds & body] tail
              bind-names (->> binds (partition 2) (map first))]
          (cond
            (has-breakpoints? binds ctx)
            (invert-impl ctx `(let [~@binds]
                                (loop [~@(interleave bind-names bind-names)]
                                  ~@body)))

            (has-breakpoints? body (dissoc ctx :recur-target))
            (let [recur-target (gensym "recur")
                  updated-ctx (add-env-syms ctx bind-names)]
              `(letfn [(~recur-target [~@bind-names]
                         (loop [~@(interleave bind-names bind-names)]
                           ~(invert-impl (assoc updated-ctx
                                                :sync-recur? true
                                                :recur-target recur-target)
                                         `(do ~@body))))]
                 (let [~@binds] (~recur-target ~@bind-names))))

            :else `(~r ~form)))

        recur
        (cond
          (and sync-recur? (not (has-breakpoints? form (dissoc ctx :recur-target))))
          form

          recur-target
          ;; Activate trampoline by wrapping in a thunk
          (resolve-sequentially ctx tail
                                (fn [args]
                                  `(is.simm.partial-cps.runtime/->thunk (fn [] (~recur-target ~@args)))))

          :else (throw (ex-info "Can't recur outside loop" {:form form})))

        try
        (let [catch-or-finally? #(and (seq? %) (#{'catch 'finally} (first %)))
              [body cfs] (split-with #(not (catch-or-finally? %)) tail)
              [catches finally] (if (->> cfs last first (= 'finally))
                                  [(drop-last cfs) (rest (last cfs))]
                                  [cfs])
              fin-do (gensym "finally-do")
              fin (gensym "finally")
              fin-throw (gensym "finally-throw")
              cat (gensym "catch")
              v (gensym) t (gensym)]
          `(letfn [(~fin-do [~v ~t]
                     (try ~(invert-impl ctx `(do ~@finally (if ~t (throw ~t) ~v)))
                          (catch ~all-ex t# (~e t#))))
                   (~fin [v#] (~fin-do v# nil))
                   (~fin-throw [t#] (~fin-do nil t#))
                   (~cat [t#]
                     (try
                       (try (throw t#)
                            ~@(map (fn [[sym cls bnd & body]]
                                     `(~sym ~cls ~bnd
                                            ~(invert-impl (assoc (add-env-syms ctx [bnd]) :r fin :e fin-throw)
                                                          `(do ~@body))))
                                   catches))
                       (catch ~all-ex t# (~fin-do nil t#))))]
             (try ~(invert-impl (assoc ctx :r fin :e cat) `(do ~@body))
                  (catch ~all-ex t# (~cat t#)))))

        throw
        (resolve-sequentially ctx tail (fn [args] `(throw ~@args)))

        new
        (let [[cls & args] tail]
          (resolve-sequentially ctx args (fn [args] `(~r (new ~cls ~@args)))))

        .
        (let [[subject second] tail
              [method & args] (if (seq? second) second (rest tail))]
          (if (symbol? subject)
            (resolve-sequentially ctx args
                                  (fn [args] `(~r (. ~subject ~method ~@args))))
            (resolve-sequentially ctx `[~subject ~@args]
                                  (fn [[subject & args]]
                                    `(~r (. ~subject ~method ~@args))))))

        set!
        (let [[subject & args] tail
              [_ object & field-args] (when (and (seq? subject)
                                                 (= '. (first subject)))
                                        subject)]
          (if (and object (has-breakpoints? object ctx))
            (resolve-sequentially ctx [object args]
                                  (fn [[object args]] `(~r (set! (. ~object ~@field-args) ~@args))))
            (resolve-sequentially ctx args
                                  (fn [args] `(~r (set! ~subject ~@args))))))

        def
        (let [[name & value] tail]
          (if (has-breakpoints? (first value) ctx)
            (resolve-sequentially ctx value
                                  (fn [value] `(~r (def ~name ~@value))))
            `(~r ~form)))

        (monitor-enter monitor-exit)
        (throw (ex-info (str "Cannot use " head " with async breakpoints. "
                             "Holding a lock across an await point would cause deadlocks. "
                             "Consider using an atom, ref, or agent instead.")
                        {:form form :special-form head}))

        (throw (ex-info (str "Unsupported special symbol [" head "]")
                        {:unknown-special-form head :form form})))

      ;; Invoke termination handler, e.g. do-await
      (contains? breakpoints (var-name env head))
      (let [handler (resolve (breakpoints (var-name env head)))]
        (resolve-sequentially ctx (rest form) (handler ctx r e)))

      (seq? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(seq form))))

      (vector? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(with-meta (vec form) (meta form)))))

      (set? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(with-meta (set form) (meta form)))))

      (map? form)
      (resolve-sequentially ctx (mapcat identity form)
                            (fn [form] `(~r ~(with-meta (apply array-map form) (meta form)))))

      :else (throw (ex-info (str "Unsupported form [" form "]")
                            {:form form})))))

(defn invert
  "CPS inversion with macro expansion caching.
   Creates an expansion cache atom that persists across has-breakpoints? calls
   to avoid re-expanding the same macros multiple times."
  [ctx form]
  (let [expansion-cache (atom {})
        ctx-with-cache (assoc ctx :expansion-cache expansion-cache)]
    (invert-impl ctx-with-cache form)))
