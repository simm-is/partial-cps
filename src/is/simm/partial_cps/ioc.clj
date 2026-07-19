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
        ;; In ClojureScript use cljs.analyzer — resolve as a var, FALLING BACK to a
        ;; macro var so a macro-named breakpoint (e.g. `cljs.core/await` on cljs >= 1.12)
        ;; resolves to its fully-qualified name and can be matched in `breakpoints`.
        (or (:name (resolve-var-cljs env sym))
            (:name (resolve-macro-var-cljs env sym)))
        ;; In Clojure — use str instead of .getName for SCI namespace compatibility
        (when-let [v (resolve env sym)]
          (let [m (meta v)
                nm (:name m)
                nsp (:ns m)]
            (when (and nm nsp)
              (symbol (str nsp) (name nm)))))))))

(defn expand-macro
  "Expand a macro form if it's actually a macro. Returns [expanded changed?]"
  [form env]
  (when (and (seq? form) (symbol? (first form)))
    (let [sym (first form)]
      (if (:js-globals env)
        ;; ClojureScript
        (let [macro-info (resolve-macro-var-cljs env sym)]
          (when (:macro macro-info)
            (let [expanded (apply (resolve (:name macro-info))
                                  form env (rest form))]
              [expanded true])))
        ;; Clojure — use meta check for SCI compatibility (SCI vars lack .isMacro field)
        (when-let [resolved (resolve env sym)]
          (when (:macro (meta resolved))
            (let [expanded (apply resolved form env (rest form))]
              [expanded true])))))))

(defn has-breakpoints?
  [form {:keys [breakpoints recur-target env expansion-cache breakpoint-cache] :as ctx}]
  ;; Check breakpoint-cache first to avoid redundant sub-tree traversals.
  ;; Cache key includes recur-target presence since it affects whether `recur` is a breakpoint.
  (let [cache-key (when breakpoint-cache [form (some? recur-target)])]
    (if-let [cached-result (when cache-key (get @breakpoint-cache cache-key))]
      cached-result
      ;; A registered breakpoint is TERMINAL: match it on the ORIGINAL operator WITHOUT
      ;; macroexpanding, so a breakpoint that is ALSO a macro (e.g. `cljs.core/await` on
      ;; cljs >= 1.12) is never expanded (its expansion would fire the macro's own assert).
      ;; :extra-site? extends what counts as a site (the direct emitter counts nested
      ;; async/async+sync forms — same-mode semantics rewrite them too). Callers using
      ;; :extra-site? must supply their OWN cache atoms — cache entries are
      ;; site-predicate-dependent.
      (if (and (seq? form) (symbol? (first form))
               (or (contains? breakpoints (var-name env (first form)))
                   (when-let [extra (:extra-site? ctx)]
                     (extra env (first form)))))
        (do (when cache-key (swap! breakpoint-cache assoc cache-key true)) true)
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
              has-term? (or (contains? breakpoints resolved-sym)
                            (boolean (when-let [extra (:extra-site? ctx)]
                                       (and (symbol? sym) (extra env sym)))))
              result (cond
                       has-term? true

                       (and recur-target (= 'recur sym)) true

                     ;; If we just expanded this form and it's different, recurse to check the expansion
                       (and (not= form form-to-check) (not has-term?))
                       (has-breakpoints? form-to-check ctx')

                       (= 'loop* sym) (some #(has-breakpoints? % (dissoc ctx' :recur-target)) (rest form-to-check))

                       (coll? form-to-check) (some #(has-breakpoints? % ctx') form-to-check)

                       :else false)]
        ;; Cache the result for this form
          (when cache-key
            (swap! breakpoint-cache assoc cache-key (boolean result)))
          result)))))

(def ^:private pcps-async-sym 'is.simm.partial-cps.async/async)
(def ^:private pcps-dual-sym 'is.simm.partial-cps.async/async+sync)

(defn- macro-name
  "Resolve sym to the fully-qualified name of the MACRO it refers to, or nil.
   Needed because cljs.analyzer/resolve-var GUESSES a current-ns name for
   unresolvable runtime symbols (macros have no runtime var), so var-name's
   var-first `or` never reaches its macro fallback for macro-only names."
  [env sym]
  (when (and (symbol? sym) (not (special-symbol? sym)))
    (if (:js-globals env)
      (:name (resolve-macro-var-cljs env sym))
      (when-let [v (resolve env sym)]
        (when (:macro (meta v))
          (let [m (meta v)]
            (symbol (str (:ns m)) (name (:name m)))))))))

(defn binding-symbols
  "All symbols BOUND by a binding form: plain symbols, & rest args, vector and
   map destructuring (:keys/:syms/:strs incl. namespaced, :as aliases, nested
   forms), ignoring :or defaults and `&` itself."
  [x]
  (cond
    (symbol? x) (when-not (= '& x) [x])
    (vector? x) (mapcat binding-symbols x)
    (map? x) (mapcat (fn [[k v]]
                       (cond
                         (and (keyword? k) (#{"keys" "syms" "strs"} (name k)))
                         (map (comp symbol name) v)
                         (= :as k) [v]
                         (= :or k) nil
                         (keyword? k) nil
                         :else (binding-symbols k)))
                     x)
    :else nil))

(defn bind-locals
  "Extend the macroexpansion env so `var-name`/`expand-macro` see `syms` as
   locals: on clj `&env` is a map keyed by local symbols (`resolve` only
   checks key presence); on cljs the analyzer looks in `:locals`."
  [env syms]
  (if (:js-globals env)
    (update env :locals (fnil into {}) (map (fn [s] [s {:name s}])) syms)
    (into (or env {}) (map (fn [s] [s true])) syms)))

(defn- guard-shadowing!
  "Reject local bindings NAMED await/async inside a dual body. The sync-arm
   strip is env-threaded and would resolve the local correctly — but the
   ASYNC arm's transform cannot (on cljs its env extension does not reach the
   analyzer's :locals), so the two arms would diverge. Fail the build instead."
  [syms form]
  (doseq [s syms]
    (when (and (symbol? s) (#{"await" "async"} (name s)))
      (throw (ex-info (str "async+sync: locally binding `" s "` inside a dual "
                           "body is unsupported (the async arm would still "
                           "treat it as a suspension point) — rename the local")
                      {:binding s :form form})))))

(defn assert-no-bare-breakpoints!
  "Closures are OPAQUE to the CPS transform: a bare breakpoint inside a
   `fn*`/`reify*`/`deftype*` body can never suspend, so it is a compile-time
   error in BOTH arms rather than a silent divergence (the strip used to
   erase it, the async arm left it to fail at runtime). Nested
   `async`/`async+sync` bodies open their own transform scope and are
   skipped — a fn CONSTRUCTING an async expression is a value and legal.
   Env-threaded so locals shadowing a breakpoint name (fn self-name, params,
   lets) are not false positives. Breakpoints hidden behind unexpanded user
   macros are not detected (the closure is never expanded by either arm)."
  [form {:keys [breakpoints env]} outer-form]
  (letfn [(scan-fn* [env f]
            (let [tail (rest f)
                  [nm tail] (if (symbol? (first tail))
                              [(first tail) (rest tail)]
                              [nil tail])
                  env (if nm (bind-locals env [nm]) env)
                  arities (if (vector? (first tail)) [tail] tail)]
              (doseq [[params & body] arities]
                (let [env' (bind-locals env (binding-symbols params))]
                  (run! #(scan env' %) body)))))
          (scan [env f]
            (cond
              (seq? f)
              (let [head (first f)]
                (cond
                  (= 'quote head) nil

                  (and (symbol? head)
                       (or (= pcps-async-sym (macro-name env head))
                           (= pcps-dual-sym (macro-name env head))))
                  nil

                  (and (symbol? head)
                       (contains? breakpoints (var-name env head)))
                  (throw (ex-info (str "breakpoint `" head "` inside a fn literal can "
                                       "never suspend (closures are opaque to the CPS "
                                       "transform) — hoist it out of the fn, or make the "
                                       "fn body its own (async ...)")
                                  {:breakpoint head :fn-form outer-form :site f}))

                  :else
                  (case head
                    (let* loop*)
                    (let [[_ bindings & body] f]
                      (loop [env env, ps (seq (partition 2 bindings))]
                        (if-let [[[b e] & more] ps]
                          (do (scan env e)
                              (recur (bind-locals env (binding-symbols b)) more))
                          (run! #(scan env %) body))))

                    fn*
                    (scan-fn* env f)

                    letfn*
                    (let [[_ bindings & body] f
                          env' (bind-locals env (take-nth 2 bindings))]
                      ;; fn names are in scope in every fn body (mutual recursion)
                      (run! #(scan env' %) (take-nth 2 (rest bindings)))
                      (run! #(scan env' %) body))

                    catch
                    (let [[_ _cls b & body] f
                          env' (bind-locals env [b])]
                      (run! #(scan env' %) body))

                    (run! #(scan env %) f))))
              (coll? f) (run! #(scan env %) f)
              :else nil))]
    (scan env form))
  nil)

(declare strip-breakpoints*)

(defn strip-breakpoints
  "Rewrite `form` to its SYNCHRONOUS shape:
   - every resolved breakpoint call `(await x)` becomes `x`,
   - a nested resolved `(async & body)` becomes `(do & body)`,
   - a nested resolved `(async+sync s & body)` becomes `(do & body)`
     (same-mode semantics: a synchronous context strips all the way down).
   Resolution uses the SAME `var-name` lookup (after the same macroexpansion
   discipline) as the async transform — a breakpoint is matched on the
   ORIGINAL operator before expansion — so whatever the async arm would
   treat as a suspension point, the sync arm un-suspends.

   The walk is ENV-THREADED: locals introduced by let*/loop*/letfn*/catch
   extend the resolution env, so macros expanding inside the body see their
   enclosing locals in &env, and resolution matches the compiler's scoping.
   fn*/reify*/deftype* are OPAQUE (matching the async transform, which never
   descends into closures); a bare breakpoint inside one is a compile-time
   error via `assert-no-bare-breakpoints!`. `(. obj (method args))` member
   position is never treated as call position. Quoted forms are untouched;
   locals shadowing await/async are rejected at compile time (the async arm
   cannot handle them).

   CAVEAT: macros in the body are expanded eagerly at strip time and the
   expansion is emitted — an &env-sensitive macro sees the threaded env of
   this walk, which now includes body locals, but expansion happens once per
   arm and the two arms expand independently.

   Site-free subtrees are emitted VERBATIM (the direct-emission analogue of
   the CPS emitter's `(r form)` fast path): `has-breakpoints?` with an
   :extra-site? counting nested async/async+sync forms decides, over caches
   private to this walk (cache entries are site-predicate-dependent, so they
   must never be shared with an invert pass)."
  [form {:keys [breakpoints env] :as ctx}]
  (let [ctx (assoc ctx
                   :extra-site? (fn [env head]
                                  (or (= pcps-async-sym (macro-name env head))
                                      (= pcps-dual-sym (macro-name env head))))
                   :expansion-cache (atom {})
                   :breakpoint-cache (atom {}))]
    (strip-breakpoints* form ctx)))

(defn- strip-breakpoints*
  [form {:keys [breakpoints env] :as ctx}]
  (letfn [(site-free? [env f]
            (not (has-breakpoints? f (assoc ctx :env env))))
          (walk-bindings [env bindings]
            ;; sequential let-semantics: each init sees the previous bindings
            (loop [env env, ps (seq (partition 2 bindings)), out []]
              (if-let [[[b e] & more] ps]
                (let [e' (walk env e)
                      syms (binding-symbols b)]
                  (guard-shadowing! syms bindings)
                  (recur (bind-locals env syms) more (conj out b e')))
                [env out])))
          (walk-case* [env f]
            ;; only RESULT positions are expressions; test constants must not
            ;; be walked (a list-shaped constant is not a call)
            (if (:js-globals env)
              ;; cljs: (case* test keys-vec vals-vec default)
              (let [[_ test keys-vec vals-vec default] f]
                (with-meta (list 'case* (walk env test) keys-vec
                                 (mapv #(walk env %) vals-vec) (walk env default))
                  (meta f)))
              ;; clj: (case* ge shift mask default imap & args), imap {hash [const expr]}
              (let [[_ ge shift mask default imap & more] f
                    imap' (reduce-kv (fn [m k [c e]] (assoc m k [c (walk env e)])) {} imap)]
                (with-meta (list* 'case* ge shift mask (walk env default) imap' more)
                  (meta f)))))
          (walk [env f]
            (cond
              ;; verbatim fast path: nothing to rewrite anywhere below
              (site-free? env f) f

              (seq? f)
              (let [head (first f)]
                (cond
                  (= 'quote head) f

                  (and (symbol? head)
                       (contains? breakpoints (var-name env head)))
                  (do (assert (= 2 (count f))
                              (str "breakpoint call must have exactly one argument: " f))
                      (walk env (second f)))

                  (and (symbol? head)
                       (= pcps-async-sym (macro-name env head)))
                  (with-meta (cons 'do (map #(walk env %) (rest f))) (meta f))

                  (and (symbol? head)
                       (= pcps-dual-sym (macro-name env head)))
                  (with-meta (cons 'do (map #(walk env %) (drop 2 f))) (meta f))

                  ;; (. obj member) / (. obj (method args…)) / (. obj method args…):
                  ;; the member position is not call position — never resolve it
                  (= '. head)
                  (let [[_ target & more] f
                        fix (fn [m] (if (seq? m)
                                      (with-meta (cons (first m) (map #(walk env %) (rest m)))
                                        (meta m))
                                      (walk env m)))]
                    (with-meta (list* '. (walk env target) (map fix more)) (meta f)))

                  :else
                  (if-let [[expanded _] (expand-macro f env)]
                    (walk env expanded)
                    (case head
                      (fn* reify* deftype*)
                      (do (assert-no-bare-breakpoints! f (assoc ctx :env env) f)
                          f)

                      (let* loop*)
                      (let [[_ bindings & body] f
                            [env' bindings'] (walk-bindings env bindings)]
                        (with-meta
                          (list* head (vec bindings') (map #(walk env' %) body))
                          (meta f)))

                      letfn*
                      (let [[_ bindings & body] f
                            names (take-nth 2 bindings)
                            _ (guard-shadowing! names f)
                            env' (bind-locals env names)]
                        (with-meta
                          (list* 'letfn*
                                 (vec (map-indexed (fn [i x] (if (odd? i) (walk env' x) x))
                                                   bindings))
                                 (map #(walk env' %) body))
                          (meta f)))

                      catch
                      (let [[_ cls b & body] f
                            _ (guard-shadowing! [b] f)
                            env' (bind-locals env [b])]
                        (with-meta (list* 'catch cls b (map #(walk env' %) body)) (meta f)))

                      case*
                      (walk-case* env f)

                      (with-meta (apply list (map #(walk env %) f)) (meta f))))))

              (vector? f) (with-meta (mapv #(walk env %) f) (meta f))
              (map? f) (into (empty f) (map (fn [[k v]] [(walk env k) (walk env v)])) f)
              (set? f) (into (empty f) (map #(walk env %)) f)
              :else f))]
    (walk env form)))

(defn can-inline?
  [form]
  (or (not (coll? form)) ; inline non-collection literals and symbols
      ; can't inline sets or maps as they throw 'Duplicate key'
      ; but inlining vectors is fine
      (and (vector? form) (every? can-inline? form))))

(declare invert)
(declare invert-impl)

(defn resolve-sequentially [ctx coll then]
  (let [[syncs [asn & others]] (split-with #(not (has-breakpoints? % ctx)) coll)]
    (if asn
      (let [syncs (map #(if (can-inline? %) [%] [(gensym) %]) syncs)
            sync-bindings (->> syncs (filter second) (mapcat identity))
            async-binding (with-meta (gensym) (meta asn))
            cont (gensym "cont")]
        ;; The continuation is bound as a `let`-scoped ANONYMOUS fn (not `letfn`).
        ;; It is single-reference and non-self-recursive, so it doesn't need
        ;; letfn's mutual/self visibility — and crucially, cljs's analyzer runs a
        ;; SECOND analysis pass over *named* fns (fn* pass2, "optimize self calls"),
        ;; which compounds multiplicatively across the N-deep cont nesting one-per-
        ;; await → O(2^N) cljs compile. An anonymous fn skips pass2, making the
        ;; compile linear. (try/loop recur-target stay `letfn` — they genuinely
        ;; need it, and don't nest per-await.)
        `(let [~@sync-bindings
               ~cont (fn [~async-binding]
                       ~(resolve-sequentially
                         (dissoc ctx :sync-recur?) others
                         #(then `[~@(map first syncs) ~async-binding ~@%])))]
           ;; Use invert-impl (not invert) to preserve expansion/breakpoint caches
           ~(invert-impl (assoc ctx :r cont) asn)))
      (then coll))))

(defn add-env-syms [ctx syms]
  ;; Extend the transform env with locals via `bind-locals` — on clj that is
  ;; a key-presence entry (`resolve` only checks contains?), on cljs it must
  ;; reach the analyzer's :locals or shadowing stays invisible to resolution.
  (update ctx :env bind-locals syms))

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
      ;; Body has breakpoints - need to wrap continuations.
      ;; IMPORTANT: Do NOT macro-expand the binding form into push/pop-thread-bindings
      ;; and then CPS-transform the expansion. That causes push on Thread A but pop
      ;; (via CPS finally) on Thread B when await suspends — "Pop without matching push".
      ;; Instead, wrap the CPS-transformed body in the original binding form so that
      ;; push/pop are balanced on the calling thread. When await suspends, the binding
      ;; scope exits normally. Continuations fire outside this scope; wrapped-r/wrapped-e
      ;; restore outer binding values.
      `(let [~@(interleave saved-syms var-syms)
             ;; Wrapped resolve - restores outer bindings before calling original r
             ~wrapped-r (fn [val#]
                          (~binding-sym [~@(interleave var-syms saved-syms)]
                                        (~r val#)))
             ;; Wrapped reject - restores outer bindings before calling original e
             ~wrapped-e (fn [err#]
                          (~binding-sym [~@(interleave var-syms saved-syms)]
                                        (~e err#)))]
         ;; Establish bindings for sync execution, CPS-transform just the body
         ;; Use invert-impl (not invert) to preserve the expansion-cache and
         ;; breakpoint-cache across binding form boundaries. Each DOM element
         ;; macro creates binding forms (with-parent-addr, with-slot), and
         ;; resetting the cache at each level caused exponential re-expansion.
         (~macro-sym ~bindings
                     ~(invert-impl (assoc ctx :r wrapped-r :e wrapped-e)
                                   `(do ~@body))))
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

      ;; Check if this is a macro and expand it — UNLESS it is a registered breakpoint.
      ;; A breakpoint that is ALSO a macro (e.g. `cljs.core/await` on cljs >= 1.12) must be
      ;; dispatched to its handler (the breakpoint clause below), NOT macroexpanded.
      (and (not (and (symbol? head) (contains? breakpoints (var-name env head))))
           (if (and head (symbol? head) (:js-globals env))
             ;; use cljs.analyzer to find macro var info — guard symbol? first:
             ;; `resolve-macro-var-cljs` casts head to Symbol, so a keyword-fn head
             ;; like `(:k (await x))` would otherwise throw ClassCastException on cljs
             ;; (the CLJ branch below already guards `symbol?`).
             (:macro (resolve-macro-var-cljs env head))
             ;; use normal Clojure resolve — meta check for SCI compatibility
             (let [resolved (when (symbol? head) (resolve env head))]
               (and resolved (:macro (meta resolved))))))
      (recur ctx
             (apply (if (:js-globals env)
                      (resolve (:name (resolve-macro-var-cljs env head)))
                      (resolve env head))
                    form env tail))

      (or (special-symbol? head) (= head 'let) (= head 'letfn) (= head 'loop) (= head 'fn))
      (case head

        (quote var clojure.core/import*)
        `(~r ~form)

        (fn* fn deftype* reify*)
        ;; closures are opaque to the transform: a bare breakpoint inside can
        ;; never suspend — reject at compile time instead of failing at runtime
        (do (assert-no-bare-breakpoints! form ctx form)
            `(~r ~form))

        if
        (let [[con left right & unexpected-others] tail
              cont (gensym "cont")]
          (if (has-breakpoints? con ctx)
            (let [ctx' (dissoc ctx :sync-recur?)]
              ;; anonymous let-bound cont (skips cljs fn* pass2) — see resolve-sequentially
              `(let [~cont (fn [con#] (if con# ~(invert-impl ctx' left)
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
                               ;; anonymous let-bound cont (skips cljs fn* pass2) — see resolve-sequentially
                               `(let* [~@(mapcat identity syncs)]
                                      (let [~cont (fn [async-value#]
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
                    ;; anonymous let-bound cont (skips cljs fn* pass2) — see resolve-sequentially
                    `(let [~cont (fn [_#] ~(invert-impl (dissoc ctx :sync-recur?)
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

      ;; Invoke termination handler, e.g. do-await — unless this arm ERASES
      ;; the breakpoint (interpretation :erase — the site is not a suspension
      ;; point here; inline its single argument and keep transforming)
      (contains? breakpoints (var-name env head))
      (let [bp (var-name env head)]
        (if (= :erase (get (:interpretations ctx) bp))
          (do (assert (= 2 (count form))
                      (str "breakpoint call must have exactly one argument: " form))
              (invert-impl ctx (second form)))
          (let [handler (resolve (breakpoints bp))]
            (resolve-sequentially ctx (rest form) (handler ctx r e)))))

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
  "CPS inversion with caching.
   Creates caches that persist across the entire inversion:
   - expansion-cache: avoids re-expanding the same macros
   - breakpoint-cache: avoids re-traversing the same sub-trees in has-breakpoints?
   Also tags the env with ::in-cps-transform so macros can detect they are being
   expanded inside a CPS transformation and skip unnecessary branches."
  [ctx form]
  (let [expansion-cache (or (:expansion-cache ctx) (atom {}))
        breakpoint-cache (or (:breakpoint-cache ctx) (atom {}))
        ctx-with-cache (assoc ctx
                              :expansion-cache expansion-cache
                              :breakpoint-cache breakpoint-cache)
        ;; Tag the env so macros can detect CPS context and optimize their expansion.
        ;; For example, DOM element macros can skip fallback branches that are dead code
        ;; inside a CPS-transformed body.
        ctx-with-flag (update ctx-with-cache :env assoc ::in-cps-transform true)]
    (invert-impl ctx-with-flag form)))
