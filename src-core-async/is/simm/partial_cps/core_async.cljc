(ns is.simm.partial-cps.core-async
  "OPTIONAL core.async ↔ partial-cps adapter — the two-way bridge between
   `clojure.core.async` channels and partial-cps CPS values, so the two substrates
   compose without either side hard-depending on the other.

   core.async is NOT a base partial-cps dependency: this namespace lives on its own
   source path (`src-core-async`, shipped in the jar) and is only loaded by code that
   already pulls core.async. Require it from such code; base partial-cps users never
   touch it.

   Two directions — both NON-blocking (this adapter never `<!!`s; deciding to block is
   the caller's concern, not the bridge's):
     - `->cps` / `chan->cps` — a core.async channel (or value, or CPS) → a partial-cps
       CPS `(fn [resolve raise])`, so it is `await`-able inside a partial-cps `async`.
     - `->chan` — a CPS (or value, or channel) → a core.async channel, so it is `<!`-able
       inside a `go` block. `unwrap-result` restores the datum (re-throws an error,
       nil for the sentinel).
     - `sync-or-cps` — bridge a `:sync?`-aware result (value on sync / channel on async)
       into an `async+sync` body, NON-blocking either way.

   Error convention: an error is a Throwable / JS error delivered ON the channel (so
   `unwrap-result` re-throws it) or passed to a CPS `raise`. A genuine nil datum is
   carried as `sentinel-nil` (core.async forbids nil puts)."
  (:require #?(:clj  [clojure.core.async :refer [chan put! close! take!]]
               :cljs [cljs.core.async :refer [chan put! close! take!]])
            #?(:clj  [clojure.core.async.impl.protocols :as impl]
               :cljs [cljs.core.async.impl.protocols :as impl])))

(defn chan?
  "True if x is a core.async channel. (core.async 1.6.681 ships no public `chan?`,
   so we test the ReadPort impl protocol; a one-liner swap when core.async is bumped.)"
  [x]
  (and (some? x) (satisfies? impl/ReadPort x)))

(defn error? [v]
  (instance? #?(:clj Throwable :cljs js/Error) v))

(defn rewrap
  "Re-wrap an error VALUE in a FRESH `ex-info` — message + ex-data preserved, the
   original as `cause`. This is superv.async's `throw-if-exception` convention: when
   an error that was caught + carried as a channel/CPS value is re-thrown in another
   context, the wrapper adds the boundary's stack frame while the cause chain keeps
   the original, so a core.async error propagates into partial-cps (and back) with a
   full trace. Unlike superv there is NO supervisor coupling (no `-free-exception`),
   so this works for plain core.async too — the shared convention is just
   `error-as-a-value-on-the-channel`."
  [e]
  (ex-info (or #?(:clj (.getMessage ^Throwable e) :cljs (.-message e)) (str e))
           (or (ex-data e) {})
           e))

(def sentinel-nil
  "Stand-in for a genuine nil datum (core.async forbids nil puts)."
  ::nil)

;; ── direction 1: core.async channel → partial-cps CPS (await-able) ──────────

(defn chan->cps
  "Wrap a core.async channel as a partial-cps CPS fn `(fn [resolve raise])`:
   `take!` the channel, `raise` on a Throwable/JS-error value, else `resolve`."
  [ch]
  (fn [resolve raise]
    (take! ch (fn [v] (if (error? v) (raise (rewrap v)) (resolve v))))))

(defn ->cps
  "Normalize value | core.async channel | CPS fn into a partial-cps CPS fn
   (`await`-able). A plain value resolves immediately; a CPS passes through."
  [x]
  (cond
    (chan? x) (chan->cps x)
    (fn? x)   x
    :else     (fn [resolve _raise] (resolve x))))

;; ── direction 2: partial-cps CPS (or value/chan) → core.async channel ───────

(defn- put-datum! [ch v]
  (put! ch (if (nil? v) sentinel-nil v))
  (close! ch))

(defn ->chan
  "Normalize value | core.async channel | partial-cps CPS `(fn [resolve raise])`
   into a core.async channel delivering the resolved datum (genuine nil as
   `sentinel-nil`; an error as a Throwable/JS error on the channel). Read with
   `(unwrap-result (<! (->chan x)))`.

   Invoking a partial-cps CPS directly from a core.async go-block is safe with no
   trampoline rebind — the `async` fn self-establishes its trampoline."
  [x]
  (cond
    (chan? x) x
    (fn? x)   (let [c (chan 1)]
                (try
                  (x (fn [v] (put-datum! c v))
                     (fn [e] (put! c e) (close! c)))
                  (catch #?(:clj Throwable :cljs :default) e
                    (put! c e) (close! c)))
                c)
    :else     (let [c (chan 1)] (put-datum! c x) c)))

(defn unwrap-result
  "Restore a datum read off a `->chan` channel: re-throw on error, nil for the
   sentinel, else the value."
  [v]
  (cond
    (error? v)         (throw (rewrap v))
    (= v sentinel-nil) nil
    :else              v))

(defn sync-or-cps
  "Bridge a `:sync?`-aware result (a VALUE on `{:sync? true}`, a core.async CHANNEL
   on `{:sync? false}`) into an `async+sync` body — NON-blocking either way: pass the
   already-materialized value through on sync, else wrap the channel as an await-able
   CPS. (For a source that has NO sync mode — always a channel — there is no
   non-blocking sync option; that block belongs at the caller that knows the source,
   not in this generic adapter.)"
  [result opts]
  (if (:sync? opts) result (chan->cps result)))
