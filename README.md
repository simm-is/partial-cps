# partial-cps

A lightweight Clojure/ClojureScript library for [continuation-passing style](https://en.wikipedia.org/wiki/Continuation-passing_style) (CPS) transformations derived from [await-cps](https://github.com/mszajna/await-cps). The CPS transform rewrites code similarly to manual callback rewriting by providing a callback (continuation) to a user specified handler only where necessary (at a user-specified breakpoint). Compared to full CPS which transforms all code expressions, this approach leaves the remaining code synchronous and therefore retains readability compared to the transformations in [pulley](https://github.com/positronic-solutions/pulley.cps), [anglican](https://github.com/probprog/anglican/blob/master/src/anglican/trap.cljc), [core.async](https://github.com/clojure/core.async) or [cloroutine](https://github.com/leonoel/cloroutine). Nonetheless the benefits of CPS transforms are not reduced by the partial approach. The breakpoints and continuations still lend themselves to an effect handling perspective, where different types of effects (or alternatively monads) can be implemented as handlers for specific breakpoints.

The transformed code is also usually faster than more invasive transforms (ANF, SSA) for synchronous sections, since it directly passes the user supplied Clojure code to the underlying compiler. Additionally, when dispatching into callbacks the transform does not prescribe dispatching into any processing or scheduling framework at breakpoints (threadpools, JS event loop, ...). Instead, the provided handler can use a direct safe trampolining dispatch. This avoids hardcoded scheduling overhead and only hits a threadpool dispatcher or JS event loop if the effect handler (e.g. awaited effect callback) explicitly schedules it. We found this to be necessary to implement fast async versions of persistent data structures and sequence abstractions in [persistent-sorted-set](https://github.com/replikativ/persistent-sorted-set/tree/lean-cps) for [datahike](https://github.com/replikativ/datahike/), where most async invocations are expected to hit warm caches and only sporadically asynchronous execution (IO) is needed, yet the fine grained nature of the async code renders dispatching into the JS event loop prohibitively expensive in such low-level code.

## Features

- **Async/Await**: Write asynchronous code that looks synchronous
- **Transparent Iteration**: Standard `doseq`, `dotimes`, and `while` work with `await` automatically
- **Async Sequences**: Lazy async sequences with transducer support and `for` comprehension
- **Custom Coroutines**: Build your own control flow primitives with breakpoints
- **Cross-platform**: Works seamlessly with both Clojure and ClojureScript
- **Lightweight**: Minimal dependencies and overhead
- **Fast**: No scheduling overhead, synchronous code stays synchronous
- **Safe**: Automatic trampolining prevents stack overflow

## Installation

### deps.edn
```clojure
{is.simm/partial-cps {:git/url "https://github.com/simm-is/partial-cps" :git/sha "LATEST"}} ; Check github for latest commit
```

## Usage

### Basic Async/Await

```clojure
(require '[is.simm.partial-cps.async :refer [async await]])

;; Define an async function
(def fetch-user
  (async
    (let [user-data (await (http-get "/api/user"))
          profile (await (http-get (str "/api/profile/" (:id user-data))))]
      (merge user-data profile))))

;; Run the async function with callbacks
(fetch-user
 (fn [result] (println "Success:" result))
 (fn [error] (println "Error:" error)))
```

### Error Handling

Errors propagate naturally through the async chain:

```clojure
(def safe-operation
  (async
    (try
      (let [result (await risky-operation)]
        (process result))
      (catch Exception e
        (println "Caught error:" (ex-message e))
        :fallback-value))))

;; Errors can also be handled in the error callback
(safe-operation
  (fn [result] (println "Got result:" result))
  (fn [error] 
    (log-error error)
    (handle-recovery error)))
```

### Iteration with Async Operations

Standard Clojure iteration macros work transparently with async operations:

```clojure
;; doseq - process items sequentially
(async
  (doseq [item (await fetch-items)]
    (println "Processing:" item)
    (await (process-item item))))

;; doseq with modifiers (:let, :when, :while)
(async
  (doseq [x [1 2 3 4 5]
          :when (even? x)
          :let [doubled (* x 2)]]
    (await (save-value doubled))))

;; dotimes - counted iteration
(async
  (dotimes [i 5]
    (println "Iteration" i)
    (await (delay-ms 1000))))

;; while - conditional iteration
(async
  (while (< @counter 10)
    (let [result (await (fetch-next))]
      (swap! counter inc)
      (process result))))
```

### Async Sequence Comprehension

For lazy async sequences, use `seq/for`:

```clojure
(require '[is.simm.partial-cps.sequence :as seq])

;; Create lazy async sequence
(async
  (let [results (seq/for [item (await fetch-items)]
                  (await (process-item item)))]
    ;; Consume the lazy sequence
    (await (seq/into [] results))))

;; Nested iteration with modifiers
(async
  (let [pairs (seq/for [x [1 2 3]
                        y [10 20 30]
                        :when (even? (+ x y))
                        :let [sum (+ x y)]]
                (await (process sum)))]
    (await (seq/into [] pairs))))
```

### Async Sequences

Work with lazy, asynchronous data streams using transducers:

```clojure
(require '[is.simm.partial-cps.sequence :as seq])

;; Define an async sequence using PAsyncSeq protocol
(defrecord AsyncRange [start end]
  seq/PAsyncSeq
  (anext [_]
    (async
      (when (< start end)
        [start (->AsyncRange (inc start) end)]))))

;; Use transducers with async sequences
(async
  (let [async-seq (->AsyncRange 0 100)]
    ;; Eagerly process with transduce
    (let [sum (await (seq/transduce (map inc) + 0 async-seq))]
      (println "Sum:" sum))

    ;; Lazily transform with sequence
    (let [transformed (seq/sequence
                        (comp (filter even?)
                              (map #(* % 2))
                              (take 5))
                        async-seq)
          result (await (seq/into [] transformed))]
      (println "Transformed:" result))))
```

### Integration with Promises (ClojureScript)

```clojure
;; Convert JS Promise to CPS
(defn promise->cps [promise]
  (fn [resolve reject]
    (.then promise resolve reject)))

(async
  (let [response (await (promise->cps (js/fetch "/api/data")))
        data (await (promise->cps (.json response)))]
    (process-data data)))
```

### Node.js Callback Integration

```clojure
;; Convert Node.js-style callbacks
(defn node-callback->cps [f & args]
  (fn [resolve reject]
    (apply f (concat args 
                    [(fn [err result]
                       (if err
                         (reject err)
                         (resolve result)))]))))

(async
  (let [file-content (await (node-callback->cps fs/readFile "file.txt" "utf8"))]
    (println "File contents:" file-content)))
```

## Advanced Usage

### Creating Custom Breakpoints

Build your own control flow primitives:

```clojure
(def ^:no-doc breakpoints
  {`my-breakpoint `my-handler})

#?(:clj
   (defmacro my-cps
     "Defines a function that takes a successful and exceptional continuation,
   and runs the body, suspending execution whenever any of the breakpoints is
   encountered, and eventually calling one of the continuations with the
   result.

   A call of the form (breakpoint args..) is forwarded to the corresponding handler
   (handler succ exc args..), which is expected to eventually call either succ
   with the value or exc with exception to substitute the original call result
   and resuming the execution."
     [& body]
     (let [r (gensym) e (gensym)
           params {:r r :e e :env &env :breakpoints breakpoints}
           expanded (try
                      (macroexpand-all (cons 'do body))
                      (catch Exception e
                        (throw e)))]
       `(fn [~r ~e]
          (try
            (loop [result# ~(invert params expanded)]
              (if (instance? is.simm.partial_cps.runtime.Thunk result#)
                ;; If continuation returns a thunk, trampoline it
                (recur ((.-f ^is.simm.partial_cps.runtime.Thunk result#)))
                result#))
            (catch ~(if (:js-globals &env) :default `Throwable) t# (~e t#)))))))

;; Define a custom handler
(defn my-handler [env r e]
  (fn [args]
    `(schedule-microtask 
       (fn [] (~r ~(first args))))))

;; Create a coroutine with custom breakpoint
(def my-coroutine
  (my-cps
    (let [i 3]
      (println "Before yield" i)
      (my-yield i)
      (println "After yield" i))))

(my-coroutine
  (fn [result] (println "Done"))
  (fn [error] (println "Error:" error)))
```

### Performance Characteristics

The library is designed for high performance:

- **Minimal transformation**: Only rewrites code at breakpoint points
- **No scheduling overhead**: Direct callback execution unless you add scheduling
- **Safe trampolining**: Prevents stack overflow while maintaining speed
- **Synchronous fast path**: Non-async code runs at full speed

## Testing

The library integrates well with standard test frameworks:

```clojure
(require '[clojure.test :refer [deftest testing is] :as test]
         '[is.simm.partial-cps.async :refer [async await]])

;; Clojure test
(deftest my-async-test
  (testing "async operations"
    (let [result (atom nil)]
      ((async
         (reset! result (await some-async-op)))
       (fn [_] (is (= @result expected)))
       (fn [err] (is false "Should not fail"))))))

;; ClojureScript test
(deftest my-async-test
  (testing "async operations"
    (test/async done  ; test.async pattern
      ((async
         (let [result (await some-async-op)]
           (is (= result expected))))
       (fn [_] (done))
       (fn [err] 
         (is false "Should not fail")
         (done))))))
```

## API Reference

### Core (`is.simm.partial-cps`)
- `(cps breakpoints & body)` - Create a coroutine with custom breakpoints
- `(coroutine success-fn error-fn)` - Execute a coroutine

### Async (`is.simm.partial-cps.async`)
- `(async & body)` - Create an async function
- `(await async-op)` - Await an async operation (must be inside async)
- `(async-fn success-fn error-fn)` - Execute an async function

### Sequences (`is.simm.partial-cps.sequence`)
- `PAsyncSeq` - Protocol for async sequences
  - `(anext this)` - Return async expression yielding `[value rest-seq]` or nil if exhausted
- `(first async-seq)` - Get first element (async)
- `(rest async-seq)` - Get rest of sequence (async)
- `(for bindings & body)` - Async sequence comprehension, supports `:let`, `:when`, `:while` modifiers
- `(transduce xform f init async-seq)` - Eagerly transduce
- `(into to xform? async-seq)` - Pour into collection
- `(sequence xform async-seq)` - Lazy transformation

## Development

### Running Tests

```bash
# Clojure tests
clojure -X:test

# ClojureScript tests
npx shadow-cljs compile test
node target/test.js
```

### Building

```bash
# Build JAR
clojure -T:build ci

# Install locally
clojure -T:build install

# Deploy to Clojars
clojure -T:build deploy
```

### REPL Development

```bash
# Start REPL with CIDER support
clojure -M:repl

# Start REPL with MCP server
clojure -M:repl-mcp
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

Areas of interest:
- Additional async sequence operations and combinators
- Performance optimizations
- Documentation and examples
- Integration with more async libraries

## License

MIT License

Copyright © 2025 Christian Weilbach, 2019 Maciej Szajna