# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

partial-cps is a Clojure/ClojureScript library for continuation-passing style (CPS) transformations. It provides lightweight coroutines and async/await functionality without the overhead of scheduling frameworks like core.async.

## Development Commands

### Building
```bash
# Build JAR
clojure -T:build ci

# Install locally
clojure -T:build install

# Deploy to Clojars
clojure -T:build deploy
```

### Testing
```bash
# Run Clojure tests
clojure -M:test

# Run ClojureScript tests (compile and run)
npx shadow-cljs compile test && node target/test.js

# Run ClojureScript tests with watch mode
npx shadow-cljs watch test
# Then in another terminal: node target/test.js
```

### REPL
```bash
# Start Clojure REPL with CIDER support
clojure -M:repl

# Start ClojureScript REPL (node-based)
npx shadow-cljs cljs-repl test
```

## Architecture

### Core Namespaces

1. **is.simm.partial-cps** - Public API entry point that re-exports key functions
2. **is.simm.partial-cps.ioc** - Inversion of Control implementation for CPS transformation
3. **is.simm.partial-cps.runtime** - Runtime execution and trampolining for coroutines
4. **is.simm.partial-cps.async** - Async/await implementation built on top of the CPS transform

### Key Concepts

**CPS Transformation**: The library transforms regular Clojure code into continuation-passing style at compile time using macros. This is done through:
- The `cps` macro that accepts breakpoints and transforms code blocks
- breakpoints that handle specific function calls (like `await`) during transformation
- The IOC namespace walks the AST and inverts control flow where needed

**Trampolining**: The runtime uses safe trampolining to avoid stack overflow while maintaining synchronous code sections for performance. Async operations only hit the JS event loop when the effect handler schedules it.

**Cross-platform Support**: Code is written in `.cljc` files with reader conditionals (`#?`) to handle platform differences between Clojure and ClojureScript.

## Code Conventions

- Use `.cljc` extension for cross-platform code
- Use reader conditionals `#?(:clj ... :cljs ...)` for platform-specific code
- Macros must be defined in Clojure (`:clj`) blocks even for ClojureScript usage
- Follow standard Clojure naming conventions (kebab-case for functions, PascalCase for records/types)
- Use `letfn` for local recursive functions
- Prefix internal/implementation functions with `-` or mark with `^:no-doc`

## Testing Approach

- Cross-platform tests use `.cljc` files with `clojure.test`/`cljs.test`
- CLJ tests are run via cognitect test-runner: `clojure -M:test`
- CLJS tests use shadow-cljs with node target: `npx shadow-cljs compile test && node target/test.js`
- Test files follow the pattern `<namespace>_test.clj[cs]`
- `test_helpers.cljc` provides cross-platform `test-async` macros for unified async testing