# 0.2.5

* Add GHCJS support (statistical analysis is not supported)
* Fix issue with perRunEnv
* Drop support for GHC 7.8

# 0.2.4

* `Enhancement`: Add `nfAppIO` and `whnfAppIO` functions, which take a function
  and its argument separately like `nf`/`whnf`, but whose function returns `IO`
  like `nfIO`/`whnfIO`. This is useful for benchmarking functions in which the
  bulk of the work is not bound by IO, but by pure computations that might
  otherwise be optimized away if the argument is known statically.

* `Bug Fix`: Pass `-m exact` option to the child processes used to run
  benchmarks in an isolated manner. This avoids running a wrong benchmark due
  to the default prefix match.

# 0.2.3

* Add a new benchmark matching option "-m exact" to match the benchmark name
  exactly.

# 0.2.2

* Write data to CSV file in quick mode too.
* Fix the CSV file header to match with the data rows for the `--csvraw` case.
* Fix issue with GC metrics in 32 bits that would silently wrap and failure in optional machinery.
* Simplify dependencies in tests using foudation checks.

# 0.2.1

* Inline math-functions & mwc-random:
  * Remove most functions, instances and types, that are unnecessary for gauge
  * Remove unsafe seeding with partial seed (unused in gauge anyway)
  * Remove vector-th-unbox dependency (transitively template-haskell, pretty, ghc-boot-th)
  * Remove time dependency
* Re-add Gauge.Benchmark to Gauge.Main to keep the transition between criterion and gauge easy
* Fix cycles reporting on linux, osx and windows
* Add some extra callstack for reporting on partial function
* Fix compilation with Semigroup => Monoid (compilation on 8.4). still unsupported
* Add some color on terminal output

# 0.2.0

* `Usability`: Simplify and organize the documentation and user APIs.
* `Functionality`:
  * Add measurement and reporting of more performance counters on
    Unices (collected via getrusage) for example page faults, user time, system
    time and rss are now available in verbose mode.
  * Re-enable CSV analysis with the same output format as criterion (`--csv`)
  * Add CSV measurement dumping with `--csvraw`
* `Control`: Provide better control over measurement process with
  `--min-samples`, `--min-duration` and `--include-first-iter` flags.
* `Speed:` Add `--quick` flag that provides results much faster (10x) without
  using statistical analysis.
* Reliability:
  * Fix a bug in GC stats collection and reporting with GHC 8.2 that caused
    incorrect reporting of some GC stats.
  * Fix a bug in statistical regression that caused incorrect reporting of mean
    and other stats.
  * Improve reliability by isolating benchmarks from one another using the
    `--measure-with` flag. The results of one benchmark are no longer affected
    by other benchmarks because each benchmark runs in a separate process.
  * Introduce an optional value type `Optional` with an efficient runtime
    representation to replace the ad-hoc fromXXX functions and the untyped
    approach.
* Modularity:
  * Introduce `--measure-only` flag that allows just measurement and no
    analysis or reporting.
  * Provide modular build, measurement code is cleanly separated from
    statistical analysis code. As a result a leaner version can now be built
    without analysis code (controlled by the `analysis` build flag).
  * Clean, refactor & rewrite source code
* Remove code-page dependency

# 0.1.3

* Simplify monad handling, remove foundation as dependency

# 0.1.2

* condensed display with `--small`

# 0.1.1

* remove optparse-applicative

# 0.1.0

* remove bunch of dependencies
* initial import of criterion-1.2.2.0
