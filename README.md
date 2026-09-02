# rvfun-benchmarks

Benchmark sources for measuring RV32 cores across three compiler versions:
MicroHs (fun backend), GHC, and GCC.  One directory per suite.

## Porting rules (uniform across the collection)
* **Never rewrite a benchmark.**  Upstream bodies are used verbatim; ports
  add only glue (entry point, fixed sim-scale arguments, prelude
  adaptation).  Every departure is a `PORT:` comment at the site.
* **Double -> Float everywhere.**  The target is rv32f (binary32 FPU, no
  D extension); ALL versions of every float benchmark compute binary32, so no
  version ever pays soft-float double against another's hardware float.
* **Compute-only windows.**  Every version reports the PMC block over exactly
  the benchmark computation (link-level `--wrap=main` for verbatim C,
  `rts_evalLazyIO` bracketing for verbatim GHC, the harness epilogue for
  the fun version); GC cycles are measured and subtracted where a collector
  runs.
* Sim-scale inputs are chosen per benchmark from its cost function and
  are identical across versions (baked argv / embedded stdin).

## Suites
| dir | contents | versions |
|---|---|---|
| flite/        | the f-lite reduction suite        | mhs(fun) + GHC, one source |
| nofib/        | nofib ports                       | mhs(fun) + GHC, one source; mhs-shadow/ = Int-backed Integer for the nano-dialect ports |
| gadt/         | GADT-structured pure benchmarks   | mhs(fun) + GHC (ghc-shim/ maps NanoPrelude to Prelude) |
| gadt-c/       | C counterparts of the gadt suite  | C -O2/-O3, bare driver |
| shootout/     | Computer Language Shootout ports  | three versions, see below |
| embench/ dhrystone/ coremark/ | conventional RV32 scores | C -O2 |

## shootout/ lanes
* `src-orig/`     upstream sources exactly as shipped by the Great
                  Computer Language Shootout (the chosen benchmarks).
* `src-verbatim/` the GHC version: upstream `.ghc` entries with only
                  compile-compat edits (pre-hierarchical imports,
                  `isTrue#` for the 7.8 primop change, bytestring module
                  moves) and the Double->Float rule; each edit carries a
                  `PORT:` note.
* `src-gcc/`      the GCC version: `f_*.c` are the Double->Float ports of the
                  upstream `.gcc` entries; `cv_*.c` are byte-identical
                  upstream copies (renamed only); older bench-wrapped
                  files from the previous campaign remain for reference.
* `src-mhs/`      the fun-version companions: the same computation in the
                  NanoPrelude dialect where the fun backend's lack of IO
                  forces a companion module (inputs embedded as data,
                  results folded with the suite hash, upstream function
                  bodies preserved).  Host-proven against the verbatim
                  versions' outputs before every RTL run.
* `data/`         the shared inputs (fasta-generated), identical bytes
                  embedded into every version.
* `include/`      upstream shootout headers (simple_hash*).

The bare-metal harness (bmkernel: syscall emulation, argv/stdin
injection, PMC windows, pthread shim) lives in its own repository; these
are the benchmark sources only.
