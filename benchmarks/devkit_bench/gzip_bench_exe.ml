(* Executable wrapper for Gzip benchmark *)
open Gc_bench_lib
let () = Gzip_bench.run_all_benchmarks ()