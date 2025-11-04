(* Executable wrapper for Stre benchmark *)
open Gc_bench_lib
let () = Stre_bench.run_all_benchmarks ()