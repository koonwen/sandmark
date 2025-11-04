(* Executable wrapper for Network benchmark *)
open Gc_bench_lib
let () = Network_bench.run_all_benchmarks ()