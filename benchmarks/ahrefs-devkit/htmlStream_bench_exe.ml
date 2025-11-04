(* Executable wrapper for HtmlStream benchmark *)
open Gc_bench_lib
let () = HtmlStream_bench.run_all_benchmarks ()