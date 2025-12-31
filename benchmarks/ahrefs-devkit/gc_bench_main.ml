(** Main GC Benchmark Runner

    This is the main entry point for running all GC benchmarks.
    It provides a unified interface to run individual or all benchmarks
    with consistent configuration.
*)

open Gc_bench_lib

(* Benchmark suite configuration *)
type suite_config = {
  warmup_iterations : int;
  benchmark_iterations : int;
  benchmarks : string list option; (* None means run all *)
}

let default_suite_config = {
  warmup_iterations = 3;
  benchmark_iterations = 10;
  benchmarks = None;
}

(* Available benchmarks *)
type benchmark_info = {
  name : string;
  runner : unit -> unit;
}

let create_htmlstream_config config = {
  HtmlStream_bench.warmup_iterations = config.warmup_iterations;
  HtmlStream_bench.benchmark_iterations = config.benchmark_iterations;
}

let create_stre_config config = {
  Stre_bench.warmup_iterations = config.warmup_iterations;
  Stre_bench.benchmark_iterations = config.benchmark_iterations;
}

let create_network_config config = {
  Network_bench.warmup_iterations = config.warmup_iterations;
  Network_bench.benchmark_iterations = config.benchmark_iterations;
}

let create_gzip_config config = {
  Gzip_bench.warmup_iterations = config.warmup_iterations;
  Gzip_bench.benchmark_iterations = config.benchmark_iterations;
}

let available_benchmarks config = [
  {
    name = "htmlstream";
    runner = (fun () -> HtmlStream_bench.run_all_benchmarks ~config:(create_htmlstream_config config) ());
  };
  {
    name = "stre";
    runner = (fun () -> Stre_bench.run_all_benchmarks ~config:(create_stre_config config) ());
  };
  {
    name = "network";
    runner = (fun () -> Network_bench.run_all_benchmarks ~config:(create_network_config config) ());
  };
  {
    name = "gzip";
    runner = (fun () -> Gzip_bench.run_all_benchmarks ~config:(create_gzip_config config) ());
  };
]

(* Run selected benchmarks *)
let run_benchmarks config =
  let benchmarks_to_run =
    match config.benchmarks with
    | None -> available_benchmarks config
    | Some names ->
        let all_benches = available_benchmarks config in
        List.filter (fun b ->
          List.mem (String.lowercase_ascii b.name)
            (List.map String.lowercase_ascii names)
        ) all_benches
  in

  List.iter (fun benchmark ->
    benchmark.runner ()
  ) benchmarks_to_run

(* Entry point - run all benchmarks with default config *)
let () =
  run_benchmarks default_suite_config
