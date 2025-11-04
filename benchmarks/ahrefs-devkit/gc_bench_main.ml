(** Main GC Benchmark Runner

    This is the main entry point for running all GC benchmarks.
    It provides a unified interface to run individual or all benchmarks
    with consistent configuration and reporting.
*)

open Gc_bench_lib


(* Benchmark suite configuration *)
type suite_config = {
  warmup_iterations : int;
  benchmark_iterations : int;
  verbose : bool;
  gc_stats : bool;
  benchmarks : string list option; (* None means run all *)
}

let default_suite_config = {
  warmup_iterations = 3;
  benchmark_iterations = 10;
  verbose = false;
  gc_stats = true;
  benchmarks = None;
}

(* Available benchmarks *)
type benchmark_info = {
  name : string;
  description : string;
  runner : unit -> unit;
}

let create_htmlstream_config config = {
  HtmlStream_bench.warmup_iterations = config.warmup_iterations;
  HtmlStream_bench.benchmark_iterations = config.benchmark_iterations;
  HtmlStream_bench.verbose = config.verbose;
  HtmlStream_bench.gc_stats = config.gc_stats;
}

let create_stre_config config = {
  Stre_bench.warmup_iterations = config.warmup_iterations;
  Stre_bench.benchmark_iterations = config.benchmark_iterations;
  Stre_bench.verbose = config.verbose;
  Stre_bench.gc_stats = config.gc_stats;
}

let create_network_config config = {
  Network_bench.warmup_iterations = config.warmup_iterations;
  Network_bench.benchmark_iterations = config.benchmark_iterations;
  Network_bench.verbose = config.verbose;
  Network_bench.gc_stats = config.gc_stats;
}

let create_gzip_config config = {
  Gzip_bench.warmup_iterations = config.warmup_iterations;
  Gzip_bench.benchmark_iterations = config.benchmark_iterations;
  Gzip_bench.verbose = config.verbose;
  Gzip_bench.gc_stats = config.gc_stats;
}

let available_benchmarks config = [
  {
    name = "htmlstream";
    description = "HTML parsing and streaming with attribute lists, text nodes, and large blocks";
    runner = (fun () -> HtmlStream_bench.run_all_benchmarks ~config:(create_htmlstream_config config) ());
  };
  {
    name = "stre";
    description = "String manipulation including splitting, slicing, and pattern operations";
    runner = (fun () -> Stre_bench.run_all_benchmarks ~config:(create_stre_config config) ());
  };
  {
    name = "network";
    description = "Network parsing with IPv4 addresses, CIDR calculations, and Int32 operations";
    runner = (fun () -> Network_bench.run_all_benchmarks ~config:(create_network_config config) ());
  };
  {
    name = "gzip";
    description = "Compression/decompression with buffer allocations and streaming patterns";
    runner = (fun () -> Gzip_bench.run_all_benchmarks ~config:(create_gzip_config config) ());
  };
]

(* Print system information *)
let print_system_info () =
  Printf.printf "System Information:\n";
  Printf.printf "  OCaml version: %s\n" Sys.ocaml_version;
  Printf.printf "  OS type: %s\n" Sys.os_type;
  Printf.printf "  Word size: %d bits\n" Sys.word_size;
  Printf.printf "  Int size: %d bits\n" Sys.int_size;

  let gc = Gc.get () in
  Printf.printf "\nInitial GC Configuration:\n";
  Printf.printf "  Minor heap size: %d words\n" gc.Gc.minor_heap_size;
  Printf.printf "  Major heap increment: %d words\n" gc.Gc.major_heap_increment;
  Printf.printf "  Space overhead: %d%%\n" gc.Gc.space_overhead;
  Printf.printf "  Max overhead: %d%%\n" gc.Gc.max_overhead;
  Printf.printf "  Stack limit: %d words\n" gc.Gc.stack_limit;
  Printf.printf "\n"

(* Run selected benchmarks *)
let run_benchmarks config =
  print_system_info ();

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

  if benchmarks_to_run = [] then begin
    Printf.printf "No matching benchmarks found.\n";
    Printf.printf "Available benchmarks:\n";
    List.iter (fun b ->
      Printf.printf "  - %s: %s\n" b.name b.description
    ) (available_benchmarks config);
    exit 1
  end;

  Printf.printf "Running %d benchmark suite(s)...\n\n" (List.length benchmarks_to_run);

  let start_time = Sys.time () in

  List.iter (fun benchmark ->
    Printf.printf "\n";
    Printf.printf "================================================================================\n";
    Printf.printf "  Running: %s\n" (String.uppercase_ascii benchmark.name);
    Printf.printf "  %s\n" benchmark.description;
    Printf.printf "================================================================================\n";
    benchmark.runner ();
    Printf.printf "\n"
  ) benchmarks_to_run;

  let total_time = Sys.time () -. start_time in

  Printf.printf "\n";
  Printf.printf "================================================================================\n";
  Printf.printf "  All Benchmarks Complete\n";
  Printf.printf "================================================================================\n";
  Printf.printf "Total execution time: %.3f seconds\n" total_time;
  Printf.printf "\n"

(* Command-line interface *)
let () =
  let config = ref default_suite_config in
  let benchmark_names = ref [] in

  let usage = Printf.sprintf
    "Usage: %s [options] [benchmark1 benchmark2 ...]\n\nAvailable benchmarks: htmlstream, stre, network, gzip\n\nIf no benchmarks are specified, all will be run."
    Sys.argv.(0) in

  let spec = [
    ("-warmup", Arg.Int (fun n -> config := { !config with warmup_iterations = n }),
     "Number of warmup iterations (default: 3)");

    ("-iterations", Arg.Int (fun n -> config := { !config with benchmark_iterations = n }),
     "Number of benchmark iterations (default: 10)");

    ("-verbose", Arg.Unit (fun () -> config := { !config with verbose = true }),
     "Enable verbose output");

    ("-no-gc-stats", Arg.Unit (fun () -> config := { !config with gc_stats = false }),
     "Disable GC statistics output");

    ("-list", Arg.Unit (fun () ->
      Printf.printf "Available benchmarks:\n";
      List.iter (fun b ->
        Printf.printf "  %-12s : %s\n" b.name b.description
      ) (available_benchmarks !config);
      exit 0
    ), "List available benchmarks and exit");
  ] in

  Arg.parse spec (fun name -> benchmark_names := name :: !benchmark_names) usage;

  (* Set benchmarks to run *)
  if !benchmark_names <> [] then
    config := { !config with benchmarks = Some (List.rev !benchmark_names) };

  run_benchmarks !config