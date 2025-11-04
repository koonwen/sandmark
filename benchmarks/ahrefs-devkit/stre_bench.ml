(** Stre GC Benchmark Suite

    This suite stresses the OCaml garbage collector through intensive string
    manipulation operations using the Stre module. Each benchmark targets
    different GC behaviors through string allocation patterns:
    - Substring allocation pressure
    - String splitting and concatenation
    - Pattern-based operations with regular expressions
    - Temporary string creation and disposal
*)

open Devkit

(* Benchmark configuration *)
type config = {
  warmup_iterations : int;
  benchmark_iterations : int;
  verbose : bool;
  gc_stats : bool;
}

let default_config = {
  warmup_iterations = 3;
  benchmark_iterations = 10;
  verbose = false;
  gc_stats = true;
}

(* GC statistics collection *)
type gc_snapshot = {
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  free_words : int;
  largest_free : int;
  fragments : int;
  compactions : int;
  top_heap_words : int;
}

let snapshot_gc () =
  let stat = Gc.stat () in
  {
    minor_words = stat.Gc.minor_words;
    promoted_words = stat.Gc.promoted_words;
    major_words = stat.Gc.major_words;
    minor_collections = stat.Gc.minor_collections;
    major_collections = stat.Gc.major_collections;
    heap_words = stat.Gc.heap_words;
    heap_chunks = stat.Gc.heap_chunks;
    live_words = stat.Gc.live_words;
    free_words = stat.Gc.free_words;
    largest_free = stat.Gc.largest_free;
    fragments = stat.Gc.fragments;
    compactions = stat.Gc.compactions;
    top_heap_words = stat.Gc.top_heap_words;
  }

let diff_gc before after = {
  minor_words = after.minor_words -. before.minor_words;
  promoted_words = after.promoted_words -. before.promoted_words;
  major_words = after.major_words -. before.major_words;
  minor_collections = after.minor_collections - before.minor_collections;
  major_collections = after.major_collections - before.major_collections;
  heap_words = after.heap_words;
  heap_chunks = after.heap_chunks;
  live_words = after.live_words;
  free_words = after.free_words;
  largest_free = after.largest_free;
  fragments = after.fragments;
  compactions = after.compactions - before.compactions;
  top_heap_words = max after.top_heap_words before.top_heap_words;
}

let print_gc_stats name stats =
  Printf.printf "\n=== GC Stats for %s ===\n" name;
  Printf.printf "Minor words allocated: %.0f\n" stats.minor_words;
  Printf.printf "Promoted words: %.0f\n" stats.promoted_words;
  Printf.printf "Major words allocated: %.0f\n" stats.major_words;
  Printf.printf "Minor collections: %d\n" stats.minor_collections;
  Printf.printf "Major collections: %d\n" stats.major_collections;
  Printf.printf "Heap words: %d\n" stats.heap_words;
  Printf.printf "Live words: %d\n" stats.live_words;
  Printf.printf "Fragments: %d\n" stats.fragments;
  Printf.printf "Compactions: %d\n" stats.compactions

(* Benchmark runner *)
let run_benchmark ~config ~name ~test_fn () =
  Printf.printf "Running benchmark: %s\n" name;

  (* Warmup *)
  for _ = 1 to config.warmup_iterations do
    test_fn ()
  done;

  (* Force major GC to start from clean state *)
  Gc.full_major ();

  let before = snapshot_gc () in
  let start = Sys.time () in

  (* Actual benchmark *)
  for _ = 1 to config.benchmark_iterations do
    test_fn ()
  done;

  let elapsed = Sys.time () -. start in
  let after = snapshot_gc () in
  let stats = diff_gc before after in

  Printf.printf "  Time: %.3f seconds\n" elapsed;
  Printf.printf "  Throughput: %.0f iterations/sec\n"
    (float_of_int config.benchmark_iterations /. elapsed);

  if config.gc_stats then print_gc_stats name stats;

  (elapsed, stats)

(* ============================================================================
   Benchmark 1: String Split Storm (Minor GC stress)
   Creates many temporary strings through splitting operations
   ============================================================================ *)
let bench_split_storm config =
  let test_fn () =
    let retained = ref [] in

    (* Generate test data with various delimiters *)
    let base_string = String.concat ","
      (List.init 1000 (fun i -> Printf.sprintf "item_%d_value_%d" i (i * 7))) in

    for i = 1 to 1000 do
      (* Test nsplitc with different delimiters *)
      let parts1 = Stre.nsplitc base_string ',' in
      let _parts2 = Stre.nsplitc_rev base_string ',' in

      (* Create nested splitting patterns *)
      let nested = String.concat "|" parts1 in
      let parts3 = Stre.nsplitc nested '|' in

      (* Retain some results to prevent immediate collection *)
      if i mod 50 = 0 then
        retained := parts3 @ !retained;

      (* Test fold-based splitting *)
      let count = ref 0 in
      let _ = Stre.nsplitc_fold base_string ',' (fun acc s ->
        incr count;
        if !count mod 10 = 0 then
          retained := s :: !retained;
        acc  (* Return the accumulator *)
      ) () in
      ();

      (* Clear old retained data periodically *)
      if List.length !retained > 1000 then
        retained := ExtList.List.take 500 !retained
    done
  in

  run_benchmark ~config ~name:"String Split Storm" ~test_fn ()

(* ============================================================================
   Benchmark 2: Substring Slicing Pressure
   Tests substring extraction with various sizes and patterns
   ============================================================================ *)
let bench_substring_slicing config =
  let test_fn () =
    let retained_slices = ref [] in

    (* Create large source string *)
    let source = String.init 100000 (fun i ->
      char_of_int (65 + (i mod 26))) in

    for i = 1 to 500 do
      (* Variable size slicing *)
      let slice_size = 10 + (i mod 1000) in
      let offset = i mod (String.length source - slice_size) in

      (* Test from_to operations *)
      let slice1 = Stre.from_to source offset (offset + slice_size) in
      let slice2 = Stre.unsafe_from_to source offset (offset + slice_size) in

      (* Test slice operations *)
      let slice3 = Stre.slice ~first:offset ~last:(offset + slice_size) source in

      (* Create overlapping slices to fragment memory *)
      for j = 0 to 9 do
        let overlap_start = offset + j * (slice_size / 10) in
        if overlap_start + slice_size < String.length source then
          let overlap = Stre.slice ~first:overlap_start ~last:(overlap_start + slice_size) source in
          if j mod 3 = 0 then
            retained_slices := overlap :: !retained_slices
      done;

      (* Retain some slices *)
      if i mod 20 = 0 then
        retained_slices := slice1 :: slice2 :: slice3 :: !retained_slices;

      (* Periodically trim retained list *)
      if List.length !retained_slices > 500 then
        retained_slices := ExtList.List.take 250 !retained_slices
    done
  in

  run_benchmark ~config ~name:"Substring Slicing Pressure" ~test_fn ()

(* ============================================================================
   Benchmark 3: Pattern-based String Operations
   Uses regular expressions and pattern matching for string manipulation
   ============================================================================ *)
let bench_pattern_operations config =
  let test_fn () =
    let retained_matches = ref [] in

    (* Generate text with patterns *)
    let text = String.concat "\n" (List.init 500 (fun i ->
      Printf.sprintf "Line %d: email_%d@example.com, phone: 555-%04d, code: ABC%03d"
        i i (i * 13 mod 9999) (i mod 1000)
    )) in

    for i = 1 to 100 do
      (* Split by various patterns *)
      let lines = Stre.nsplitc text '\n' in

      (* Process each line with pattern operations *)
      let processed = List.map (fun line ->
        (* Split by multiple delimiters *)
        let parts1 = Stre.nsplitc line ':' in
        let parts2 = List.concat_map (fun p -> Stre.nsplitc p ',') parts1 in

        (* Extract substrings based on position *)
        let extracted = List.filter_map (fun p ->
          if String.length p > 5 then
            Some (Stre.slice ~first:0 ~last:5 p)
          else None
        ) parts2 in

        String.concat "|" extracted
      ) lines in

      (* Retain some results *)
      if i mod 10 = 0 then
        retained_matches := processed @ !retained_matches;

      (* Create temporary strings through transformations *)
      let _ = List.map (fun s ->
        let upper = String.uppercase_ascii s in
        let lower = String.lowercase_ascii s in
        let reversed = String.init (String.length s) (fun j ->
          String.get s (String.length s - 1 - j))
        in
        if i mod 20 = 0 then
          retained_matches := upper :: lower :: reversed :: !retained_matches
      ) (ExtList.List.take 10 processed) in

      (* Trim retention list *)
      if List.length !retained_matches > 1000 then
        retained_matches := ExtList.List.take 500 !retained_matches
    done
  in

  run_benchmark ~config ~name:"Pattern-based Operations" ~test_fn ()

(* ============================================================================
   Benchmark 4: String Concatenation Chains
   Creates allocation pressure through repeated concatenation
   ============================================================================ *)
let bench_concatenation_chains config =
  let test_fn () =
    let retained_chains = ref [] in

    for i = 1 to 200 do
      (* Build strings through various concatenation patterns *)
      let chain1 = ref "" in
      let chain2 = ref "" in

      (* Small repeated concatenations *)
      for j = 1 to 100 do
        chain1 := !chain1 ^ (string_of_int j) ^ ",";
        if j mod 10 = 0 then
          chain2 := !chain2 ^ !chain1
      done;

      (* Split and rejoin with different delimiters *)
      let parts = Stre.nsplitc !chain1 ',' in
      let rejoined1 = String.concat "|" parts in
      let rejoined2 = String.concat ";" parts in
      let rejoined3 = String.concat "::" parts in

      (* Create nested concatenation structures *)
      let nested = List.fold_left (fun acc p ->
        acc ^ "[" ^ p ^ "]"
      ) "" (ExtList.List.take 50 parts) in

      (* Retain some chains *)
      if i mod 10 = 0 then begin
        retained_chains := !chain1 :: !chain2 :: rejoined1 ::
                          rejoined2 :: rejoined3 :: nested :: !retained_chains
      end;

      (* Periodically clear *)
      if List.length !retained_chains > 200 then
        retained_chains := ExtList.List.take 100 !retained_chains
    done
  in

  run_benchmark ~config ~name:"Concatenation Chains" ~test_fn ()

(* ============================================================================
   Benchmark 5: Enumeration-based String Processing
   Uses Enum operations which create lazy sequences with closures
   ============================================================================ *)
let bench_enum_string_ops config =
  let test_fn () =
    let retained_enums = ref [] in

    for i = 1 to 300 do
      (* Create source data *)
      let text = String.init 10000 (fun j ->
        if j mod 100 = 0 then '\n'
        else char_of_int (65 + ((i + j) mod 26))) in

      (* Create enumerations through splitting *)
      let enum1 = Stre.nsplitc_enum text '\n' in

      (* Process through enum operations *)
      let processed = Enum.map (fun line ->
        (* Further split each line *)
        let words = Stre.nsplitc line ' ' in
        String.concat "_" (List.map String.uppercase_ascii words)
      ) enum1 in

      (* Force evaluation of some elements *)
      let partial = Enum.take 50 processed |> ExtList.List.of_enum in

      (* Create another enum with different pattern *)
      let enum2 = Stre.nsplitc_enum text '\n' in
      let filtered = Enum.filter (fun s -> String.length s > 50) enum2 in
      let filtered_list = Enum.take 20 filtered |> ExtList.List.of_enum in

      (* Retain some results *)
      if i mod 15 = 0 then
        retained_enums := partial @ filtered_list @ !retained_enums;

      (* Create enumeration chains *)
      let enum3 = Stre.nsplitc_enum text '\n' in
      let chain = Enum.map (fun s ->
        Stre.slice ~first:0 ~last:(min 10 (String.length s)) s
      ) enum3 in
      let chain_list = Enum.take 30 chain |> ExtList.List.of_enum in

      if i mod 20 = 0 then
        retained_enums := chain_list @ !retained_enums;

      (* Trim retention *)
      if List.length !retained_enums > 500 then
        retained_enums := ExtList.List.take 250 !retained_enums
    done
  in

  run_benchmark ~config ~name:"Enum String Operations" ~test_fn ()

(* ============================================================================
   Benchmark 6: Mixed-size String Allocations
   Creates fragmentation through varying string sizes
   ============================================================================ *)
let bench_mixed_size_allocations config =
  let test_fn () =
    let retained_mixed = Hashtbl.create 1000 in
    let counter = ref 0 in

    for i = 1 to 500 do
      (* Create strings of varying sizes *)
      let sizes = [| 10; 100; 1000; 50; 500; 5000; 20; 200; 2000 |] in

      Array.iter (fun size ->
        incr counter;

        (* Create string of specific size *)
        let s = String.init size (fun j ->
          char_of_int (65 + ((i * j) mod 26))) in

        (* Split into variable chunks *)
        let chunk_size = max 1 (size / (10 + (i mod 10))) in
        let chunks = ref [] in
        let pos = ref 0 in
        while !pos < String.length s do
          let len = min chunk_size (String.length s - !pos) in
          chunks := (Stre.slice ~first:!pos ~last:(!pos + len) s) :: !chunks;
          pos := !pos + len
        done;

        (* Process chunks *)
        let processed = List.map (fun chunk ->
          (* Apply transformations *)
          let upper = String.uppercase_ascii chunk in
          let doubled = chunk ^ chunk in
          if !counter mod 7 = 0 then doubled else upper
        ) !chunks in

        (* Retain with non-uniform pattern *)
        if !counter mod 13 = 0 || !counter mod 17 = 0 then
          Hashtbl.replace retained_mixed !counter processed;

        (* Occasionally clear old entries *)
        if !counter mod 100 = 0 then
          Hashtbl.iter (fun k _ ->
            if k < !counter - 500 then
              Hashtbl.remove retained_mixed k
          ) retained_mixed
      ) sizes
    done
  in

  run_benchmark ~config ~name:"Mixed-size Allocations" ~test_fn ()

(* ============================================================================
   Benchmark 7: String Building with Buffers
   Tests allocation patterns when building strings incrementally
   ============================================================================ *)
let bench_string_building config =
  let test_fn () =
    let retained_built = ref [] in

    for i = 1 to 200 do
      (* Build strings using different strategies *)

      (* Strategy 1: Direct concatenation *)
      let direct = ref "" in
      for j = 1 to 100 do
        direct := !direct ^ Printf.sprintf "item_%d_%d " i j
      done;

      (* Strategy 2: List accumulation *)
      let parts = List.init 100 (fun j ->
        Printf.sprintf "item_%d_%d" i j) in
      let from_list = String.concat " " parts in

      (* Strategy 3: Substring operations *)
      let base = String.init 5000 (fun _ -> 'x') in
      let substrings = List.init 50 (fun j ->
        let start = j * 100 in
        let len = 50 + (j mod 50) in
        Stre.slice ~first:start ~last:(start + len) base
      ) in
      let from_subs = String.concat "-" substrings in

      (* Split and rebuild *)
      let split1 = Stre.nsplitc from_list ' ' in
      let rebuilt1 = String.concat "," split1 in

      let split2 = Stre.nsplitc from_subs '-' in
      let rebuilt2 = String.concat ";" split2 in

      (* Retain some results *)
      if i mod 10 = 0 then
        retained_built := !direct :: from_list :: from_subs ::
                         rebuilt1 :: rebuilt2 :: !retained_built;

      (* Trim retention *)
      if List.length !retained_built > 300 then
        retained_built := ExtList.List.take 150 !retained_built
    done
  in

  run_benchmark ~config ~name:"String Building Patterns" ~test_fn ()

(* ============================================================================
   Benchmark 8: Deep String Transformation Chains
   Creates complex reference patterns through chained transformations
   ============================================================================ *)
let bench_transformation_chains config =
  let test_fn () =
    let transformation_cache = Hashtbl.create 500 in
    let stage_results = ref [] in

    for i = 1 to 150 do
      (* Start with base string *)
      let base = String.concat "," (List.init 200 (fun j ->
        Printf.sprintf "data_%d_%d" i j
      )) in

      (* Stage 1: Split and transform *)
      let stage1 = Stre.nsplitc base ',' in
      let stage1_transformed = List.map (fun s ->
        let len = String.length s in
        if len > 5 then
          Stre.slice ~first:2 ~last:(len - 1) s
        else
          s ^ s  (* Double short strings *)
      ) stage1 in

      (* Stage 2: Recombine with different delimiter *)
      let stage2 = String.concat "|" stage1_transformed in
      let stage2_split = Stre.nsplitc stage2 '|' in

      (* Stage 3: Filter and transform *)
      let stage3 = List.filter_map (fun s ->
        if String.length s mod 2 = 0 then
          Some (String.uppercase_ascii s)
        else if String.length s > 3 then
          Some (Stre.slice ~first:1 ~last:(String.length s - 1) s)
        else
          None
      ) stage2_split in

      (* Stage 4: Cross-product operations *)
      let stage4 = if List.length stage3 < 100 then
        List.concat_map (fun s1 ->
          List.map (fun s2 ->
            if String.length s1 + String.length s2 < 50 then
              s1 ^ "_" ^ s2
            else
              Stre.slice ~first:0 ~last:10 s1 ^ "_" ^ Stre.slice ~first:0 ~last:10 s2
          ) (ExtList.List.take 5 stage3)
        ) (ExtList.List.take 10 stage3)
      else
        stage3 in

      (* Cache intermediate results *)
      Hashtbl.replace transformation_cache i stage4;

      (* Create references between stages *)
      if i > 10 then begin
        match Hashtbl.find_opt transformation_cache (i - 5) with
        | Some old_stage ->
            let combined = ExtList.List.take 10 stage4 @ ExtList.List.take 10 old_stage in
            stage_results := combined :: !stage_results
        | None -> ()
      end;

      (* Clear old cache entries *)
      if i mod 50 = 0 then
        Hashtbl.iter (fun k _ ->
          if k < i - 20 then Hashtbl.remove transformation_cache k
        ) transformation_cache;

      (* Trim stage results *)
      if List.length !stage_results > 50 then
        stage_results := ExtList.List.take 25 !stage_results
    done
  in

  run_benchmark ~config ~name:"Transformation Chains" ~test_fn ()

(* Main benchmark suite runner *)
let run_all_benchmarks ?(config = default_config) () =
  Printf.printf "\n========================================\n";
  Printf.printf "  Stre GC Benchmark Suite\n";
  Printf.printf "========================================\n\n";

  Printf.printf "Configuration:\n";
  Printf.printf "  Warmup iterations: %d\n" config.warmup_iterations;
  Printf.printf "  Benchmark iterations: %d\n" config.benchmark_iterations;
  Printf.printf "  GC stats: %s\n" (if config.gc_stats then "enabled" else "disabled");
  Printf.printf "\n";

  (* Set GC parameters for more predictable behavior *)
  let original_gc = Gc.get () in
  Gc.set { original_gc with
    Gc.minor_heap_size = 262144;  (* 256KB minor heap *)
    Gc.major_heap_increment = 126976;  (* ~124KB increment *)
  };

  let results = [
    ("Split Storm", bench_split_storm config);
    ("Substring Slicing", bench_substring_slicing config);
    ("Pattern Operations", bench_pattern_operations config);
    ("Concatenation Chains", bench_concatenation_chains config);
    ("Enum String Ops", bench_enum_string_ops config);
    ("Mixed-size Allocs", bench_mixed_size_allocations config);
    ("String Building", bench_string_building config);
    ("Transform Chains", bench_transformation_chains config);
  ] in

  Printf.printf "\n\n========================================\n";
  Printf.printf "  Summary\n";
  Printf.printf "========================================\n\n";

  let total_time = List.fold_left (fun acc (_, (time, _)) -> acc +. time) 0.0 results in

  List.iter (fun (name, (time, stats)) ->
    Printf.printf "%-20s: %7.3fs | Minor GCs: %4d | Major GCs: %3d | Allocated: %.0fMB\n"
      name time stats.minor_collections stats.major_collections
      (stats.minor_words /. 1_000_000.0)
  ) results;

  Printf.printf "\nTotal time: %.3f seconds\n" total_time;

  (* Restore original GC settings *)
  Gc.set original_gc

(* Command-line interface *)
let () =
  let config = ref default_config in

  let usage = "Usage: " ^ Sys.argv.(0) ^ " [options]" in
  let spec = [
    ("-warmup", Arg.Int (fun n -> config := { !config with warmup_iterations = n }),
     "Number of warmup iterations");
    ("-iterations", Arg.Int (fun n -> config := { !config with benchmark_iterations = n }),
     "Number of benchmark iterations");
    ("-verbose", Arg.Unit (fun () -> config := { !config with verbose = true }),
     "Enable verbose output");
    ("-no-gc-stats", Arg.Unit (fun () -> config := { !config with gc_stats = false }),
     "Disable GC statistics output");
  ] in

  Arg.parse spec (fun _ -> ()) usage;
  run_all_benchmarks ~config:!config ()