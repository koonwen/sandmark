(** Gzip GC Benchmark Suite

    This suite stresses the OCaml garbage collector through intensive
    compression and decompression operations. The benchmarks test:
    - Buffer-based I/O with continuous allocations
    - Zlib compression/decompression creating temporary buffers
    - Streaming patterns with state management
    - CRC calculations and header processing
    - Variable-sized buffer operations
*)

open Devkit
open ExtLib

(* Helper functions for string compression/decompression *)
let compress_string ?level str =
  (* Note: Gzip_io doesn't expose level parameter, so we ignore it *)
  let _ = level in
  let oc = Gzip_io.output (IO.output_string ()) in
  IO.nwrite_string oc str;
  IO.close_out oc

let uncompress_string str =
  let ic = Gzip_io.input (IO.input_string str) in
  let result = IO.read_all ic in
  IO.close_in ic;
  result

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

(* Helper function to generate test data *)
let generate_test_data size pattern =
  let data = Bytes.create size in
  for i = 0 to size - 1 do
    Bytes.set data i (char_of_int ((i * pattern) mod 256))
  done;
  Bytes.to_string data

(* ============================================================================
   Benchmark 1: Small Buffer Compression Storm
   Creates many small compressions causing frequent buffer allocations
   ============================================================================ *)
let bench_small_buffer_storm config =
  let test_fn () =
    let compressed_data = ref [] in

    for i = 1 to 5000 do
      (* Generate small data chunks of varying sizes *)
      let size = 100 + (i mod 900) in (* 100-1000 bytes *)
      let data = generate_test_data size (i mod 256) in

      (* Compress the data *)
      let compressed = compress_string data in

      (* Decompress to verify *)
      let decompressed = uncompress_string compressed in

      (* Create multiple compression levels *)
      let compressed_fast = compress_string ~level:1 data in
      let compressed_best = compress_string ~level:9 data in

      (* Retain some compressed data *)
      if i mod 100 = 0 then
        compressed_data := compressed :: compressed_fast :: compressed_best :: !compressed_data;

      (* Check sizes to force evaluation *)
      let _ = String.length compressed in
      let _ = String.length decompressed in

      (* Clear old retained data periodically *)
      if List.length !compressed_data > 300 then
        compressed_data := ExtList.List.take 150 !compressed_data
    done
  in

  run_benchmark ~config ~name:"Small Buffer Storm" ~test_fn ()

(* ============================================================================
   Benchmark 2: Large Block Compression
   Tests compression of large blocks causing major heap pressure
   ============================================================================ *)
let bench_large_block_compression config =
  let test_fn () =
    let retained_blocks = ref [] in

    for i = 1 to 100 do
      (* Generate large data blocks *)
      let size = 10000 * (1 + (i mod 10)) in (* 10KB-100KB *)
      let data = generate_test_data size i in

      (* Compress with different strategies *)
      let compressed_default = compress_string data in
      let compressed_filtered = compress_string ~level:5 data in

      (* Decompress both *)
      let decompressed1 = uncompress_string compressed_default in
      let decompressed2 = uncompress_string compressed_filtered in

      (* Verify correctness forces evaluation *)
      assert (decompressed1 = data);
      assert (decompressed2 = data);

      (* Compress in chunks to create intermediate buffers *)
      let chunk_size = 1024 in
      let chunks = ref [] in
      let pos = ref 0 in
      while !pos < String.length data do
        let len = min chunk_size (String.length data - !pos) in
        let chunk = String.sub data !pos len in
        let compressed_chunk = compress_string chunk in
        chunks := compressed_chunk :: !chunks;
        pos := !pos + len
      done;

      (* Retain some large blocks *)
      if i mod 10 = 0 then
        retained_blocks := compressed_default :: compressed_filtered :: !retained_blocks;

      (* Clear old blocks *)
      if List.length !retained_blocks > 20 then
        retained_blocks := ExtList.List.take 10 !retained_blocks
    done
  in

  run_benchmark ~config ~name:"Large Block Compression" ~test_fn ()

(* ============================================================================
   Benchmark 3: Streaming Compression/Decompression
   Tests streaming operations with continuous buffer allocations
   ============================================================================ *)
let bench_streaming_operations config =
  let test_fn () =
    let stream_buffers = ref [] in

    for i = 1 to 500 do
      (* Create input/output channels using strings *)
      let data_size = 5000 + (i * 100) in
      let source_data = generate_test_data data_size i in

      (* Compress using streaming interface *)
      let out_channel = Gzip_io.output (IO.output_string ()) in

      (* Write data in chunks *)
      let chunk_size = 256 in
      let pos = ref 0 in
      while !pos < String.length source_data do
        let len = min chunk_size (String.length source_data - !pos) in
        let chunk = String.sub source_data !pos len in
        IO.nwrite_string out_channel chunk;
        pos := !pos + len
      done;

      let compressed = IO.close_out out_channel in

      (* Decompress using streaming interface *)
      let in_channel = Gzip_io.input (IO.input_string compressed) in
      let decompressed_buf = Buffer.create data_size in

      (* Read in small chunks to stress buffer allocation *)
      let read_buf = Bytes.create 128 in
      let rec read_loop () =
        try
          let n = IO.input in_channel read_buf 0 128 in
          if n > 0 then begin
            Buffer.add_subbytes decompressed_buf read_buf 0 n;
            read_loop ()
          end
        with IO.No_more_input -> ()
      in
      read_loop ();
      IO.close_in in_channel;

      let decompressed = Buffer.contents decompressed_buf in

      (* Verify data integrity *)
      assert (decompressed = source_data);

      (* Retain some buffers *)
      if i mod 50 = 0 then
        stream_buffers := compressed :: decompressed :: !stream_buffers;

      (* Clear old buffers *)
      if List.length !stream_buffers > 100 then
        stream_buffers := ExtList.List.take 50 !stream_buffers
    done
  in

  run_benchmark ~config ~name:"Streaming Operations" ~test_fn ()

(* ============================================================================
   Benchmark 4: Mixed Size Compression Patterns
   Creates fragmentation through varying buffer sizes
   ============================================================================ *)
let bench_mixed_size_patterns config =
  let test_fn () =
    let mixed_cache = Hashtbl.create 1000 in

    for i = 1 to 1000 do
      (* Generate data with exponentially varying sizes *)
      let sizes = [| 64; 128; 256; 512; 1024; 2048; 4096; 8192; 128; 64 |] in
      let size = sizes.(i mod Array.length sizes) in
      let data = generate_test_data size (i * 7) in

      (* Compress with different levels *)
      let levels = [1; 3; 5; 7; 9] in
      let compressed_versions = List.map (fun level ->
        compress_string ~level data
      ) levels in

      (* Decompress all versions *)
      let decompressed_versions = List.map uncompress_string compressed_versions in

      (* Verify all decompressions *)
      List.iter (fun d -> assert (d = data)) decompressed_versions;

      (* Mix compressions of different data *)
      let mixed = String.concat "" compressed_versions in
      let mixed_compressed = compress_string mixed in

      (* Store in cache with non-uniform pattern *)
      if i mod 13 = 0 || i mod 17 = 0 then
        Hashtbl.replace mixed_cache i mixed_compressed;

      (* Create compression chains *)
      let chain = List.fold_left (fun acc comp ->
        compress_string (acc ^ comp)
      ) "" (ExtList.List.take 3 compressed_versions) in

      if i mod 23 = 0 then
        Hashtbl.replace mixed_cache (i + 10000) chain;

      (* Clear old cache entries *)
      if i mod 100 = 0 then
        Hashtbl.iter (fun k _ ->
          if k < i - 200 then Hashtbl.remove mixed_cache k
        ) mixed_cache
    done
  in

  run_benchmark ~config ~name:"Mixed Size Patterns" ~test_fn ()

(* ============================================================================
   Benchmark 5: Concurrent-style Compression
   Simulates multiple compression streams with interleaved operations
   ============================================================================ *)
let bench_concurrent_style config =
  let test_fn () =
    (* Multiple active compression contexts *)
    let active_streams = Array.init 10 (fun _ -> ref []) in
    let completed = ref [] in

    for i = 1 to 2000 do
      let stream_id = i mod Array.length active_streams in
      let stream = active_streams.(stream_id) in

      (* Generate data for this stream *)
      let data = generate_test_data (500 + stream_id * 100) i in

      (* Compress and add to stream *)
      let compressed = compress_string data in
      stream := compressed :: !stream;

      (* Periodically process streams *)
      if i mod 50 = 0 then begin
        (* Process each stream *)
        Array.iteri (fun _idx s ->
          if List.length !s > 5 then begin
            (* Combine compressions *)
            let combined = String.concat "" (List.rev !s) in
            let recompressed = compress_string combined in

            (* Decompress to verify *)
            let _ = uncompress_string recompressed in

            completed := recompressed :: !completed;
            s := []  (* Clear stream *)
          end
        ) active_streams
      end;

      (* Trim completed list *)
      if List.length !completed > 100 then
        completed := ExtList.List.take 50 !completed
    done
  in

  run_benchmark ~config ~name:"Concurrent-style Compression" ~test_fn ()

(* ============================================================================
   Benchmark 6: Compression with Headers and Metadata
   Tests header processing and CRC calculations
   ============================================================================ *)
let bench_headers_metadata config =
  let test_fn () =
    let metadata_cache = ref [] in

    for i = 1 to 2000 do
      (* Generate data with patterns *)
      let data = Printf.sprintf "File_%d_Content_%s"
        i (String.make (100 + i mod 400) (char_of_int (65 + i mod 26))) in

      (* Compress with metadata simulation *)
      let compressed = compress_string data in

      (* Parse compressed data to extract header info *)
      let header_size = min 10 (String.length compressed) in
      let header = String.sub compressed 0 header_size in

      (* Calculate CRC-like checksum *)
      let checksum = ref 0 in
      String.iter (fun c -> checksum := (!checksum * 31 + Char.code c) mod 65536) data;

      (* Create metadata structure *)
      let metadata = (header, !checksum, String.length data, String.length compressed) in

      (* Decompress and verify *)
      let decompressed = uncompress_string compressed in
      assert (decompressed = data);

      (* Recompress with different parameters *)
      let recompressed = compress_string ~level:(1 + i mod 9) decompressed in

      (* Calculate compression ratio *)
      let ratio = float_of_int (String.length compressed) /.
                  float_of_int (String.length data) in

      (* Store metadata *)
      if i mod 50 = 0 then
        metadata_cache := (metadata, ratio, recompressed) :: !metadata_cache;

      (* Trim cache *)
      if List.length !metadata_cache > 100 then
        metadata_cache := ExtList.List.take 50 !metadata_cache
    done
  in

  run_benchmark ~config ~name:"Headers and Metadata" ~test_fn ()

(* ============================================================================
   Benchmark 7: Buffer Reuse and Recycling
   Tests patterns where buffers are reused, stressing generational hypothesis
   ============================================================================ *)
let bench_buffer_recycling config =
  let test_fn () =
    (* Pool of reusable buffers *)
    let buffer_pool = Array.init 20 (fun _ -> Buffer.create 1024) in
    let compressed_pool = ref [] in
    let generation_counter = ref 0 in

    for i = 1 to 1500 do
      let buffer_idx = i mod Array.length buffer_pool in
      let buffer = buffer_pool.(buffer_idx) in

      (* Clear and reuse buffer *)
      Buffer.clear buffer;

      (* Generate data into buffer *)
      for j = 1 to (100 + i mod 200) do
        Buffer.add_string buffer (Printf.sprintf "Line_%d_%d " i j)
      done;

      let data = Buffer.contents buffer in

      (* Compress data *)
      let compressed = compress_string data in

      (* Decompress into another reused buffer *)
      let decode_buffer = buffer_pool.((buffer_idx + 1) mod Array.length buffer_pool) in
      Buffer.clear decode_buffer;
      let decompressed = uncompress_string compressed in
      Buffer.add_string decode_buffer decompressed;

      (* Update generation tracking *)
      incr generation_counter;

      (* Promote some compressions to old generation *)
      if !generation_counter mod 30 = 0 then begin
        compressed_pool := compressed :: !compressed_pool;

        (* Mix old and new data *)
        if List.length !compressed_pool > 10 then begin
          let old_data = List.hd !compressed_pool in
          let mixed = old_data ^ compressed in
          let mixed_compressed = compress_string mixed in
          compressed_pool := mixed_compressed :: List.tl !compressed_pool
        end
      end;

      (* Periodically compact pool *)
      if !generation_counter mod 100 = 0 && List.length !compressed_pool > 50 then
        compressed_pool := ExtList.List.take 25 !compressed_pool
    done
  in

  run_benchmark ~config ~name:"Buffer Recycling" ~test_fn ()

(* ============================================================================
   Benchmark 8: Complex Compression Pipelines
   Creates complex reference patterns through chained compression operations
   ============================================================================ *)
let bench_compression_pipelines config =
  let test_fn () =
    let pipeline_stages = Hashtbl.create 500 in
    let final_results = ref [] in

    for i = 1 to 1000 do
      (* Stage 1: Generate and compress base data *)
      let base_data = generate_test_data (1000 + i * 10) i in
      let stage1 = compress_string base_data in

      (* Stage 2: Decompress, modify, recompress *)
      let decompressed = uncompress_string stage1 in
      let modified = decompressed ^ Printf.sprintf "_modified_%d" i in
      let stage2 = compress_string ~level:5 modified in

      (* Stage 3: Combine with previous stages if available *)
      let stage3 = match Hashtbl.find_opt pipeline_stages (i - 10) with
        | Some (prev1, prev2, _) ->
            let combined = stage1 ^ prev1 ^ stage2 ^ prev2 in
            compress_string ~level:3 combined
        | None ->
            compress_string (stage1 ^ stage2)
      in

      (* Store pipeline stages *)
      Hashtbl.replace pipeline_stages i (stage1, stage2, stage3);

      (* Stage 4: Create multi-level compression *)
      let multi_compressed =
        let temp1 = compress_string ~level:1 base_data in
        let temp2 = compress_string ~level:5 temp1 in
        compress_string ~level:9 temp2
      in

      (* Decompress multi-level *)
      let multi_decompressed =
        let temp1 = uncompress_string multi_compressed in
        let temp2 = uncompress_string temp1 in
        uncompress_string temp2
      in

      (* Verify integrity *)
      assert (multi_decompressed = base_data);

      (* Save final results *)
      if i mod 50 = 0 then
        final_results := (stage3, multi_compressed) :: !final_results;

      (* Clean old pipeline stages *)
      if i mod 100 = 0 then begin
        Hashtbl.iter (fun k _ ->
          if k < i - 50 then Hashtbl.remove pipeline_stages k
        ) pipeline_stages;
        if List.length !final_results > 40 then
          final_results := ExtList.List.take 20 !final_results
      end
    done
  in

  run_benchmark ~config ~name:"Compression Pipelines" ~test_fn ()

(* Main benchmark suite runner *)
let run_all_benchmarks ?(config = default_config) () =
  Printf.printf "\n========================================\n";
  Printf.printf "  Gzip GC Benchmark Suite\n";
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
    ("Small Buffer Storm", bench_small_buffer_storm config);
    ("Large Block Comp", bench_large_block_compression config);
    ("Streaming Ops", bench_streaming_operations config);
    ("Mixed Size Patterns", bench_mixed_size_patterns config);
    ("Concurrent Style", bench_concurrent_style config);
    ("Headers Metadata", bench_headers_metadata config);
    ("Buffer Recycling", bench_buffer_recycling config);
    ("Comp Pipelines", bench_compression_pipelines config);
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