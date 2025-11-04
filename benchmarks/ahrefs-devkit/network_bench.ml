(** Network GC Benchmark Suite

    This suite stresses the OCaml garbage collector through intensive network
    parsing and manipulation operations. The benchmarks test:
    - IPv4 address parsing (ragel-based parser creating intermediate values)
    - CIDR subnet calculations with bitwise operations
    - Int32 boxing/unboxing patterns
    - String-to-structured data conversions
    - Network address comparisons and transformations
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
   Benchmark 1: IPv4 Address Parsing Storm
   Tests the ragel-based parser with high allocation rates
   ============================================================================ *)
let bench_ipv4_parsing_storm config =
  let test_fn () =
    let parsed_ips = ref [] in

    (* Generate various IP address formats *)
    for i = 1 to 10000 do
      let ip_strings = [
        Printf.sprintf "%d.%d.%d.%d"
          (i mod 256) ((i * 7) mod 256) ((i * 13) mod 256) ((i * 17) mod 256);
        Printf.sprintf "192.168.%d.%d" (i mod 256) ((i * 3) mod 256);
        Printf.sprintf "10.%d.%d.%d" ((i * 5) mod 256) ((i * 11) mod 256) (i mod 256);
        Printf.sprintf "172.%d.%d.%d" (16 + (i mod 16)) ((i * 2) mod 256) ((i * 19) mod 256);
      ] in

      List.iter (fun ip_str ->
        try
          (* Parse IP address - this uses the ragel parser *)
          let ip = Network.ipv4_of_string_exn ip_str in

          (* Convert back to string to force allocation *)
          let str_back = Network.string_of_ipv4 ip in

          (* Parse CIDR notation *)
          let cidr_str = ip_str ^ "/" ^ string_of_int (8 + (i mod 25)) in
          let cidr = Network.cidr_of_string_exn cidr_str in
          let cidr_back = Network.string_of_cidr cidr in

          (* Retain some parsed values *)
          if i mod 100 = 0 then
            parsed_ips := (ip, cidr, str_back, cidr_back) :: !parsed_ips;

          (* Check subnet membership (more Int32 operations) *)
          let _ = Network.ipv4_matches ip cidr in
          ()
        with _ -> ()
      ) ip_strings;

      (* Clear old retained data *)
      if List.length !parsed_ips > 100 then
        parsed_ips := ExtList.List.take 50 !parsed_ips
    done
  in

  run_benchmark ~config ~name:"IPv4 Parsing Storm" ~test_fn ()

(* ============================================================================
   Benchmark 2: CIDR Subnet Calculations
   Heavy Int32 boxing/unboxing with bitwise operations
   ============================================================================ *)
let bench_cidr_calculations config =
  let test_fn () =
    let subnet_cache = Hashtbl.create 1000 in

    for i = 1 to 5000 do
      (* Generate base subnet *)
      let base_ip = Network.ipv4_of_int32 (Int32.of_int (0x0A000000 + i * 256)) in
      let masks = [8; 16; 24; 28; 30; 32] in

      List.iter (fun mask ->
        let cidr_str = Printf.sprintf "%s/%d" (Network.string_of_ipv4 base_ip) mask in
        let cidr = Network.cidr_of_string_exn cidr_str in
        let net_ip = Network.int32_of_ipv4 (Network.prefix_of_cidr cidr) in
        let net_mask = mask in

        (* Generate IPs within subnet *)
        let test_ips = List.init 50 (fun j ->
          let offset = Int32.of_int j in
          let test_ip = Int32.add net_ip offset in
          Network.ipv4_of_int32 test_ip
        ) in

        (* Check membership for each IP *)
        let members = List.filter (fun ip ->
          Network.ipv4_matches ip cidr
        ) test_ips in

        (* String conversions force allocations *)
        let member_strings = List.map Network.string_of_ipv4 members in

        (* Cache some results *)
        if i mod 20 = 0 then
          Hashtbl.replace subnet_cache (i, mask) member_strings;

        (* Calculate subnet ranges *)
        let subnet_size = Int32.shift_left 1l (32 - net_mask) in
        let broadcast = Int32.sub (Int32.add net_ip subnet_size) 1l in
        let broadcast_ip = Network.ipv4_of_int32 broadcast in
        let _ = Network.string_of_ipv4 broadcast_ip in
        ()
      ) masks;

      (* Clear old cache entries *)
      if i mod 100 = 0 then
        Hashtbl.iter (fun (idx, _) _ ->
          if idx < i - 200 then
            Hashtbl.remove subnet_cache (idx, 0)
        ) subnet_cache
    done
  in

  run_benchmark ~config ~name:"CIDR Calculations" ~test_fn ()

(* ============================================================================
   Benchmark 3: Network Range Operations
   Tests range comparisons and iterations
   ============================================================================ *)
let bench_range_operations config =
  let test_fn () =
    let range_results = ref [] in

    for i = 1 to 2000 do
      (* Generate IP ranges *)
      let start_ip = Network.ipv4_of_int32 (Int32.of_int (0xC0A80000 + i * 10)) in
      let end_ip = Network.ipv4_of_int32 (Int32.of_int (0xC0A80000 + i * 10 + 255)) in

      (* Enumerate IPs in range *)
      let ips_in_range = ref [] in
      let current = ref (Network.int32_of_ipv4 start_ip) in
      let end_val = Network.int32_of_ipv4 end_ip in

      while Int32.compare !current end_val <= 0 do
        let ip = Network.ipv4_of_int32 !current in
        let ip_str = Network.string_of_ipv4 ip in
        ips_in_range := ip_str :: !ips_in_range;
        current := Int32.succ !current
      done;

      (* Compare IPs *)
      let sorted = List.sort (fun a b ->
        let ip_a = Network.ipv4_of_string_exn a in
        let ip_b = Network.ipv4_of_string_exn b in
        Int32.compare (Network.int32_of_ipv4 ip_a) (Network.int32_of_ipv4 ip_b)
      ) !ips_in_range in

      (* Check if IPs are in private ranges *)
      let private_ips = List.filter (fun ip_str ->
        try
          let ip = Network.ipv4_of_string_exn ip_str in
          let ip_val = Network.int32_of_ipv4 ip in
          (* Check common private ranges *)
          (Int32.logand ip_val 0xFF000000l = 0x0A000000l) || (* 10.0.0.0/8 *)
          (Int32.logand ip_val 0xFFF00000l = 0xAC100000l) || (* 172.16.0.0/12 *)
          (Int32.logand ip_val 0xFFFF0000l = 0xC0A80000l)    (* 192.168.0.0/16 *)
        with _ -> false
      ) sorted in

      (* Retain some results *)
      if i mod 50 = 0 then
        range_results := private_ips @ !range_results;

      (* Trim retention *)
      if List.length !range_results > 500 then
        range_results := ExtList.List.take 250 !range_results
    done
  in

  run_benchmark ~config ~name:"Range Operations" ~test_fn ()

(* ============================================================================
   Benchmark 4: Mixed Network Format Parsing
   Tests parsing of various network string formats
   ============================================================================ *)
let bench_mixed_format_parsing config =
  let test_fn () =
    let parsed_data = ref [] in

    for i = 1 to 3000 do
      (* Generate different format strings *)
      let formats = [
        (* Standard dotted decimal *)
        Printf.sprintf "%d.%d.%d.%d"
          ((i * 7) mod 256) ((i * 11) mod 256) ((i * 13) mod 256) ((i * 17) mod 256);
        (* CIDR notation *)
        Printf.sprintf "%d.%d.%d.%d/%d"
          ((i * 3) mod 256) ((i * 5) mod 256) ((i * 7) mod 256) ((i * 9) mod 256)
          (16 + (i mod 17));
        (* With leading zeros (may fail parsing) *)
        Printf.sprintf "%03d.%03d.%03d.%03d"
          (i mod 256) ((i * 2) mod 256) ((i * 3) mod 256) ((i * 4) mod 256);
        (* Edge cases *)
        "0.0.0.0"; "255.255.255.255"; "127.0.0.1";
      ] in

      List.iter (fun format ->
        (* Try parsing as IP *)
        (try
          let ip = Network.ipv4_of_string_exn format in
          let back = Network.string_of_ipv4 ip in
          let int_val = Network.int32_of_ipv4 ip in
          let from_int = Network.ipv4_of_int32 int_val in
          let final = Network.string_of_ipv4 from_int in

          if i mod 100 = 0 then
            parsed_data := (back, final) :: !parsed_data
        with _ -> ());

        (* Try parsing as CIDR *)
        (try
          let cidr = Network.cidr_of_string_exn format in
          let back = Network.string_of_cidr cidr in
          let ip = Network.int32_of_ipv4 (Network.prefix_of_cidr cidr) in
          (* Extract mask from the string format *)
          let mask = try
            let slash_pos = String.index format '/' in
            int_of_string (String.sub format (slash_pos + 1) (String.length format - slash_pos - 1))
          with _ -> 32 in

          (* Calculate network info *)
          let network_addr = Int32.logand ip
            (Int32.lognot (Int32.sub (Int32.shift_left 1l (32 - mask)) 1l)) in
          let network_ip = Network.ipv4_of_int32 network_addr in
          let network_str = Network.string_of_ipv4 network_ip in

          if i mod 100 = 0 then
            parsed_data := (back, network_str) :: !parsed_data
        with _ -> ())
      ) formats;

      (* Clear old data *)
      if List.length !parsed_data > 200 then
        parsed_data := ExtList.List.take 100 !parsed_data
    done
  in

  run_benchmark ~config ~name:"Mixed Format Parsing" ~test_fn ()

(* ============================================================================
   Benchmark 5: Network Address Translation Tables
   Simulates NAT table operations with address mappings
   ============================================================================ *)
let bench_nat_tables config =
  let test_fn () =
    (* NAT translation table *)
    let nat_table = Hashtbl.create 10000 in
    let reverse_table = Hashtbl.create 10000 in

    for i = 1 to 5000 do
      (* Generate internal and external IPs *)
      let internal_ip = Network.ipv4_of_string_exn
        (Printf.sprintf "192.168.%d.%d" ((i * 3) mod 256) (i mod 256)) in
      let external_ip = Network.ipv4_of_string_exn
        (Printf.sprintf "203.0.113.%d" (i mod 256)) in

      (* Generate port mappings *)
      for port = 0 to 9 do
        let internal_port = 1024 + i * 10 + port in
        let external_port = 30000 + i * 10 + port in

        (* Create mapping entries *)
        let internal_addr = (internal_ip, internal_port) in
        let external_addr = (external_ip, external_port) in

        (* Add to tables *)
        Hashtbl.replace nat_table internal_addr external_addr;
        Hashtbl.replace reverse_table external_addr internal_addr;

        (* Simulate lookups *)
        if i mod 10 = 0 then begin
          (* Forward lookup *)
          (match Hashtbl.find_opt nat_table internal_addr with
          | Some (ext_ip, ext_port) ->
              let _ = Network.string_of_ipv4 ext_ip in
              let _ = string_of_int ext_port in
              ()
          | None -> ());

          (* Reverse lookup *)
          (match Hashtbl.find_opt reverse_table external_addr with
          | Some (int_ip, int_port) ->
              let _ = Network.string_of_ipv4 int_ip in
              let _ = string_of_int int_port in
              ()
          | None -> ())
        end
      done;

      (* Expire old entries *)
      if i mod 100 = 0 then begin
        let to_remove = ref [] in
        Hashtbl.iter (fun k _v ->
          let (_ip, port) = k in
          if port < 1024 + (i - 200) * 10 then
            to_remove := k :: !to_remove
        ) nat_table;
        List.iter (Hashtbl.remove nat_table) !to_remove;
        List.iter (fun k ->
          match Hashtbl.find_opt nat_table k with
          | Some v -> Hashtbl.remove reverse_table v
          | None -> ()
        ) !to_remove
      end
    done
  in

  run_benchmark ~config ~name:"NAT Tables" ~test_fn ()

(* ============================================================================
   Benchmark 6: IP Address Sorting and Comparison
   Tests comparison operations that create intermediate values
   ============================================================================ *)
let bench_ip_sorting config =
  let test_fn () =
    let sorted_lists = ref [] in

    for i = 1 to 1000 do
      (* Generate random IP list *)
      let ip_list = List.init 200 (fun j ->
        let idx = i * 200 + j in
        Printf.sprintf "%d.%d.%d.%d"
          ((idx * 7) mod 256) ((idx * 11) mod 256)
          ((idx * 13) mod 256) ((idx * 17) mod 256)
      ) in

      (* Parse all IPs *)
      let parsed = List.map (fun s ->
        try Some (Network.ipv4_of_string_exn s, s)
        with _ -> None
      ) ip_list |> List.filter_map (fun x -> x) in

      (* Sort by IP value *)
      let sorted = List.sort (fun (ip1, _) (ip2, _) ->
        Int32.compare (Network.int32_of_ipv4 ip1) (Network.int32_of_ipv4 ip2)
      ) parsed in

      (* Convert back to strings *)
      let sorted_strings = List.map (fun (ip, _) ->
        Network.string_of_ipv4 ip
      ) sorted in

      (* Group by subnet *)
      let grouped = Hashtbl.create 256 in
      List.iter (fun (ip, orig) ->
        let subnet = Int32.shift_right (Network.int32_of_ipv4 ip) 8 in
        let existing = match Hashtbl.find_opt grouped subnet with
          | Some l -> l
          | None -> []
        in
        Hashtbl.replace grouped subnet ((ip, orig) :: existing)
      ) sorted;

      (* Get largest subnet *)
      let _max_subnet = Hashtbl.fold (fun subnet ips (max_sub, max_count) ->
        let count = List.length ips in
        if count > max_count then (subnet, count) else (max_sub, max_count)
      ) grouped (0l, 0) in

      (* Retain some sorted lists *)
      if i mod 50 = 0 then
        sorted_lists := sorted_strings :: !sorted_lists;

      (* Trim retention *)
      if List.length !sorted_lists > 20 then
        sorted_lists := ExtList.List.take 10 !sorted_lists
    done
  in

  run_benchmark ~config ~name:"IP Sorting" ~test_fn ()

(* ============================================================================
   Benchmark 7: Broadcast and Network Address Calculations
   Tests address boundary calculations with Int32 operations
   ============================================================================ *)
let bench_broadcast_calculations config =
  let test_fn () =
    let boundary_cache = ref [] in

    for i = 1 to 3000 do
      (* Generate various subnet sizes *)
      let subnet_sizes = [30; 29; 28; 27; 26; 25; 24; 23; 22; 21; 20; 16; 8] in

      List.iter (fun mask ->
        let base = Int32.of_int (0x0A000000 + i * 0x10000) in
        let cidr_str = Printf.sprintf "%s/%d"
          (Network.string_of_ipv4 (Network.ipv4_of_int32 base)) mask in

        try
          let cidr = Network.cidr_of_string_exn cidr_str in
          let net_ip = Network.int32_of_ipv4 (Network.prefix_of_cidr cidr) in
          let net_mask = mask in (* mask comes from the loop variable *)

          (* Calculate network address *)
          let mask_val = Int32.lognot (Int32.sub (Int32.shift_left 1l (32 - net_mask)) 1l) in
          let network = Int32.logand net_ip mask_val in
          let network_ip = Network.ipv4_of_int32 network in

          (* Calculate broadcast address *)
          let host_bits = 32 - net_mask in
          let broadcast = Int32.logor network
            (Int32.sub (Int32.shift_left 1l host_bits) 1l) in
          let broadcast_ip = Network.ipv4_of_int32 broadcast in

          (* Calculate first and last usable IPs *)
          let first_usable = if host_bits > 1 then
            Network.ipv4_of_int32 (Int32.succ network)
          else network_ip in
          let last_usable = if host_bits > 1 then
            Network.ipv4_of_int32 (Int32.pred broadcast)
          else broadcast_ip in

          (* Calculate total hosts *)
          let total_hosts = if host_bits >= 2 then
            Int32.sub (Int32.shift_left 1l host_bits) 2l
          else 0l in

          (* Convert all to strings *)
          let boundaries = (
            Network.string_of_ipv4 network_ip,
            Network.string_of_ipv4 broadcast_ip,
            Network.string_of_ipv4 first_usable,
            Network.string_of_ipv4 last_usable,
            Int32.to_string total_hosts
          ) in

          (* Retain some calculations *)
          if i mod 100 = 0 then
            boundary_cache := boundaries :: !boundary_cache
        with _ -> ()
      ) subnet_sizes;

      (* Trim cache *)
      if List.length !boundary_cache > 100 then
        boundary_cache := ExtList.List.take 50 !boundary_cache
    done
  in

  run_benchmark ~config ~name:"Broadcast Calculations" ~test_fn ()

(* ============================================================================
   Benchmark 8: Complex Network Operations
   Combines multiple network operations to create complex allocation patterns
   ============================================================================ *)
let bench_complex_network_ops config =
  let test_fn () =
    let operation_cache = Hashtbl.create 1000 in
    let results = ref [] in

    for i = 1 to 2000 do
      (* Generate source network *)
      let source_net = Network.cidr_of_string_exn
        (Printf.sprintf "10.%d.0.0/16" (i mod 256)) in

      (* Subnet into smaller networks *)
      let base_ip = Network.int32_of_ipv4 (Network.prefix_of_cidr source_net) in
      let subnets = List.init 16 (fun j ->
        let subnet_base = Int32.add base_ip (Int32.of_int (j * 256)) in
        (* Create subnet CIDR string directly since we can't create cidr tuple *)
        Printf.sprintf "%s/24" (Network.string_of_ipv4 (Network.ipv4_of_int32 subnet_base))
      ) in

      (* Parse and validate each subnet *)
      let valid_subnets = List.filter_map (fun s ->
        try
          let cidr = Network.cidr_of_string_exn s in
          (* Check if subnet is within source network *)
          let sub_ip_obj = Network.prefix_of_cidr cidr in
          if Network.ipv4_matches sub_ip_obj source_net then
            Some cidr
          else None
        with _ -> None
      ) subnets in

      (* Generate IPs within each subnet *)
      let all_ips = List.concat_map (fun cidr ->
        let net = Network.int32_of_ipv4 (Network.prefix_of_cidr cidr) in
        List.init 10 (fun k ->
          let ip = Int32.add net (Int32.of_int k) in
          Network.ipv4_of_int32 ip
        )
      ) valid_subnets in

      (* Convert to strings and back *)
      let ip_strings = List.map Network.string_of_ipv4 all_ips in
      let reparsed = List.filter_map (fun s ->
        try Some (Network.ipv4_of_string_exn s) with _ -> None
      ) ip_strings in

      (* Create routing table entries *)
      List.iteri (fun j ip ->
        let key = (i, j) in
        let value = (ip, List.nth_opt valid_subnets (j mod List.length valid_subnets)) in
        Hashtbl.replace operation_cache key value
      ) reparsed;

      (* Perform lookups *)
      if i mod 10 = 0 then begin
        for j = 0 to 50 do
          match Hashtbl.find_opt operation_cache (i - 5, j) with
          | Some (ip, Some cidr) ->
              let ip_str = Network.string_of_ipv4 ip in
              let cidr_str = Network.string_of_cidr cidr in
              results := (ip_str, cidr_str) :: !results
          | _ -> ()
        done
      end;

      (* Clean old entries *)
      if i mod 100 = 0 then begin
        Hashtbl.iter (fun (idx, _) _ ->
          if idx < i - 200 then
            Hashtbl.remove operation_cache (idx, 0)
        ) operation_cache;
        if List.length !results > 200 then
          results := ExtList.List.take 100 !results
      end
    done
  in

  run_benchmark ~config ~name:"Complex Network Ops" ~test_fn ()

(* Main benchmark suite runner *)
let run_all_benchmarks ?(config = default_config) () =
  Printf.printf "\n========================================\n";
  Printf.printf "  Network GC Benchmark Suite\n";
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
    ("IPv4 Parsing", bench_ipv4_parsing_storm config);
    ("CIDR Calculations", bench_cidr_calculations config);
    ("Range Operations", bench_range_operations config);
    ("Mixed Parsing", bench_mixed_format_parsing config);
    ("NAT Tables", bench_nat_tables config);
    ("IP Sorting", bench_ip_sorting config);
    ("Broadcast Calc", bench_broadcast_calculations config);
    ("Complex Ops", bench_complex_network_ops config);
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