(** HtmlStream GC Benchmark Suite

    This suite is designed to stress the OCaml garbage collector with various
    allocation patterns and heap shape changes over time. Each benchmark targets
    different GC behaviors:
    - Minor collection pressure (ephemeral allocations)
    - Major collection pressure (long-lived data)
    - Heap fragmentation
    - Generational hypothesis violations
    - Large object handling
*)

open Devkit

(* Benchmark configuration *)
type config = {
  warmup_iterations : int;
  benchmark_iterations : int;
}

let default_config = {
  warmup_iterations = 3;
  benchmark_iterations = 10;
}

(* Benchmark runner *)
let run_benchmark ~config ~generate_html ~process_elem () =
  (* Warmup *)
  for _ = 1 to config.warmup_iterations do
    let html = generate_html () in
    let ctx = HtmlStream.init () in
    HtmlStream.parse ~ctx process_elem html
  done;

  (* Force major GC to start from clean state *)
  Gc.full_major ();

  (* Actual benchmark *)
  for _ = 1 to config.benchmark_iterations do
    let html = generate_html () in
    let ctx = HtmlStream.init () in
    HtmlStream.parse ~ctx process_elem html
  done

(* Benchmark 1: Small String Pressure (Minor GC stress) *)
let bench_small_strings config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024) in
    for i = 1 to 10000 do
      Buffer.add_string buf "<p>";
      Buffer.add_string buf (string_of_int i);
      Buffer.add_string buf " small text ";
      Buffer.add_string buf (String.make 10 (char_of_int (65 + i mod 26)));
      Buffer.add_string buf "</p>";
    done;
    Buffer.contents buf
  in

  let collected_texts = ref [] in
  let process_elem = function
    | HtmlStream.Text t ->
        if Random.int 100 < 10 then
          collected_texts := HtmlStream.Raw.project t :: !collected_texts
    | _ -> ()
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Benchmark 2: Attribute List Pressure *)
let bench_attribute_lists config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 2) in
    for i = 1 to 5000 do
      Buffer.add_string buf "<div";
      let num_attrs = 20 + (i mod 30) in
      for j = 1 to num_attrs do
        Printf.bprintf buf " attr%d=\"value%d\"" j (i * j);
      done;
      Buffer.add_string buf ">";
      Buffer.add_string buf (string_of_int i);
      Buffer.add_string buf "</div>";
    done;
    Buffer.contents buf
  in

  let total_attrs = ref 0 in
  let process_elem = function
    | HtmlStream.Tag (_, attrs) ->
        total_attrs := !total_attrs + List.length attrs
    | _ -> ()
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Benchmark 3: Large Block Allocations *)
let bench_large_blocks config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 5) in
    for i = 1 to 1000 do
      let size = 1024 * (1 + i mod 100) in
      Buffer.add_string buf "<script>";
      Buffer.add_string buf (String.make size 'x');
      Buffer.add_string buf "</script>";

      if i mod 2 = 0 then begin
        Buffer.add_string buf "<style>";
        Buffer.add_string buf (String.make (size / 2) 'y');
        Buffer.add_string buf "</style>";
      end;
    done;
    Buffer.contents buf
  in

  let retained_blocks = ref [] in
  let process_elem = function
    | HtmlStream.Script (_, s) | HtmlStream.Style (_, s) ->
        if Random.int 100 < 20 then
          retained_blocks := s :: !retained_blocks
    | _ -> ()
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Benchmark 4: Heap Shape Morphing *)
let bench_morphing_heap config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 3) in
    for phase = 1 to 100 do
      if phase mod 3 = 0 then begin
        for _ = 1 to 100 do
          Buffer.add_string buf "<span>small</span>";
        done
      end;

      if phase mod 3 = 1 then begin
        for depth = 1 to 20 do
          Printf.bprintf buf "<div class=\"level%d\" id=\"node%d\">" depth (phase * depth);
        done;
        Buffer.add_string buf "nested content";
        for _ = 1 to 20 do
          Buffer.add_string buf "</div>";
        done
      end;

      if phase mod 3 = 2 then begin
        Buffer.add_string buf "<script>";
        Buffer.add_string buf (String.make (10240 * phase) 'z');
        Buffer.add_string buf "</script>";
      end
    done;
    Buffer.contents buf
  in

  let phase_data = ref [] in
  let process_elem elem =
    if Random.int 100 < 15 then
      phase_data := elem :: !phase_data;
    if List.length !phase_data > 1000 then
      phase_data := List.tl !phase_data
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Benchmark 5: Fragmentation Stress *)
let bench_fragmentation config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 4) in
    let sizes = [| 10; 100; 1000; 10000; 100; 10; 5000; 50; 500 |] in
    for i = 1 to 2000 do
      let size = sizes.(i mod Array.length sizes) in

      match i mod 4 with
      | 0 ->
          Printf.bprintf buf "<p>%s</p>" (String.make size 'a')
      | 1 ->
          Buffer.add_string buf "<div";
          for j = 1 to (size / 100 + 1) do
            Printf.bprintf buf " a%d=\"%s\"" j (String.make (size / 20) 'b');
          done;
          Buffer.add_string buf ">content</div>"
      | 2 ->
          Printf.bprintf buf "<script>%s</script>" (String.make size 'c')
      | _ ->
          for _ = 1 to (size / 100) do
            Buffer.add_string buf "<span>x</span>";
          done
    done;
    Buffer.contents buf
  in

  let retained = Hashtbl.create 1000 in
  let counter = ref 0 in
  let process_elem elem =
    incr counter;
    if !counter mod 7 = 0 || !counter mod 11 = 0 || !counter mod 13 = 0 then
      Hashtbl.replace retained !counter elem;
    if !counter mod 100 = 0 then
      Hashtbl.iter (fun k _ ->
        if k < !counter - 500 then Hashtbl.remove retained k
      ) retained
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Benchmark 6: Generational Hypothesis Violation *)
let bench_generational_violation config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 2) in
    for batch = 1 to 100 do
      for i = 1 to 100 do
        Printf.bprintf buf "<div id=\"gen_%d_%d\">" batch i;
        Printf.bprintf buf "Generation %d Item %d" batch i;
        for j = 1 to batch do
          Printf.bprintf buf "<span class=\"ref_%d\">%d</span>" j (batch * i * j);
        done;
        Buffer.add_string buf "</div>";
      done;
    done;
    Buffer.contents buf
  in

  let old_generation = ref [] in
  let middle_generation = ref [] in
  let young_generation = ref [] in
  let counter = ref 0 in

  let process_elem elem =
    incr counter;
    if !counter mod 100 = 0 then begin
      old_generation := !middle_generation;
      middle_generation := !young_generation;
      young_generation := [];
    end;
    young_generation := elem :: !young_generation;

    if !counter mod 50 = 0 then begin
      let mixed = !old_generation @ !young_generation in
      young_generation :=
        let rec take n = function
          | [] -> []
          | _ when n <= 0 -> []
          | h :: t -> h :: take (n-1) t
        in
        List.rev (take 10 (List.rev mixed))
    end
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Benchmark 7: Allocation Rate Variation *)
let bench_variable_rate config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 3) in
    for phase = 1 to 50 do
      let intensity =
        int_of_float (50.0 +. 45.0 *. sin (float_of_int phase *. 0.3))
      in

      if intensity < 30 then begin
        for _ = 1 to intensity do
          Buffer.add_string buf "<p>low intensity</p>";
        done
      end
      else if intensity < 70 then begin
        for i = 1 to intensity * 10 do
          Printf.bprintf buf "<div id=\"med%d\">content %d</div>" i i;
        done
      end
      else begin
        for i = 1 to intensity * 20 do
          Buffer.add_string buf "<span";
          for j = 1 to 5 do
            Printf.bprintf buf " a%d=\"v%d\"" j (i*j);
          done;
          Printf.bprintf buf ">%d</span>" i;
        done
      end
    done;
    Buffer.contents buf
  in

  let allocation_history = Array.make 1000 [] in
  let index = ref 0 in

  let process_elem elem =
    let i = !index mod Array.length allocation_history in
    allocation_history.(i) <- elem :: allocation_history.(i);
    incr index;
    if !index mod 100 = 0 then begin
      let clear_index = (!index - 500) mod Array.length allocation_history in
      if clear_index >= 0 then
        allocation_history.(clear_index) <- []
    end
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Benchmark 8: Reference Graph Complexity *)
let bench_complex_references config =
  let generate_html () =
    let buf = Buffer.create (1024 * 1024 * 2) in
    for layer = 1 to 20 do
      for node = 1 to 50 do
        Printf.bprintf buf "<div class=\"layer_%d node_%d\">" layer node;
        for ref_layer = 1 to 5 do
          for ref_node = 1 to 10 do
            Printf.bprintf buf "<a href=\"#layer_%d_node_%d\">ref</a>"
              ((layer + ref_layer) mod 20 + 1)
              ((node + ref_node) mod 50 + 1);
          done;
        done;
        Buffer.add_string buf "</div>";
      done;
    done;
    Buffer.contents buf
  in

  let graph = Hashtbl.create 1000 in
  let edges = ref [] in
  let node_counter = ref 0 in

  let process_elem elem =
    incr node_counter;
    let node_id = !node_counter in
    Hashtbl.add graph node_id elem;

    if node_id > 10 then begin
      for _ = 1 to Random.int 5 + 1 do
        let target = Random.int (node_id - 1) + 1 in
        edges := (node_id, target) :: !edges;
      done
    end;

    if node_id mod 200 = 0 then begin
      edges := List.filter (fun (s, t) -> s > node_id - 1000 && t > node_id - 1000) !edges;
      Hashtbl.iter (fun k _ ->
        if k < node_id - 1000 then Hashtbl.remove graph k
      ) graph
    end
  in

  run_benchmark ~config ~generate_html ~process_elem ()

(* Main benchmark suite runner *)
let run_all_benchmarks ?(config = default_config) () =
  bench_small_strings config;
  bench_attribute_lists config;
  bench_large_blocks config;
  bench_morphing_heap config;
  bench_fragmentation config;
  bench_generational_violation config;
  bench_variable_rate config;
  bench_complex_references config
