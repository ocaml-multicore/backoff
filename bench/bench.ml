let item_count = 3_000_000

type 'a t = { value : 'a; next : 'a t option Atomic.t }

let empty () = { value = Obj.magic (); next = Atomic.make None }

let push ~backoff_once t value =
  let b = Backoff.create () in
  let new_head = ({ value; next = Atomic.make None } : 'a t) in
  let rec push_f b =
    let head = Atomic.get t.next in
    Atomic.set new_head.next head;
    if Atomic.compare_and_set t.next head (Some new_head) then ()
    else
      let b = backoff_once b in
      push_f b
  in
  push_f b

let pop ?lower_wait_log ~backoff_once t =
  let b = Backoff.create ?lower_wait_log () in
  let rec pop_f b =
    let head = Atomic.get t.next in
    match head with
    | None -> None
    | Some node ->
        if Atomic.compare_and_set t.next head (Atomic.get node.next) then
          Some node.value
        else
          let b = backoff_once b in
          pop_f b
  in
  pop_f b

let run_basic ~backoff_once () =
  let stack = empty () in
  let pusher =
    Domain.spawn (fun () ->
        let start_time = Unix.gettimeofday () in
        for i = 1 to item_count do
          push ~backoff_once stack i
        done;
        start_time)
  in
  for _ = 1 to item_count do
    while Option.is_none (pop ~backoff_once stack) do
      ()
    done
  done;
  let end_time = Unix.gettimeofday () in
  let start_time = Domain.join pusher in
  let time_diff = end_time -. start_time in
  time_diff

let run_artificial ~backoff_once () =
  let threads = 6 in
  let stack = empty () in

  (* prepare stack *)
  for i = 1 to item_count do
    push ~backoff_once stack i
  done;

  (* *)
  let counter = Atomic.make 0 in
  let domains =
    List.init threads (fun _ ->
        Domain.spawn (fun () ->
            Atomic.incr counter;
            (* wait for all ready *)
            while Atomic.get counter <= threads do
              ()
            done;

            (* bench !*)
            while Option.is_some (pop ~lower_wait_log:7 ~backoff_once stack) do
              ()
            done;

            Unix.gettimeofday ()))
  in

  (* wait for all domains to start *)
  while Atomic.get counter < threads do
    ()
  done;
  let start_time = Unix.gettimeofday () in

  (* let them run! *)
  Atomic.incr counter;

  (* wait for finish *)
  let end_time =
    List.map Domain.join domains |> List.fold_left Float.min Float.max_float
  in
  let time_diff = end_time -. start_time in
  time_diff

let bench ~run_type ~with_backoff () =
  let backoff_once =
    if with_backoff then Backoff.once else fun (b : Backoff.t) -> b
  in
  let results = ref [] in
  let run =
    match run_type with `Artificial -> run_artificial | `Basic -> run_basic
  in
  for i = 1 to 10 do
    let time = run ~backoff_once () in
    if i > 1 then results := time :: !results
  done;
  let results = List.sort Float.compare !results in
  let median_time = List.nth results 4 in
  let median_throughput = Float.of_int item_count /. median_time in
  let name =
    Printf.sprintf "backoff-%s-%s"
      (if with_backoff then "on" else "off")
      (match run_type with `Artificial -> "artificial" | `Basic -> "basic")
  in
  Benchmark_result.create_generic ~median_time ~median_throughput name

let bench_artificial = bench ~run_type:`Artificial
let bench_basic = bench ~run_type:`Basic

let backoff_benchmarks =
  [
    bench_basic ~with_backoff:true;
    bench_basic ~with_backoff:false;
    bench_artificial ~with_backoff:true;
    bench_artificial ~with_backoff:false;
  ]

let () =
  let results =
    (* todo: should assert no stranded domains between tests. *)
    List.map (fun f -> f ()) backoff_benchmarks
    |> List.map Benchmark_result.to_json
    |> String.concat ", "
  in
  let output =
    Printf.sprintf {|{"results": [%s]}|} results
    (* Cannot use Yojson rewriters as of today none works on OCaml 5.1.0.
       This at least verifies that the manually crafted JSON is well-formed.

       If the type grow, we could switch to running ppx manually on 5.0.0 and
       pasting in its output. *)
    |> Yojson.Basic.prettify
  in
  Printf.printf "%s" output
