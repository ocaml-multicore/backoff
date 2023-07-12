open Taslock

let mean ?(cut_minmax = 0) data =
  let data = List.sort compare data in
  let length = List.length data in
  if cut_minmax * 2 > length then failwith "Not enougth data";
  let data =
    List.filteri (fun i _ -> i >= cut_minmax && i <= length - cut_minmax) data
  in
  let sum = List.fold_left (fun curr_sum b -> curr_sum +. b) 0. data in
  let n = Int.to_float (List.length data) in
  sum /. n

let print ~gnuplot res =
  if gnuplot then
    let n_domains = List.hd res |> snd |> List.map fst in
    let nlines = List.length n_domains in
    let names, lines =
      List.fold_left
        (fun (names, acc) (name, res) ->
          ( names ^ name ^ "\t",
            List.map2
              (fun tmp (_, mean) -> tmp ^ "\t" ^ Float.to_string mean)
              acc res ))
        ("ndomains\t", List.init nlines (fun _ -> ""))
        res
    in
    let lines =
      names
      :: List.map2 (fun line nd -> Int.to_string nd ^ line) lines n_domains
    in
    List.iter (Format.printf "%s@.") lines
  else
    List.iter
      (fun (name, means) ->
        Format.printf "%s : @." name;
        List.iter
          (fun (ndomain, mean) ->
            Format.printf "  for ndomain= %d : %f.6@." ndomain mean)
          means)
      res

(* This test is here to compare a TAS lock performances with or without backoff*)
let test_taslock ?(gnuplot = false) () =
  let test (module Lock : LOCK) ndomain nlock rounds =
    let orch = Orchestrator.init ~total_domains:ndomain ~rounds in
    let counter = ref 0 in
    let lock = Lock.create () in

    let incr () =
      Lock.acquire lock;
      incr counter;
      Lock.release lock
    in

    let domains =
      List.init ndomain (fun _ ->
          Domain.spawn (fun () ->
              Orchestrator.worker orch (fun () ->
                  for _ = 0 to nlock - 1 do
                    incr ()
                  done)))
    in

    let res = Orchestrator.run orch in
    List.iter Domain.join domains;
    res
  in

  let nlock, nround = (10_000, 100) in

  let all_lock =
    [
      ("TAS-lock", (module TASlock : LOCK));
      ("TTAS-lock", (module TTASlock));
      ("TTAS-lock-with-backoff", (module TTASlock_boff));
    ]
  in
  let res =
    List.map
      (fun (name, (module Lock : LOCK)) ->
        ( name,
          (List.map (fun ndomain ->
               let data = test (module Lock) ndomain nlock nround in
               Gc.major ();
               let mean = mean ~cut_minmax:(nround / 20) data in
               (ndomain, mean)))
            [ 1; 2; 4; 6; 8 ] ))
      all_lock
  in
  print ~gnuplot res

let _ = test_taslock ~gnuplot:true ()

(*

dune exec ./main.exe > bench.data

gnuplot -p -e 'plot for [col=2:4] "bench.data" using 1:col with lines title columnheader'

     *)
