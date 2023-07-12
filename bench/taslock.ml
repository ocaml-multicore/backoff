(* TASlock, TTASlock implementations from The Art of Multiprocessor Programming -
   M.Herlihy, N. Shavit, V. Luchangco, M. Spear *)

module type LOCK = sig
  type t

  val create : unit -> t
  val acquire : t -> unit
  val release : t -> unit
end

module TASlock : LOCK = struct
  type t = bool Atomic.t

  let create () = Atomic.make false

  let acquire t =
    while not @@ Atomic.compare_and_set t false true do
      Domain.cpu_relax ()
    done

  let release t = Atomic.set t false
end

module TTASlock : LOCK = struct
  type t = bool Atomic.t

  let create () = Atomic.make false

  exception Lock_acquired

  let acquire t =
    try
      while true do
        while Atomic.get t do
          Domain.cpu_relax ()
        done;
        if Atomic.compare_and_set t false true then raise Lock_acquired
      done
    with Lock_acquired -> ()

  let release t = Atomic.set t false
end

module TTASlock_boff : LOCK = struct
  type t = bool Atomic.t

  let create () = Atomic.make false

  exception Lock_acquired

  let acquire t =
    let backoff = ref @@ Backoff.create () in
    try
      while true do
        while Atomic.get t do
          backoff := Backoff.once !backoff
        done;
        if Atomic.compare_and_set t false true then raise Lock_acquired
      done
    with Lock_acquired -> ()

  let release t = Atomic.set t false
end
