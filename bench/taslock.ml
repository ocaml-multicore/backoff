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

  let rec acquire t =
    if not @@ Atomic.compare_and_set t false true then (
      Domain.cpu_relax ();
      acquire t)

  let release t = Atomic.set t false
end

module TTASlock : LOCK = struct
  type t = bool Atomic.t

  let create () = Atomic.make false

  let rec acquire t =
    if Atomic.get t then (
      Domain.cpu_relax ();
      acquire t)
    else if not (Atomic.compare_and_set t false true) then (
      Domain.cpu_relax ();
      acquire t)

  let release t = Atomic.set t false
end

module TTASlock_boff : LOCK = struct
  type t = bool Atomic.t

  let create () = Atomic.make false

  let rec acquire_ ?(backoff = Backoff.default) t =
    if Atomic.get t then (
      Domain.cpu_relax ();
      acquire_ ~backoff t)
    else if not (Atomic.compare_and_set t false true) then
      acquire_ ~backoff:(Backoff.once backoff) t

  let acquire t = acquire_ t
  let release t = Atomic.set t false
end
