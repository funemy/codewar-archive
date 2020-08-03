(* Let's implement laziness!
 * A lazy value can be implemented as a closure (also known as thunk),
 * and getting a strict value out of it is just function evaluation. *)

(* Note that you should not use reference unless we explicitly ask for it.
 * Also, whenever you construct a lazy value, make sure to defer
 * all calculation as late as possible *)
module type Lazy = 
sig
  type _ t
  val mk: (unit -> 'a) -> 'a t
  val get: 'a t -> 'a
  val map: ('a -> 'b) -> 'a t -> 'b t
  val return: 'a -> 'a t
  val join: 'a t t -> 'a t
  val bind:  ('a -> 'b t) -> 'a t -> 'b t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  (* The argument is a lazy value that might refer to itself.
   * Tie the value to itself using reference. *)
  val tie: ('a t -> 'a t) -> 'a t 
end;;

module LazyThunk: Lazy with type 'a t = unit -> 'a =
struct
  type 'a t = unit -> 'a
  let mk x = x
  let get x = x()
  let map f x = mk (fun () -> x() |> f)
  let return x = fun () -> x
  let join y = y()
  let bind f x = x() |> f
  let (>>=) x f = x() |> f
  (* this is definitely wrong *)
  let rec tie f = tie f
end;;

(* However, the thunk need to be evaluated each time get is used,
 * instead of at most once. This is outrageous! This is unfair!
 * It can be fixed by caching the result - the thunk is only evaluated
 * if the cache is empty. *)
(* module LazyOption: Lazy with type 'a t = 'a option ref * (unit -> 'a) =
   struct
   end;; *)

(* Notice how there is two components for a Lazy: a thunk and a cache.
 * Here is a pretty cool trick: instead of having two components,
 * just cache the thunk!
 * This is called tagless because we no longer use algebraic data type,
 * which tag the value.
 * We just have a uniform way to handle the value. *)
(* module LazyTagless: Lazy with type 'a t = (unit -> 'a) ref = 
   struct
   end;; *)

(* Notice how most definition of lazy can be derived from other?
 * Try to lookup how module works and refactor them. *)

(* To test that implementation work, do some infinite stream *)
module type StreamSig =
sig
  module L: Lazy
  type 'a stream = Stream of ('a * 'a stream) L.t
  val hd: 'a stream -> 'a
  (* Be as lazy as possible. *)
  val tl: 'a stream -> 'a stream 
  val mk: (unit -> 'a * 'a stream) -> 'a stream
  val gen: ('a -> 'a) -> 'a L.t -> 'a stream
  (* val map: ('a -> 'b) -> 'a stream -> 'b stream *)
  (* val zip: 'a stream -> 'b stream -> ('a * 'b) stream *)
  (* val zipWith: ('a * 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream *)
  (* val takeWhile: ('a -> bool) -> 'a stream -> 'a list *)
  (* val app: 'a list -> 'a stream -> 'a stream *)
  (* fib = tie fib_aux *)
  (* val fib_aux: int stream -> int stream  *)
  (*traverse evey element of 'a stream that is in 'a stream 'stream*)
  (* val join: 'a stream stream -> 'a stream *)
end;;

module Stream (L: Lazy): StreamSig with module L = L =
struct
  module L = L
  type 'a stream = Stream of ('a * 'a stream) L.t
  let hd = function
    | Stream lzy -> fst (L.get lzy)
  let tl = function
    | Stream lzy -> snd (L.get lzy)
  let mk f = Stream (L.mk f)
  let rec gen f init = Stream (L.return (L.get init, gen f init))
end;;