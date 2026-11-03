open! Stdppx
open Ppxlib

type ('config, 'output, 'fn) t =
  | Return : 'config -> ('config, 'output, 'output) t
  | Param : 'a Deriving.Args.param -> ('a, 'output, 'a -> 'output) t
  | Map : ('a, 'output, 'fn) t * ('a -> 'b) -> ('b, 'output, 'fn) t
  | Both : ('a, 'mid, 'fn) t * ('b, 'output, 'mid) t -> ('a * 'b, 'output, 'fn) t

type ('config, 'output) packed =
  | T : ('config, 'output, 'fn) t -> ('config, 'output) packed

module Arg_parsers = struct
  let arg name pattern = Param (Deriving.Args.arg name pattern)
  let flag name = Param (Deriving.Args.flag name)
end

module Combinators = struct
  let return c = Return c
  let map t ~f = Map (t, f)
  let both a b = Both (a, b)
  let ( >>| ) t f = map t ~f
end

include Arg_parsers
include Combinators

let rec to_args_with
  : type config output fn acc.
    (config, output, fn) t -> (acc, fn) Deriving.Args.t -> (acc, output) Deriving.Args.t
  =
  fun spec acc ->
  match spec with
  | Return _ -> acc
  | Param p -> Deriving.Args.( +> ) acc p
  | Map (inner, _) -> to_args_with inner acc
  | Both (a, b) -> to_args_with b (to_args_with a acc)
;;

let to_args spec = to_args_with spec Deriving.Args.empty

let rec to_fn
  : type config output fn. (config, output, fn) t -> f:(config -> output) -> fn
  =
  fun spec ~f ->
  match spec with
  | Return c -> f c
  | Param _ -> fun x -> f x
  | Map (inner, g) -> to_fn inner ~f:(fun a -> f (g a))
  | Both (a, b) -> to_fn a ~f:(fun x -> to_fn b ~f:(fun y -> f (x, y)))
;;

let pack spec = T spec
let no_args = T (Return ())

let to_generator_v2 ?deps ?unused_code_warnings (T spec) ~f =
  Deriving.Generator.V2.make
    ?deps
    ?unused_code_warnings
    (to_args spec)
    (fun ~ctxt input -> to_fn spec ~f:(f ~ctxt input))
;;

let to_generator_v1 ?deps ?unused_code_warnings (T spec) ~f =
  Deriving.Generator.make
    ?deps
    ?unused_code_warnings
    (to_args spec)
    (fun ~loc ~path input -> to_fn spec ~f:(f ~loc ~path input))
;;

module Let_syntax = struct
  module Let_syntax = struct
    include Combinators

    module Open_on_rhs = struct
      include Arg_parsers
      include Combinators
    end
  end
end
