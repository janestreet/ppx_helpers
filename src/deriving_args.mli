open Ppxlib

(** A spec for a deriver's arguments.
    - ['config] is the value the spec produces
    - ['output] is the return type of the associated argument-parsing function. The
      complete spec used for a ppx should have the generated AST type as its ['output],
      e.g. for [to_generator_v2]. Partial specs may have other ['output] types, such as
      for combining with [both].
    - ['fn] is the curried generator function needed for [Deriving.Args.t] *)
type ('config, 'output, 'fn) t

(** Hides the ['fn] parameter of a spec. This makes it easier to export from a module,
    without having to name the ['fn] type that is otherwise useless to the client. *)
type ('config, 'output) packed =
  | T : ('config, 'output, 'fn) t -> ('config, 'output) packed

module type Combinators := sig
  (** A spec that consumes no arguments and always produces [config]. *)
  val return : 'config -> ('config, 'output, 'output) t

  (** Transform the config value *)
  val map : ('a, 'output, 'fn) t -> f:('a -> 'b) -> ('b, 'output, 'fn) t

  (** Infix form of [map] *)
  val ( >>| ) : ('a, 'output, 'fn) t -> ('a -> 'b) -> ('b, 'output, 'fn) t

  (** Combine two specs, concatenating their arguments. The arguments of the first spec
      come before those of the second. *)
  val both : ('a, 'mid, 'fn) t -> ('b, 'output, 'mid) t -> ('a * 'b, 'output, 'fn) t
end

(** [Deriving.Args.param]-like constructors for [t]. *)
module type Arg_parsers := sig
  (** [flag name] is [false] when the flag is absent, and [true] when it is present *)
  val flag : string -> (bool, 'output, bool -> 'output) t

  (** [arg name pattern] is an optional argument [~name:<expr>], with [expr] matched by
      [pattern] (e.g. [estring __], [eint __]). It is [None] when the argument is absent. *)
  val arg
    :  string
    -> (expression, 'a -> 'a option, 'a option) Ast_pattern.t
    -> ('a option, 'output, 'a option -> 'output) t
end

include Combinators
include Arg_parsers

(** Produce the [Deriving.Args.t] used to register the generator *)
val to_args : ('config, 'output, 'fn) t -> ('fn, 'output) Deriving.Args.t

(** [to_fn spec ~f] returns the curried function that [Deriving.Args] hands the parsed
    values to. It assembles those values into a ['config] and passes it to [f], whose
    result becomes the generator's output. *)
val to_fn : ('config, 'output, 'fn) t -> f:('config -> 'output) -> 'fn

(** Hide the ['fn] parameter of a finished spec. Useful for e.g. exporting from a module,
    where the ['fn] parameter is not directly relevant. *)
val pack : ('config, 'output, 'fn) t -> ('config, 'output) packed

(** A packed spec that consumes no arguments and produces [()]. Equivalent to
    [pack (return ())]. *)
val no_args : (unit, 'output) packed

(** Build a ppxlib generator from a packed spec. [f] receives ppxlib's [~ctxt] and the
    input AST, and returns a function from the assembled ['config] to the ['output] AST. *)
val to_generator_v2
  :  ?deps:Deriving.t list
  -> ?unused_code_warnings:bool
  -> ('config, 'output) packed
  -> f:(ctxt:Expansion_context.Deriver.t -> 'input -> 'config -> 'output)
  -> ('output, 'input) Deriving.Generator.t

(** Like [to_generator_v2], but for [Deriving.Generator] generators rather than
    [Deriving.Generator.V2], which receive a [~loc]/[~path] rather than an expansion
    [~ctxt]. *)
val to_generator_v1
  :  ?deps:Deriving.t list
  -> ?unused_code_warnings:bool
  -> ('config, 'output) packed
  -> f:(loc:Location.t -> path:string -> 'input -> 'config -> 'output)
  -> ('output, 'input) Deriving.Generator.t

(** Supports [let%map] / [let%map_open] syntax (requires [ppx_let] in the using library). *)
module Let_syntax : sig
  module Let_syntax : sig
    include Combinators

    module Open_on_rhs : sig
      include Combinators
      include Arg_parsers
    end
  end
end
