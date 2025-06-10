open! Stdppx
open Ppxlib

(** Common utility functions for PPXs that we may eventually upstream to ppxlib *)

(** Generate a [pattern] for defining a type constructor conversion function *)
val type_constr_conv_pattern
  :  longident Loc.t
  -> f:(string -> string) (** Transform each component of the identifier *)
  -> pattern
