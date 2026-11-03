(* An example ppx built with [Ppx_helpers.Deriving_args]. For each type [ty],
   [@@deriving sexp_args] derives [ty_sexp_args : Sexplib0.Sexp.t], which returns a sexp
   of the arguments the deriving clause was invoked with. *)

open! Stdppx
open Ppxlib
open Ast_builder.Default
module Deriving_args = Ppx_helpers.Deriving_args

type config =
  { flag : bool
  ; name : string option
  ; count : int option
  }

let str_spec () =
  let%map_open.Deriving_args flag = flag "flag"
  and name = arg "name" Ast_pattern.(estring __)
  and count = arg "count" Ast_pattern.(eint __) in
  { flag; name; count }
;;

let sexp_of_config { flag; name; count } : Sexp.t =
  let opt to_string : _ option -> Sexp.t = function
    | None -> List []
    | Some x -> List [ Atom (to_string x) ]
  in
  List
    [ List [ Atom "flag"; Atom (string_of_bool flag) ]
    ; List [ Atom "name"; opt Fn.id name ]
    ; List [ Atom "count"; opt string_of_int count ]
    ]
;;

let rec exp_of_sexp ~loc : Sexp.t -> expression = function
  | Atom atom -> [%expr Sexplib0.Sexp.Atom [%e estring ~loc atom]]
  | List list ->
    [%expr Sexplib0.Sexp.List [%e elist ~loc (List.map list ~f:(exp_of_sexp ~loc))]]
;;

let sexp_args_name td = Ppx_helpers.mangle_unboxed td.ptype_name.txt ^ "_sexp_args"

(* For each type [ty], emit [let ty_sexp_args = <sexp of the deriver's arguments>]. *)
let expand ~ctxt (_rec_flag, type_declarations) config =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let sexp = exp_of_sexp ~loc (sexp_of_config config) in
  List.map type_declarations ~f:(fun td ->
    let name = sexp_args_name td in
    [%stri let [%p pvar ~loc name] = [%e sexp]])
;;

let str_type_decl =
  Deriving_args.to_generator_v2 (Deriving_args.pack (str_spec ())) ~f:expand
;;

let expand_sig ~ctxt (_rec_flag, type_declarations) () =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
    let name = sexp_args_name td in
    value_description
      ~loc
      ~name:{ loc; txt = name }
      ~type_:[%type: Sexplib0.Sexp.t]
      ~prim:[]
    |> psig_value ~loc)
;;

let sig_type_decl = Deriving_args.to_generator_v2 Deriving_args.no_args ~f:expand_sig
let () = Deriving.add "sexp_args" ~str_type_decl ~sig_type_decl |> Deriving.ignore
