open! Stdppx
open Ppxlib
open Ast_builder.Default

(* These are private functions taken from [Ppxlib.Ast_builder] with minor adjustments to
   suit our purposes. *)
open struct
  (* Like [Ast_builder.unapplied_type_constr_conv_without_apply], but
     returns the identifier directly rather than a [pexp_ident]. *)
  let unapplied_type_constr_conv_name_without_apply ~loc (ident : Longident.t) ~f =
    match ident with
    | Lident n -> { txt = Lident (f n); loc }
    | Ldot (path, n) -> { txt = Ldot (path, f n); loc }
    | Lapply _ -> Location.raise_errorf ~loc "unexpected applicative functor type"
  ;;

  (* [Ppxlib] inlines this function in [Ast_builder.unapplied_type_constr_conv] directly;
     we pull it out for readability. *)
  let functor_conv_name_and_args module_path n =
    let suffix_n functor_ = String.uncapitalize_ascii functor_ ^ "__" ^ n in
    let rec gather_lapply functor_args : Longident.t -> Longident.t * _ = function
      | Lapply (rest, arg) -> gather_lapply (arg :: functor_args) rest
      | Lident functor_ -> Lident (suffix_n functor_), functor_args
      | Ldot (functor_path, functor_) ->
        Ldot (functor_path, suffix_n functor_), functor_args
    in
    gather_lapply [] module_path
  ;;
end

(* Like [Ast_builder.unapplied_type_constr_conv], but returns the name of the value
   rather than constructing an expression. *)
let type_constr_conv_name { Loc.loc; txt = longident } ~f =
  let loc = { loc with loc_ghost = true } in
  match (longident : Longident.t) with
  | Lident _ | Ldot ((Lident _ | Ldot _), _) | Lapply _ ->
    unapplied_type_constr_conv_name_without_apply longident ~loc ~f
  | Ldot ((Lapply _ as module_path), n) ->
    let ident, _functor_args = functor_conv_name_and_args module_path n in
    unapplied_type_constr_conv_name_without_apply ident ~loc ~f
;;

let type_constr_conv_pattern id ~f =
  match type_constr_conv_name id ~f with
  | { txt = Lident name; loc } -> ppat_var ~loc { loc; txt = name }
  | { loc; _ } ->
    ppat_extension
      ~loc
      (Location.error_extensionf
         ~loc
         "Invalid identifier %s for converter in pattern position. Only simple \
          identifiers (like t or string) or applications of functors with simple \
          identifiers (like M(K).t) are supported"
         (Longident.name id.txt))
;;
