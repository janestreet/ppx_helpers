open! Stdppx
open Ppxlib
open Ast_builder.Default

let hide_from_docs ~loc sigis =
  List.concat [ [ [%sigi: (**/**)] ]; sigis; [ [%sigi: (**/**)] ] ]
;;

let[@tail_mod_cons] rec simplify_doc_toggling = function
  | [%sigi: (**/**)] :: [%sigi: (**/**)] :: rest -> simplify_doc_toggling rest
  | sigi :: rest -> sigi :: simplify_doc_toggling rest
  | [] -> []
;;

let demangle_template typename =
  List.init ~len:(String.length typename - 1) ~f:Fn.id
  |> List.find_opt ~f:(fun i -> Char.(typename.[i] = '_' && typename.[i + 1] = '_'))
  |> Option.map ~f:(fun i -> String.prefix typename i, String.drop_prefix typename i)
  |> Option.value ~default:(typename, "")
;;

(* These are private functions taken from [Ppxlib.Ast_builder] with minor adjustments to
   suit our purposes. *)
open struct
  (* Like [Ast_builder.unapplied_type_constr_conv_without_apply], but
     returns the identifier directly rather than a [pexp_ident]. *)
  let unapplied_type_constr_conv_without_apply ~loc ~functor_ (ident : Longident.t) ~f =
    match ident with
    | Lident n -> { txt = Lident (f ~functor_ n); loc }
    | Ldot (arg, n) -> { txt = Ldot (arg, f ~functor_ n); loc }
    | Lapply _ -> Location.raise_errorf ~loc "unexpected applicative functor type"
  ;;

  (* [Ppxlib] inlines this function in [Ast_builder.unapplied_type_constr_conv] directly;
     we pull it out for readability. *)
  let functor_conv_name_and_args module_path n =
    let rec gather_lapply functor_args : Longident.t -> _ * Longident.t * _ = function
      | Lapply (rest, arg) -> gather_lapply (arg :: functor_args) rest
      | Lident functor_ -> String.uncapitalize_ascii functor_, Lident n, functor_args
      | Ldot (functor_path, functor_) ->
        String.uncapitalize_ascii functor_, Ldot (functor_path, n), functor_args
    in
    gather_lapply [] module_path
  ;;
end

let type_constr_conv_and_apply ~loc:apply_loc { loc; txt = longident } ~f apply =
  let loc = { loc with loc_ghost = true } in
  match (longident : Longident.t) with
  | Lident _ | Ldot ((Lident _ | Ldot _), _) | Lapply _ ->
    let ident =
      unapplied_type_constr_conv_without_apply ~functor_:None longident ~loc ~f
    in
    apply ~loc:apply_loc ident []
  | Ldot ((Lapply _ as module_path), n) ->
    let module_path, ident, functor_args = functor_conv_name_and_args module_path n in
    apply
      ~loc:apply_loc
      (unapplied_type_constr_conv_without_apply
         ~functor_:(Some module_path)
         ident
         ~loc
         ~f)
      functor_args
;;

let type_constr_conv_expr ~loc longident ~f args =
  let f ~functor_ x = f ?functor_ x in
  type_constr_conv_and_apply ~loc longident ~f (fun ~loc ident functor_args ->
    let expr = pexp_ident ~loc ident in
    let functor_args =
      List.map functor_args ~f:(fun path ->
        pexp_pack ~loc (pmod_ident ~loc { txt = path; loc }))
    in
    match functor_args @ args with
    | [] -> expr
    | args -> eapply ~loc expr args)
;;

let type_constr_conv_pat ~loc longident ~f =
  let f ~functor_ x = f ?functor_ x in
  type_constr_conv_and_apply ~loc longident ~f (fun ~loc ident _ ->
    match ident.txt with
    | Lident name -> ppat_var ~loc { loc; txt = name }
    | _ ->
      ppat_extension
        ~loc
        (Location.error_extensionf
           ~loc
           "Invalid identifier %s for converter in pattern position. Only simple \
            identifiers (like t or string) or applications of functors with simple \
            identifiers (like M(K).t) are supported."
           (Longident.name longident.txt)))
;;

let mangle_unboxed name =
  if String.is_suffix name ~suffix:"#" then String.drop_suffix name 1 ^ "_u" else name
;;

let has_unboxed_attribute td =
  List.exists td.ptype_attributes ~f:(fun attr ->
    match attr.attr_name.txt with
    | "unboxed" | "ocaml.unboxed" -> true
    | _ -> false)
;;

let implicit_unboxed_record td =
  let make_immutable ld =
    { ld with
      pld_mutable = Immutable
    ; pld_attributes =
        List.filter ld.pld_attributes ~f:(fun { attr_name; _ } ->
          not (String.equal attr_name.txt "deprecated_mutable"))
    }
  in
  match td.ptype_kind with
  | Ptype_record lds when not (has_unboxed_attribute td) ->
    Some
      { td with
        ptype_kind =
          Ppxlib_jane.Shim.Type_kind.Ptype_record_unboxed_product
            (List.map lds ~f:make_immutable)
          |> Ppxlib_jane.Shim.Type_kind.to_parsetree
      ; ptype_name = { td.ptype_name with txt = td.ptype_name.txt ^ "#" }
      }
  | _ -> None
;;

let with_implicit_unboxed_records ~unboxed tds =
  match unboxed with
  | false -> tds
  | true ->
    List.concat_map tds ~f:(fun td ->
      match implicit_unboxed_record td with
      | None -> [ td ]
      | Some td_u -> [ td; td_u ])
;;
