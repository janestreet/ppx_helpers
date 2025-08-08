open! Stdppx
open Ppxlib

(** Common utility functions for PPXs that we may eventually upstream to ppxlib *)

(** [hide_from_docs] surrounds a list of signature items with the [(**/**)] comment, which
    toggles odoc generation. *)
val hide_from_docs : loc:Location.t -> signature_item list -> signature_item list

(** [simplify_doc_toggling] removes consecutive [(**/**)] attributes in a signature *)
val simplify_doc_toggling : signature_item list -> signature_item list

(** [demangle_template] separates an identifier on the first ["__"], which by our
    conventions corresponds to the mangling suffix added by [ppx_template]. Other ppxs use
    this to recognize names mangled by [ppx_template] and treat them specially. *)
val demangle_template : string -> string * string

(** [type_constr_conv_expr] is the standard way to map (long) identifiers to conversion
    functions, for preprocessors that creates values that follow the structure of types.

    [f] produces the last part of a long identifier (the part after the module path) for
    the conversion function, given the name of the functor module in the type (if there is
    one) and the type name itself.

    For example, the arguments to [f] are:
    - [None, "t"] if the type is [t]
    - [None, "foo"] if the type is [A.B.foo]
    - [Some "f", "foo"] if the type is [A.B.F(A1)(A2).foo]

    [type_constr_conv_expr] assembles the module path from the type, the conversion
    function basename produced by [f], any modules passed as arguments to the functor in
    the type, and the list of additional expression arguments into an expression
    representing the fully-applied conversion function call.

    For example,
    {[
      type_constr_conv_expr
        ~loc
        lident
        (fun ?functor_ t ->
          sprintf
            "sexp_of_%s%s"
            (Option.value_map functor_ ~f:(fun functor_ -> functor_ ^ "__") ~default:"")
            t)
        [ [%expr arg] ]
    ]}
    is:

    - [sexp_of_t arg] if [lident] is [t]
    - [A.B.sexp_of_foo arg] if [lident] is [A.B.foo]
    - [A.B.sexp_of_f__foo (module A1) (module A2) arg] if [lident] is [A.B.F(A1)(A2).foo] *)
val type_constr_conv_expr
  :  loc:Location.t (** [loc] to use for the application expression *)
  -> Longident.t loc (** Identifier of type for which to generate a conversion *)
  -> f:(?functor_:string -> string -> string)
       (** How to build up the conversion function name *)
  -> expression list (** Additional arguments to the conversion *)
  -> expression

(** Like [type_constr_conv_expr], but generates a [pattern] for defining a type
    constructor conversion function. [type_constr_conv_pat] only works on that are a
    simple identifier, without any dots or functors. This is because e.g. [sexp_of_t] is a
    valid pattern, but [A.B.sexp_of_foo] is not a pattern. *)
val type_constr_conv_pat
  :  Longident.t loc
  -> f:(?functor_:string -> string -> string)
  -> pattern

(** Converts implicit unboxed type names (e.g. [t#]) into names that can be used as
    identifiers (e.g. [t_u]). Leaves all other type names alone. *)
val mangle_unboxed : String.t -> String.t

(** Reports whether a type has the [[@@ocaml.unboxed]] or [[@@unboxed]] attribute. *)
val has_unboxed_attribute : type_declaration -> bool

(** Returns the implicit unboxed version of a type declaration. Produces [None] if a type
    declaration doesn't have an implicit unboxed version. *)
val implicit_unboxed_record : type_declaration -> type_declaration option

(** For derivers that take an [~unboxed] flag. If [unboxed], each [td] is followed by
    [implicit_unboxed_record td]. If [not unboxed], returns the input unchanged. *)
val with_implicit_unboxed_records
  :  unboxed:bool
  -> type_declaration list
  -> type_declaration list
