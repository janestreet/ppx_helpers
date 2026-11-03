(* Example usage of the [sexp_args] deriver defined in ppx/ppx_helpers_example_ppx *)

module Default = struct
  type t = char [@@deriving sexp_args]
end

module With_flag = struct
  type t = char [@@deriving sexp_args ~flag]
end

module With_args = struct
  type t = char [@@deriving sexp_args ~name:"foo" ~count:3]
end

module With_everything = struct
  type t = char [@@deriving sexp_args ~flag ~name:"foo" ~count:3]
end
