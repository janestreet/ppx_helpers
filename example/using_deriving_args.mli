module Default : sig
  type t = char [@@deriving sexp_args]
end

module With_flag : sig
  type t = char [@@deriving sexp_args]
end

module With_args : sig
  type t = char [@@deriving sexp_args]
end

module With_everything : sig
  type t = char [@@deriving sexp_args]
end
