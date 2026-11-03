(** [Base_lite_for_ppxes] serves two main purposes:

    1) It provides a more familiar interface for working with basic types
    2) It makes migration away from depending on [Base] itself easier

    We lazily add definitions to this shim as needed, with the goal of keeping
    [Base_lite_for_ppxes] as a significantly lighter-weight dependency than [Base].

    Most definitions are just rebindings of functions from [Stdlib]/[Stdppx] with
    different names or argument orders; functions that have more complicated
    implementations are lightly tested.

    This library is *not* intended to be used by proper libraries, and is only meant for
    migrating ppxes to have a lighter dependency footprint. If you are writing a ppx, then
    you should instead depend on [Ppx_helpers], and access this library via
    [Ppx_helpers.Base_lite]. *)

module Definitions = struct
  module type Hashable = sig
    type t

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val compare : t -> t -> int
    val hash : t -> int
  end

  module type Intable = sig
    type t

    val of_int_exn : int -> t
    val to_int_exn : t -> int
  end

  module type Summable = sig
    type t

    val zero : t
    val ( + ) : t -> t -> t
  end

  module type Map = sig
    type key
    type 'a t

    val of_alist_multi : (key * 'a) list -> 'a list t
    val of_alist_exn : (key * 'a) list -> 'a t
    val of_alist : (key * 'a) list -> [ `Ok of 'a t | `Duplicate_key of key ]
    val of_alist_reduce : (key * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
    val to_alist : 'a t -> (key * 'a) list
    val fold : 'a t -> init:'acc -> f:(key:key -> data:'a -> 'acc -> 'acc) -> 'acc
    val find : 'a t -> key -> 'a option
    val find_exn : 'a t -> key -> 'a
    val mem : 'a t -> key -> bool
    val set : 'a t -> key:key -> data:'a -> 'a t
    val add_exn : 'a t -> key:key -> data:'a -> 'a t
    val change : 'a t -> key -> f:('a option -> 'a option) -> 'a t
    val update : 'a t -> key -> f:('a option -> 'a) -> 'a t
    val keys : 'a t -> key list
    val singleton : key -> 'a -> 'a t
    val empty : 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(key:key -> data:'a -> 'b) -> 'b t
    val merge_disjoint_exn : 'a t -> 'a t -> 'a t
    val merge_skewed : 'a t -> 'a t -> combine:(key:key -> 'a -> 'a -> 'a) -> 'a t
    val data : 'a t -> 'a list
  end

  module type Set = sig
    type elt
    type t

    val of_list : elt list -> t
    val to_list : t -> elt list
    val singleton : elt -> t
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val union : t -> t -> t
    val union_list : t list -> t
    val empty : t
    val is_empty : t -> bool
    val fold : t -> init:'acc -> f:('acc -> elt -> 'acc) -> 'acc
    val fold_right : t -> init:'acc -> f:(elt -> 'acc -> 'acc) -> 'acc
  end

  module type Monad_infix = sig
    type 'a t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module type Monad = sig
    type 'a t

    include Monad_infix with type 'a t := 'a t
    module Monad_infix : Monad_infix with type 'a t := 'a t

    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val join : 'a t t -> 'a t
    val ignore_m : 'a t -> unit t
    val all : 'a t list -> 'a list t
    val all_unit : unit t list -> unit t

    module Let_syntax : sig
      val return : 'a -> 'a t

      include Monad_infix with type 'a t := 'a t

      module Let_syntax : sig
        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t

        module Open_on_rhs : sig end
      end
    end
  end

  module type Hashtbl = sig
    type key
    type 'a t

    val create : unit -> 'a t
    val clear : 'a t -> unit
    val length : 'a t -> int
    val data : 'a t -> 'a list
    val find : 'a t -> key -> 'a option
    val to_alist : 'a t -> (key * 'a) list
    val add_exn : 'a t -> key:key -> data:'a -> unit
    val find_or_add : 'a t -> key -> default:(unit -> 'a) -> 'a
  end

  module type Hash_set = sig
    type elt
    type t

    val create : unit -> t
    val length : t -> int
    val mem : t -> elt -> bool
    val add : t -> elt -> unit
    val to_list : t -> elt list
  end

  module type Stringable = sig
    type t

    val of_string : string -> t
    val to_string : t -> string
  end

  module type Comparable = sig
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val ( = ) : t -> t -> bool
    val ( <> ) : t -> t -> bool

    (* [Base_lite]'s [Map] and [Set] are stdlib-style functor instantiations, so the
       equivalent here is to expose the instantiated modules. *)
    module Map : Map with type key := t
    module Set : Set with type elt := t
  end

  (* A small subset of [Base.Identifiable.S] *)
  module type Identifiable = sig
    type t

    include Stringable with type t := t
    include Comparable with type t := t

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val hash : t -> int
  end
end

module type Base_lite_for_ppxes = sig
  include module type of struct
    include Stdppx
  end

  val force : 'a Lazy.t -> 'a
  val raise_s : Sexp.t -> 'a
  val compare_int : int -> int -> int
  val compare_string : string -> string -> int
  val phys_equal : 'a -> 'a -> bool
  val all_of_option : 'a list -> 'a option list
  val all_of_unit : unit list

  module Map : sig
    module type S = Definitions.Map

    module Make (Key : Map.OrderedType) : S with type key := Key.t
  end

  module Set : sig
    module type S = Definitions.Set

    module Make (Elt : Set.OrderedType) : S with type elt := Elt.t
  end

  module Array : sig
    include module type of struct
      include Array
    end

    val create : len:int -> 'a -> 'a array
    val of_list_map : 'a list -> f:('a -> 'b) -> 'b array
  end

  module Bool : sig
    include module type of struct
      include Bool
    end

    type t = bool

    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val all : t list
    val to_int : t -> int
  end

  module Char : sig
    include module type of struct
      include Char
    end

    val to_string : t -> string
    val lowercase : char -> char
    val uppercase : char -> char
    val is_lowercase : char -> bool
    val is_uppercase : char -> bool
    val is_whitespace : char -> bool
  end

  module Comparable : sig
    module type S = Definitions.Comparable

    val lift : ('a -> 'a -> int) -> f:('b -> 'a) -> 'b -> 'b -> int
    val lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int
  end

  module Container : sig
    module type Summable = Definitions.Summable
  end

  module Dynamic : sig
    include module type of struct
      include Basement.Dynamic
    end
  end

  module Error : sig
    type t

    val of_string : string -> t
    val of_sexp : Sexp.t -> t
    val createf : ('a, unit, string, t) format4 -> 'a
    val to_string_hum : t -> string
    val to_string_mach : t -> string
  end

  module Or_error : sig
    type 'a t = ('a, Error.t) result

    val error_string : string -> _ t
    val errorf : ('a, unit, string, _ t) format4 -> 'a
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val combine_errors : 'a t list -> 'a list t
  end

  module Exn : sig
    include module type of struct
      include Exn
    end

    type t = exn

    val raise_without_backtrace : t -> 'a
    val to_string : t -> string
  end

  module Fn : sig
    include module type of struct
      include Fun
    end

    val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  end

  module Hash : sig
    include module type of struct
      include Base_internalhash_types
    end

    val create : unit -> t
  end

  module Hashable : sig
    module type Key = Definitions.Hashable
  end

  val hash_fold_int : Hash.t -> int -> Hash.t

  module Hashtbl : sig
    module type S = Definitions.Hashtbl

    module Make (Key : Hashtbl.HashedType) : S with type key := Key.t
  end

  module Hash_set : sig
    module type S = Definitions.Hash_set

    module Make (Elt : Stdlib.Hashtbl.HashedType) : S with type elt := Elt.t
  end

  module Iarray : sig
    include module type of struct
      include Basement.Stdlib_iarray_labels
    end

    module O : sig
      external ( .:() )
        : 'a.
        ('a t[@local_opt]) -> int -> ('a[@local_opt])
        = "%array_safe_get"
      [@@layout_poly]
    end
  end

  include module type of struct
    include Iarray.O
  end

  module Identifiable : sig
    module type S = Definitions.Identifiable
  end

  module Intable : sig
    module type S = Definitions.Intable
  end

  module Int : sig
    include module type of struct
      include Int
    end

    type t = int

    include Intable.S with type t := t
    include Hashable.Key with type t := t
    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t

    external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
    external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
    external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
    external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
    external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
    external ( + ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%addint"
    val zero : t
    val incr : int ref -> unit
    val decr : int ref -> unit
  end

  module Int63 : sig
    type t

    val of_string : string -> t
  end

  module List : sig
    include module type of struct
      include List
    end

    val return : 'a -> 'a t
    val hd : 'a t -> 'a option
    val hd_exn : 'a t -> 'a
    val split_n : 'a t -> int -> 'a t * 'a t

    val fold2_exn
      :  'a list
      -> 'b list
      -> init:'acc
      -> f:('acc -> 'a -> 'b -> 'acc)
      -> 'acc

    val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
    val last_exn : 'a t -> 'a
    val drop_last_exn : 'a t -> 'a list
    val is_sorted : 'a t -> compare:('a -> 'a -> int) -> bool
    val dedup_and_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
    val find_all_dups : 'a t -> compare:('a -> 'a -> int) -> 'a list
    val stable_dedup : 'a t -> compare:('a -> 'a -> int) -> 'a t
    val find : 'a t -> f:('a -> bool) -> 'a option
    val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
    val init : int -> f:(int -> 'a) -> 'a t
    val split_while : 'a t -> f:('a -> bool) -> 'a t * 'a t

    val for_all2
      :  'a t
      -> 'b t
      -> f:('a -> 'b -> bool)
      -> [ `Ok of bool | `Unequal_lengths ]

    val partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t
    val partition_mapi : 'a t -> f:(int -> 'a -> ('b, 'c) Either.t) -> 'b t * 'c t
    val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t

    val partition3_map
      :  'a t
      -> f:('a -> [ `Fst of 'b | `Snd of 'c | `Trd of 'd ])
      -> 'b t * 'c t * 'd t

    val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
    val foldi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc
    val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b list

    val sum
      :  (module Container.Summable with type t = 'sum)
      -> 'a t
      -> f:('a -> 'sum)
      -> 'sum

    val unzip : ('a * 'b) t -> 'a t * 'b t
    val unzip3 : ('a * 'b * 'c) t -> 'a t * 'b t * 'c t

    module Or_unequal_lengths : sig
      type 'a t =
        | Ok of 'a
        | Unequal_lengths
    end

    val zip : 'a t -> 'b t -> ('a * 'b) t Or_unequal_lengths.t
    val zip_exn : 'a t -> 'b t -> ('a * 'b) t
    val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    val map3_exn : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
    val folding_mapi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc * 'b) -> 'b t
    val filter_opt : 'a option t -> 'a t
    val folding_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'b t
    val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option
    val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a
    val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

    module Assoc : sig
      type ('k, 'v) t = ('k * 'v) list

      val find : ('k, 'v) t -> equal:('k -> 'k -> bool) -> 'k -> 'v option
    end

    module Cartesian_product : sig
      val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    end

    module Let_syntax : sig
      val return : 'a -> 'a t

      module Let_syntax : sig
        val return : 'a -> 'a t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
      end
    end
  end

  module Monad : sig
    module type Infix = Definitions.Monad_infix
    module type S = Definitions.Monad
  end

  (** The same type [Base.Nonempty_list] uses, so values are interchangeable, with the
      handful of functions ppx code needs. *)
  module Nonempty_list : sig
    type 'a t = 'a Nonempty_list_type.Nonempty_list.t = ( :: ) of 'a * 'a list

    val of_list_exn : 'a list -> 'a t
    val to_list : 'a t -> 'a list
    val of_list : 'a list -> 'a t option
    val cons : 'a -> 'a t -> 'a t
    val length : _ t -> int
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
    val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
    val fold_right : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
    val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b list
    val exists : 'a t -> f:('a -> bool) -> bool
    val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
    val append : 'a t -> 'a list -> 'a t
    val ( @ ) : 'a t -> 'a t -> 'a t
    val reverse : 'a t -> 'a t
    val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
    val last : 'a t -> 'a
    val drop_last : 'a t -> 'a list
  end

  module Option : sig
    include module type of struct
      include Option
    end

    type 'a t = 'a option =
      | None
      | Some of 'a

    val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t
    val first_some : 'a t -> 'a t -> 'a t
    val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
    val some_if_thunk : bool -> (unit -> 'a) -> 'a t
    val return : 'a -> 'a option
    val some : 'a -> 'a option
    val both : 'a option -> 'b option -> ('a * 'b) option
    val map2 : 'a option -> 'b option -> f:('a -> 'b -> 'c) -> 'c option
    val is_none : _ option -> bool
    val mem : 'a option -> 'a -> equal:('a -> 'a -> bool) -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val some_if : bool -> 'a -> 'a option
    val try_with : (unit -> 'a) -> 'a option
    val value_exn : 'a t -> 'a
    val value_or_thunk : 'a t -> default:(unit -> 'a) -> 'a
    val value_map : 'a t -> f:('a -> 'b) -> default:'b -> 'b
    val all : 'a t list -> 'a list t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t

    module Let_syntax : sig
      val return : 'a -> 'a t

      module Let_syntax : sig
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
      end
    end
  end

  module Printf : sig
    include module type of struct
      include Printf
    end

    val failwithf : ('a, unit, string, unit -> 'b) format4 -> 'a
    val invalid_argf : ('a, unit, string, unit -> 'b) format4 -> 'a
  end

  module Queue : sig
    type 'a t

    val create : unit -> 'a t
    val enqueue : 'a t -> 'a -> unit
    val dequeue : 'a t -> 'a option
    val dequeue_exn : 'a t -> 'a
    val to_list : 'a t -> 'a list
    val of_list : 'a list -> 'a t
  end

  module Ref : sig
    type 'a t = 'a ref = { mutable contents : 'a }

    val replace : 'a t -> ('a -> 'a) -> unit
  end

  module Result : sig
    include module type of struct
      include Result
    end

    type ('ok, 'err) t = ('ok, 'err) result =
      | Ok of 'ok
      | Error of 'err

    val return : 'ok -> ('ok, 'err) t
    val fail : 'err -> ('ok, 'err) t
    val all : ('ok, 'err) t list -> ('ok list, 'err) t
    val to_either : ('ok, 'err) t -> ('ok, 'err) Either.t
    val combine_errors : ('ok, 'err) t list -> ('ok list, 'err list) t
    val map_error : ('ok, 'err1) t -> f:('err1 -> 'err2) -> ('ok, 'err2) t

    module Let_syntax : sig
      val return : 'ok -> ('ok, 'err) t
      val ( >>| ) : ('ok1, 'err) t -> ('ok1 -> 'ok2) -> ('ok2, 'err) t
      val ( >>= ) : ('ok1, 'err) t -> ('ok1 -> ('ok2, 'err) t) -> ('ok2, 'err) t

      module Let_syntax : sig
        val map : ('ok1, 'err) t -> f:('ok1 -> 'ok2) -> ('ok2, 'err) t
        val bind : ('ok1, 'err) t -> f:('ok1 -> ('ok2, 'err) t) -> ('ok2, 'err) t
        val both : ('ok1, 'err) t -> ('ok2, 'err) t -> ('ok1 * 'ok2, 'err) t
      end
    end
  end

  module Sexp : sig
    include module type of struct
      include Sexplib0.Sexp
    end
  end

  module Stringable : sig
    module type S = Definitions.Stringable
  end

  module Sexpable : sig
    module Of_stringable (M : sig
        type t

        val to_string : t -> string
        val of_string : string -> t
      end) : sig
      val sexp_of_t : M.t -> Sexp.t
      val t_of_sexp : Sexp.t -> M.t
    end
  end

  module Staged : sig
    type 'a t

    val stage : 'a -> 'a t
    val unstage : 'a t -> 'a
  end

  module Stack : sig
    type 'a t

    val create : unit -> 'a t
    val push : 'a t -> 'a -> unit
    val pop : 'a t -> 'a option
    val pop_exn : 'a t -> 'a
  end

  module String : sig
    include module type of struct
        include String
      end
      with module Map := String.Map
       and module Set := String.Set

    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t
    module Table : Hashtbl.S with type key := t

    val of_string : string -> t
    val to_string : t -> string
    val sexp_of_t : t -> Sexp.t
    val findi : string -> f:(int -> char -> bool) -> (int * char) option
    val substr_index_all : string -> may_overlap:bool -> pattern:string -> int list
    val lstrip : ?drop:(char -> bool) -> string -> string
    val split_lines : string -> string list
    val is_suffix : string -> suffix:string -> bool
    val rstrip : ?drop:(char -> bool) -> string -> string
    val strip : ?drop:(char -> bool) -> string -> string
    val rsplit2 : string -> on:char -> (string * string) option
    val filter : string -> f:(char -> bool) -> string
    val index_from : string -> int -> char -> int option
    val substr_index : ?pos:int -> string -> pattern:string -> int option
    val chop_prefix : string -> prefix:string -> string option
    val chop_suffix : string -> suffix:string -> string option
    val chop_suffix_exn : string -> suffix:string -> string
    val split : string -> on:char -> string list
    val concat : ?sep:string -> string list -> string
    val tr : target:char -> replacement:char -> t -> t
    val is_substring : t -> substring:t -> bool
    val lowercase : string -> string
    val uppercase : string -> string
    val capitalize : string -> string
    val uncapitalize : string -> string
    val mem : string -> char -> bool
  end

  module Source_code_position : sig
    type t = Stdlib.Lexing.position =
      { pos_fname : string
      ; pos_lnum : int
      ; pos_bol : int
      ; pos_cnum : int
      }

    (** [of_pos Stdlib.__POS__] is like [[%here]] but without using ppx. *)
    val of_pos : string * int * int * int -> t

    val sexp_of_t : t -> Sexp.t
  end

  module Type_equal : sig
    type ('a, 'b) t = T : ('a, 'a) t
  end

  module With_return : sig
    type -'a return = private { return : 'b. 'a -> 'b } [@@unboxed]

    val with_return : 'a. ('a return -> 'a) -> 'a
  end
end
