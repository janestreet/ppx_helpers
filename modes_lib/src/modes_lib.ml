open! StdLabels
include Modes_lib_intf.Definitions

type universe =
  | Modes
  | Modalities
  | Crossings

type implication =
  | I : universe list * 'a axis * 'b axis * ('a -> 'b option) -> implication

let implications =
  let all_universes = [ Modes; Modalities; Crossings ] in
  [ I
      ( all_universes
      , Locality
      , Yielding
      , function
        | Global -> Some Unyielding
        | Local -> Some Yielding )
  ; I
      ( all_universes
      , Locality
      , Forkable
      , function
        | Global -> Some Forkable
        | Local -> Some Unforkable )
  ; I
      ( all_universes
      , Statefulness
      , Portability
      , function
        | Stateless -> Some Portable
        | Reading -> Some Shareable
        | Stateful -> Some Nonportable )
  ; I
      ( all_universes
      , Visibility
      , Contention
      , function
        | Read_write -> Some Uncontended
        | Read -> Some Shared
        | Immutable -> Some Contended )
  ; I
      ( [ Modalities; Crossings ]
      , Locality
      , Uniqueness
      , function
        | Global -> Some Aliased
        | Local -> None )
  ]
;;

module Make_wrapper () : Wrapper = struct
  type 'a t = 'a

  let sexp_of_t sexp_of_a = sexp_of_a
  let mk a = a
  let unwrap a = a
end

module Mode = Make_wrapper ()
module Modality = Make_wrapper ()
module Crossing = Make_wrapper ()

module Make_stringable (M : sig
    type t

    val t_of_sexp : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
  end) : sig
  val of_string : string -> M.t
  val to_string : M.t -> string
end = struct
  include M

  let of_string string = t_of_sexp (Atom string)
  let to_string t = Sexplib0.Sexp.to_string (sexp_of_t t) |> String.lowercase_ascii
end

module Modal = struct
  include Modal
  include Make_stringable (Modal)
end

module Nonmodal = struct
  include Nonmodal
  include Make_stringable (Nonmodal)
end

module Make_total_lattice (Input : sig
    type t

    val all : t list
  end) : Total_lattice with type t = Input.t = struct
  include Input

  let compare : t -> t -> int = Stdlib.compare
  let join : t -> t -> t = Stdlib.max
  let meet : t -> t -> t = Stdlib.min

  let reduce op ts =
    match ts with
    | [] -> None
    | hd :: tl -> Some (List.fold_left tl ~init:hd ~f:op)
  ;;

  let join_list ts = reduce join ts
  let meet_list ts = reduce meet ts
  let top = join_list all |> Option.get
  let bottom = meet_list all |> Option.get
  let le t1 t2 = compare t1 t2 <= 0
  let equal t1 t2 = compare t1 t2 = 0
end

module Nullability = Make_total_lattice (struct
    type t = nullability

    let all = all_of_nullability
  end)

module Separability = Make_total_lattice (struct
    type t = separability

    let all = all_of_separability
  end)

module Scannable_axes = struct
  include Scannable_axes

  let top = { nullability = Nullability.top; separability = Separability.top }
  let bottom = { nullability = Nullability.bottom; separability = Separability.bottom }

  let join t1 t2 =
    { nullability = Nullability.join t1.nullability t2.nullability
    ; separability = Separability.join t1.separability t2.separability
    }
  ;;

  let meet t1 t2 =
    { nullability = Nullability.meet t1.nullability t2.nullability
    ; separability = Separability.meet t1.separability t2.separability
    }
  ;;

  let le
    { nullability = nullability1; separability = separability1 }
    { nullability = nullability2; separability = separability2 }
    =
    Nullability.le nullability1 nullability2
    && Separability.le separability1 separability2
  ;;

  let equal
    { nullability = nullability1; separability = separability1 }
    { nullability = nullability2; separability = separability2 }
    =
    Nullability.equal nullability1 nullability2
    && Separability.equal separability1 separability2
  ;;

  module Axis = struct
    type 'a t = 'a scannable_axis
    type packed = P : 'a t -> packed [@@unboxed]

    let all = [ P Nullability; P Separability ]

    let module_ : type a. a t -> (module Total_lattice with type t = a) = function
      | Nullability -> (module Nullability)
      | Separability -> (module Separability)
    ;;

    let to_scannable_axis : type a. a t -> a -> Scannable_axis.t =
      fun axis value ->
      match axis with
      | Nullability -> Scannable_axis.Nullability value
      | Separability -> Scannable_axis.Separability value
    ;;

    let of_scannable_axis : type a. a t -> Scannable_axis.t -> a option =
      fun axis value ->
      match axis, value with
      | Nullability, Nullability n -> Some n
      | Separability, Separability s -> Some s
      | Nullability, Separability _ | Separability, Nullability _ -> None
    ;;
  end

  let of_scannable_axes values =
    let axis (type a) (axis : a Axis.t) : a =
      let module M = (val Axis.module_ axis) in
      match List.filter_map values ~f:(Axis.of_scannable_axis axis) with
      | [] -> M.top
      | [ value ] -> value
      | _ :: _ :: _ ->
        failwith
          "[Modes_lib.Scannable_axes.of_scannable_axes]: multiple inputs from same axis"
    in
    { nullability = axis Nullability; separability = axis Separability }
  ;;

  let get : type a. t -> a Axis.t -> a =
    fun t axis ->
    match axis with
    | Nullability -> t.nullability
    | Separability -> t.separability
  ;;

  let set : type a. t -> a Axis.t -> a -> t =
    fun t axis value ->
    match axis with
    | Nullability -> { t with nullability = value }
    | Separability -> { t with separability = value }
  ;;
end

module Scannable_axis = struct
  module T = struct
    include Scannable_axis

    let t_of_sexp sexp =
      try Nullability (nullability_of_sexp sexp) with
      | _ -> Separability (separability_of_sexp sexp)
    ;;

    let sexp_of_t = function
      | Nullability n -> sexp_of_nullability n
      | Separability s -> sexp_of_separability s
    ;;

    let non_redundant t =
      match t with
      | Nullability n -> if Nullability.equal n Nullability.top then [] else [ t ]
      | Separability s -> if Separability.equal s Separability.top then [] else [ t ]
    ;;
  end

  include T
  include Make_stringable (T)
end

module Jkind_mod = struct
  module T = struct
    include Jkind_mod

    let t_of_sexp sexp =
      try Modal (Modal.t_of_sexp sexp) with
      | _ ->
        (try Nonmodal (Nonmodal.t_of_sexp sexp) with
         | _ -> Scannable_axis (Scannable_axis.t_of_sexp sexp))
    ;;

    let sexp_of_t = function
      | Modal x -> Modal.sexp_of_t x
      | Nonmodal x -> Nonmodal.sexp_of_t x
      | Scannable_axis x -> Scannable_axis.sexp_of_t x
    ;;
  end

  include T
  include Make_stringable (T)
end

module Layout = struct
  include Layout

  let apply_scannable_axes t axes : t =
    match t with
    | Scannable axes' -> Scannable (Scannable_axes.meet axes' axes)
    | Any axes' -> Any (Scannable_axes.meet axes' axes)
    | Void
    | Untagged_immediate
    | Float64
    | Float32
    | Word
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512 -> t
  ;;

  let apply_scannable_axis t axis =
    apply_scannable_axes t (Scannable_axes.of_scannable_axes [ axis ])
  ;;

  let le t1 t2 =
    match t2 with
    | Any sa2 ->
      (match t1 with
       | Scannable sa1 | Any sa1 -> Scannable_axes.le sa1 sa2
       | _ -> true)
    | Scannable sa2 ->
      (match t1 with
       | Scannable sa1 -> Scannable_axes.le sa1 sa2
       | _ -> false)
    | Void
    | Untagged_immediate
    | Float64
    | Float32
    | Word
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512 -> Stdlib.( = ) t1 t2
  ;;

  let equal t1 t2 = le t1 t2 && le t2 t1
  let compare (t1 : t) t2 = Stdlib.compare t1 t2

  let to_string : t -> string = function
    | ( Void
      | Untagged_immediate
      | Float64
      | Float32
      | Word
      | Bits8
      | Bits16
      | Bits32
      | Bits64
      | Vec128
      | Vec256
      | Vec512 ) as t ->
      t |> sexp_of_t |> Sexplib0.Sexp.to_string |> String.lowercase_ascii
    | Any { nullability; separability } ->
      [ "any" ]
      @ (Scannable_axis.non_redundant (Separability separability)
         |> List.map ~f:Scannable_axis.to_string)
      @ (Scannable_axis.non_redundant (Nullability nullability)
         |> List.map ~f:Scannable_axis.to_string)
      |> String.concat ~sep:" "
    (* Scannable layouts are special-cased to print in terms of [value] *)
    | Scannable { nullability = Non_null; separability = Non_pointer } ->
      "value non_pointer"
    | Scannable { nullability = Non_null; separability = Non_pointer64 } ->
      "value non_pointer64"
    | Scannable { nullability = Non_null; separability = Non_float } -> "value non_float"
    | Scannable { nullability = Non_null; separability = Separable } -> "value"
    | Scannable { nullability = Non_null; separability = Maybe_separable } ->
      "value_maybe_separable"
    | Scannable { nullability = Maybe_null; separability = Non_pointer } ->
      "value_or_null non_pointer"
    | Scannable { nullability = Maybe_null; separability = Non_pointer64 } ->
      "value_or_null non_pointer64"
    | Scannable { nullability = Maybe_null; separability = Non_float } ->
      "value_or_null non_float"
    | Scannable { nullability = Maybe_null; separability = Separable } ->
      "value_maybe_null"
    | Scannable { nullability = Maybe_null; separability = Maybe_separable } ->
      "value_or_null"
  ;;

  let abbreviations : (string * t) list =
    List.filter_map all ~f:(fun layout ->
      let str = to_string layout in
      if not (String.contains str ' ') then Some (str, layout) else None)
  ;;

  let all_abbreviations = List.map abbreviations ~f:snd

  let of_string string =
    match
      String.split_on_char ~sep:' ' string
      |> List.filter ~f:(fun s -> String.length s > 0)
    with
    | [] -> failwith ("[Modes_lib.Layout.of_string]: invalid layout " ^ string)
    | name :: axes ->
      (match List.assoc_opt (String.lowercase_ascii name) abbreviations with
       | None -> failwith ("[Modes_lib.Layout.of_string]: invalid layout " ^ string)
       | Some t ->
         List.fold_left axes ~init:t ~f:(fun t axis ->
           apply_scannable_axis t (Scannable_axis.of_string axis)))
  ;;
end

module type Axis =
  Axis
  with type 'a mode := 'a Mode.t
   and type 'a modality := 'a Modality.t
   and type 'a crossing := 'a Crossing.t

module Make_axis (Input : sig
    type t

    val all : t list
    val monadicity : monadicity
    val legacy : t
    val of_modal : Modal.t -> t option
  end) : Axis with type t = Input.t = struct
  include Input

  let compare : t -> t -> int = Stdlib.compare
  let join : t -> t -> t = Stdlib.max
  let meet : t -> t -> t = Stdlib.min

  let reduce ts ~f =
    match ts with
    | [] -> None
    | hd :: tl -> Some (List.fold_left tl ~init:hd ~f)
  ;;

  let join_list ts = reduce ~f:join ts
  let meet_list ts = reduce ~f:meet ts
  let top = join_list all |> Option.get
  let bottom = meet_list all |> Option.get
  let le t1 t2 = compare t1 t2 <= 0
  let equal t1 t2 = compare t1 t2 = 0

  let apply_modality (t1 : t Modality.t) (t2 : t Mode.t) =
    match monadicity with
    | Monadic -> Mode.mk (join (t1 :> t) (t2 :> t))
    | Comonadic -> Mode.mk (meet (t1 :> t) (t2 :> t))
  ;;

  let cross (cross : t Crossing.t) (mode : t Mode.t) =
    let cross = (cross :> t) in
    let mode = (mode :> t) in
    Mode.mk
      (match monadicity with
       | Comonadic -> meet cross mode
       | Monadic -> if le mode cross then bottom else mode)
  ;;

  let uncross (cross : t Crossing.t) (mode : t Mode.t) =
    let cross = (cross :> t) in
    let mode = (mode :> t) in
    Mode.mk
      (match monadicity with
       | Comonadic -> if le cross mode then top else mode
       | Monadic -> join cross mode)
  ;;

  let to_modal t =
    List.find Modal.all ~f:(fun modal ->
      match of_modal modal with
      | None -> false
      | Some t' -> equal t t')
  ;;

  let sexp_of_t t = Modal.sexp_of_t (to_modal t)
end

module Make_nonmodal_axis (Input : sig
    type t

    val all : t list
    val of_nonmodal : Nonmodal.t -> t option
  end) : Nonmodal_axis with type t = Input.t = struct
  include Make_total_lattice (Input)

  let of_nonmodal = Input.of_nonmodal

  let to_nonmodal t =
    List.find Nonmodal.all ~f:(fun nonmodal ->
      match of_nonmodal nonmodal with
      | None -> false
      | Some t' -> equal t t')
  ;;
end

module Locality = Make_axis (struct
    type t = locality

    let all = [ Global; Local ]
    let monadicity = Comonadic
    let legacy = Global

    let of_modal : Modal.t -> t option = function
      | Global -> Some Global
      | Local -> Some Local
      | _ -> None
    ;;
  end)

module Portability = Make_axis (struct
    type t = portability

    let all = [ Portable; Shareable; Nonportable ]
    let monadicity = Comonadic
    let legacy = Nonportable

    let of_modal : Modal.t -> t option = function
      | Portable -> Some Portable
      | Shareable -> Some Shareable
      | Nonportable -> Some Nonportable
      | _ -> None
    ;;
  end)

module Contention = Make_axis (struct
    type t = contention

    let all = [ Uncontended; Shared; Contended ]
    let monadicity = Monadic
    let legacy = Uncontended

    let of_modal : Modal.t -> t option = function
      | Uncontended -> Some Uncontended
      | Shared -> Some Shared
      | Contended -> Some Contended
      | _ -> None
    ;;
  end)

module Statefulness = Make_axis (struct
    type t = statefulness

    let all = [ Stateless; Reading; Stateful ]
    let monadicity = Comonadic
    let legacy = Stateful

    let of_modal : Modal.t -> t option = function
      | Stateless -> Some Stateless
      | Reading -> Some Reading
      | Stateful -> Some Stateful
      | _ -> None
    ;;
  end)

module Visibility = Make_axis (struct
    type t = visibility

    let all = [ Read_write; Read; Immutable ]
    let monadicity = Monadic
    let legacy = Read_write

    let of_modal : Modal.t -> t option = function
      | Read_write -> Some Read_write
      | Read -> Some Read
      | Immutable -> Some Immutable
      | _ -> None
    ;;
  end)

module Linearity = Make_axis (struct
    type t = linearity

    let all = [ Many; Once ]
    let monadicity = Comonadic
    let legacy = Many

    let of_modal : Modal.t -> t option = function
      | Many -> Some Many
      | Once -> Some Once
      | _ -> None
    ;;
  end)

module Uniqueness = Make_axis (struct
    type t = uniqueness

    let all = [ Unique; Aliased ]
    let monadicity = Monadic
    let legacy = Aliased

    let of_modal : Modal.t -> t option = function
      | Unique -> Some Unique
      | Aliased -> Some Aliased
      | _ -> None
    ;;
  end)

module Yielding = Make_axis (struct
    type t = yielding

    let all = [ Unyielding; Yielding ]
    let monadicity = Comonadic
    let legacy = Unyielding

    let of_modal : Modal.t -> t option = function
      | Unyielding -> Some Unyielding
      | Yielding -> Some Yielding
      | _ -> None
    ;;
  end)

module Forkable = Make_axis (struct
    type t = forkable

    let all : t list = [ Forkable; Unforkable ]
    let monadicity = Comonadic
    let legacy : t = Forkable

    let of_modal : Modal.t -> t option = function
      | Forkable -> Some Forkable
      | Unforkable -> Some Unforkable
      | _ -> None
    ;;
  end)

module Staticity = Make_axis (struct
    type t = staticity

    let all : t list = [ Static; Dynamic ]
    let monadicity = Monadic
    let legacy : t = Dynamic

    let of_modal : Modal.t -> t option = function
      | Static -> Some Static
      | Dynamic -> Some Dynamic
      | _ -> None
    ;;
  end)

module Externality = Make_nonmodal_axis (struct
    type t = externality

    let all = [ External_; External64; Internal ]

    let of_nonmodal : Nonmodal.t -> t option = function
      | External_ -> Some External_
      | External64 -> Some External64
      | Internal -> Some Internal
    ;;
  end)

module Axis = struct
  type 'a t = 'a axis
  type packed = P : 'a t -> packed [@@unboxed]

  let equal_witness (type a b) (x : a t) (y : b t) : (a, b) Type.eq option =
    match x, y with
    | Locality, Locality -> Some Equal
    | Portability, Portability -> Some Equal
    | Contention, Contention -> Some Equal
    | Statefulness, Statefulness -> Some Equal
    | Visibility, Visibility -> Some Equal
    | Linearity, Linearity -> Some Equal
    | Uniqueness, Uniqueness -> Some Equal
    | Yielding, Yielding -> Some Equal
    | Forkable, Forkable -> Some Equal
    | Staticity, Staticity -> Some Equal
    | ( ( Locality
        | Portability
        | Contention
        | Statefulness
        | Visibility
        | Linearity
        | Uniqueness
        | Yielding
        | Forkable
        | Staticity )
      , _ ) -> None
  ;;

  (* Exhaustiveness tested in modes_lib_test.ml *)
  let all =
    [ P Locality
    ; P Portability
    ; P Contention
    ; P Statefulness
    ; P Visibility
    ; P Linearity
    ; P Uniqueness
    ; P Yielding
    ; P Forkable
    ; P Staticity
    ]
  ;;

  let module_ : type a. a t -> (module Axis with type t = a) = function
    | Locality -> (module Locality)
    | Portability -> (module Portability)
    | Contention -> (module Contention)
    | Statefulness -> (module Statefulness)
    | Visibility -> (module Visibility)
    | Linearity -> (module Linearity)
    | Uniqueness -> (module Uniqueness)
    | Yielding -> (module Yielding)
    | Forkable -> (module Forkable)
    | Staticity -> (module Staticity)
  ;;
end

module Nonmodal_axis = struct
  type 'a t = 'a nonmodal_axis
  type packed = P : 'a t -> packed [@@unboxed]

  let all = [ P Externality ]

  let module_ : type a. a t -> (module Nonmodal_axis with type t = a) = function
    | Externality -> (module Externality)
  ;;
end

module Jkind_axis = struct
  type 'a t = 'a jkind_axis
  type packed = P : 'a t -> packed [@@unboxed]

  let all =
    List.map Axis.all ~f:(fun (P axis : Axis.packed) -> P (Modal axis))
    @ List.map Nonmodal_axis.all ~f:(fun (P axis : Nonmodal_axis.packed) ->
      P (Nonmodal axis))
    @ List.map Scannable_axes.Axis.all ~f:(fun (P axis : Scannable_axes.Axis.packed) ->
      P (Scannable axis))
  ;;
end

module Lift (T : sig
    type 'a t

    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end) =
struct
  type t =
    { locality : locality T.t
    ; portability : portability T.t
    ; contention : contention T.t
    ; statefulness : statefulness T.t
    ; visibility : visibility T.t
    ; linearity : linearity T.t
    ; uniqueness : uniqueness T.t
    ; yielding : yielding T.t
    ; forkable : forkable T.t
    ; staticity : staticity T.t
    }
  [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun { locality = locality__002_
         ; portability = portability__004_
         ; contention = contention__006_
         ; statefulness = statefulness__008_
         ; visibility = visibility__010_
         ; linearity = linearity__012_
         ; uniqueness = uniqueness__014_
         ; yielding = yielding__016_
         ; forkable = forkable__018_
         ; staticity = staticity__020_
         } ->
       let bnds__001_ = ([] : _ Stdlib.List.t) in
       let bnds__001_ =
         let arg__021_ = T.sexp_of_t sexp_of_staticity staticity__020_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "staticity"; arg__021_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__019_ = T.sexp_of_t sexp_of_forkable forkable__018_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "forkable"; arg__019_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__017_ = T.sexp_of_t sexp_of_yielding yielding__016_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "yielding"; arg__017_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__015_ = T.sexp_of_t sexp_of_uniqueness uniqueness__014_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "uniqueness"; arg__015_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__013_ = T.sexp_of_t sexp_of_linearity linearity__012_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "linearity"; arg__013_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__011_ = T.sexp_of_t sexp_of_visibility visibility__010_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "visibility"; arg__011_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__009_ = T.sexp_of_t sexp_of_statefulness statefulness__008_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "statefulness"; arg__009_ ]
          :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__007_ = T.sexp_of_t sexp_of_contention contention__006_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "contention"; arg__007_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__005_ = T.sexp_of_t sexp_of_portability portability__004_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "portability"; arg__005_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       let bnds__001_ =
         let arg__003_ = T.sexp_of_t sexp_of_locality locality__002_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "locality"; arg__003_ ] :: bnds__001_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__001_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]

  type make_f = { f : 'a. 'a Axis.t -> 'a T.t }

  let make ({ f } : make_f) =
    { locality = f Locality
    ; portability = f Portability
    ; contention = f Contention
    ; statefulness = f Statefulness
    ; visibility = f Visibility
    ; linearity = f Linearity
    ; uniqueness = f Uniqueness
    ; yielding = f Yielding
    ; forkable = f Forkable
    ; staticity = f Staticity
    }
  ;;

  let get : type a. t -> a Axis.t -> a T.t =
    fun t axis ->
    match axis with
    | Locality -> t.locality
    | Portability -> t.portability
    | Contention -> t.contention
    | Statefulness -> t.statefulness
    | Visibility -> t.visibility
    | Linearity -> t.linearity
    | Uniqueness -> t.uniqueness
    | Yielding -> t.yielding
    | Forkable -> t.forkable
    | Staticity -> t.staticity
  ;;

  let set : type a. t -> a Axis.t -> a T.t -> t =
    fun t axis x ->
    match axis with
    | Locality -> { t with locality = x }
    | Portability -> { t with portability = x }
    | Contention -> { t with contention = x }
    | Statefulness -> { t with statefulness = x }
    | Visibility -> { t with visibility = x }
    | Linearity -> { t with linearity = x }
    | Uniqueness -> { t with uniqueness = x }
    | Yielding -> { t with yielding = x }
    | Forkable -> { t with forkable = x }
    | Staticity -> { t with staticity = x }
  ;;
end

module Lift_lattice (T : sig
    include Wrapper

    val monadic_op : bool
  end) =
struct
  include Lift (T)

  let flip (type a) (axis : a Axis.t) =
    T.monadic_op
    && let module M = (val Axis.module_ axis) in
       match M.monadicity with
       | Monadic -> true
       | Comonadic -> false
  ;;

  let top =
    make
      { f =
          (fun (type a) (axis : a Axis.t) ->
            let module M = (val Axis.module_ axis) in
            T.mk (if flip axis then M.bottom else M.top))
      }
  ;;

  let bottom =
    make
      { f =
          (fun (type a) (axis : a Axis.t) ->
            let module M = (val Axis.module_ axis) in
            T.mk (if flip axis then M.top else M.bottom))
      }
  ;;

  let join t1 t2 =
    make
      { f =
          (fun (type a) (axis : a Axis.t) ->
            let module M = (val Axis.module_ axis) in
            T.mk
              ((if flip axis then M.meet else M.join)
                 (get t1 axis :> a)
                 (get t2 axis :> a)))
      }
  ;;

  let meet t1 t2 =
    make
      { f =
          (fun (type a) (axis : a Axis.t) ->
            let module M = (val Axis.module_ axis) in
            T.mk
              ((if flip axis then M.join else M.meet)
                 (get t1 axis :> a)
                 (get t2 axis :> a)))
      }
  ;;

  let le t1 t2 =
    List.for_all Axis.all ~f:(fun (Axis.P (type a) (axis : a Axis.t)) ->
      let module M = (val Axis.module_ axis) in
      if flip axis
      then M.le (get t2 axis :> a) (get t1 axis :> a)
      else M.le (get t1 axis :> a) (get t2 axis :> a))
  ;;

  let equal t1 t2 =
    List.for_all Axis.all ~f:(fun (Axis.P (type a) (axis : a Axis.t)) ->
      let module M = (val Axis.module_ axis) in
      M.equal (get t1 axis :> a) (get t2 axis :> a))
  ;;

  let to_modals_explicit t =
    List.map Axis.all ~f:(fun (Axis.P axis) ->
      let module M = (val Axis.module_ axis) in
      get t axis |> T.unwrap |> M.to_modal)
  ;;
end

module Explicit = struct
  include Lift (struct
      type 'a t = 'a

      let sexp_of_t sexp_of_a a = sexp_of_a a
    end)
end

module Optional = struct
  include Lift (struct
      include Option

      let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_option
    end)

  let of_modals modals =
    make
      { f =
          (fun (type a) (axis : a Axis.t) ->
            let module M = (val Axis.module_ axis) in
            match List.filter_map modals ~f:M.of_modal with
            | [] -> None
            | [ modal ] -> Some modal
            | _ :: _ :: _ ->
              failwith "[Modes_lib.Modalities.of_modals]: multiple inputs from same axis")
      }
  ;;

  let to_modals t =
    List.filter_map Axis.all ~f:(fun (Axis.P axis) ->
      let module Axis = (val Axis.module_ axis) in
      Option.map Axis.to_modal (get t axis))
  ;;

  let get_implied_or_default t (type a) (axis : a axis) ~default ~universe =
    match get t axis with
    | Some modal -> modal
    | None ->
      List.find_map
        implications
        ~f:(fun (I (universes, domain, codomain, imply)) : a option ->
          match List.mem universe ~set:universes with
          | false -> None
          | true ->
            (match Axis.equal_witness axis codomain with
             | None -> None
             | Some Equal ->
               (match get t domain with
                | None -> None
                | Some preimage -> imply preimage)))
      |> Option.value ~default
  ;;

  type default = { default_without_implies : 'a. 'a axis -> 'a }

  let of_explicit explicit ~default:{ default_without_implies } ~universe =
    let redundant (type a) (axis : a axis) =
      let module Axis' = (val Axis.module_ axis) in
      let explicit_modal = Explicit.get explicit axis in
      match
        List.find_map implications ~f:(fun (I (universes, domain, codomain, imply)) ->
          match List.mem universe ~set:universes with
          | false -> None
          | true ->
            (match Axis.equal_witness axis codomain with
             | None -> None
             | Some Equal ->
               let preimage = Explicit.get explicit domain in
               (match imply preimage with
                | None -> None
                | Some implied -> Some (Axis'.equal explicit_modal implied))))
      with
      | Some redundant -> redundant
      | None -> Axis'.equal explicit_modal (default_without_implies axis)
    in
    make
      { f =
          (fun axis -> if redundant axis then None else Some (Explicit.get explicit axis))
      }
  ;;
end

module Modes = struct
  include Lift_lattice (struct
      include Mode

      let monadic_op = false
    end)

  let universe = Modes

  let default_without_implies (type a) (axis : a axis) =
    let module Axis = (val Axis.module_ axis) in
    Axis.legacy
  ;;

  let of_modals modals =
    let optional = Optional.of_modals modals in
    make
      { f =
          (fun (type a) (axis : a Axis.t) ->
            let default = default_without_implies axis in
            Optional.get_implied_or_default optional axis ~default ~universe |> Mode.mk)
      }
  ;;

  let to_modals t =
    Explicit.make { f = (fun axis -> Mode.unwrap (get t axis)) }
    |> Optional.of_explicit ~default:{ default_without_implies } ~universe
    |> Optional.to_modals
  ;;
end

module Modalities = struct
  include Lift_lattice (struct
      include Modality

      let monadic_op = false
    end)

  let universe = Modalities

  let default_without_implies (type a) (axis : a axis) ~mutable_implied =
    let module Axis = (val Axis.module_ axis) in
    if mutable_implied
    then Axis.legacy
    else (
      match Axis.monadicity with
      | Comonadic -> Axis.top
      | Monadic -> Axis.bottom)
  ;;

  let of_modals ~mutable_implied modals =
    let optional = Optional.of_modals modals in
    make
      { f =
          (fun (type a) (axis : a axis) ->
            let default = default_without_implies axis ~mutable_implied in
            Optional.get_implied_or_default optional axis ~default ~universe
            |> Modality.mk)
      }
  ;;

  let to_modals ~mutable_implied t =
    Explicit.make { f = (fun axis -> Modality.unwrap (get t axis)) }
    |> Optional.of_explicit
         ~default:
           { default_without_implies =
               (fun axis -> default_without_implies axis ~mutable_implied)
           }
         ~universe
    |> Optional.to_modals
  ;;
end

module Crossings = struct
  include Lift_lattice (struct
      include Crossing

      let monadic_op = true
    end)

  let universe = Crossings

  let default_without_implies (type a) (axis : a axis) =
    let module Axis = (val Axis.module_ axis) in
    match Axis.monadicity with
    | Comonadic -> Axis.top
    | Monadic -> Axis.bottom
  ;;

  let of_modals modals =
    let optional = Optional.of_modals modals in
    make
      { f =
          (fun (type a) (axis : a axis) ->
            let default = default_without_implies axis in
            Optional.get_implied_or_default optional axis ~default ~universe
            |> Crossing.mk)
      }
  ;;

  let to_modals t =
    Explicit.make { f = (fun axis -> Crossing.unwrap (get t axis)) }
    |> Optional.of_explicit
         ~default:{ default_without_implies = (fun axis -> default_without_implies axis) }
         ~universe
    |> Optional.to_modals
  ;;
end

module Nonmodals = struct
  include Nonmodals

  type make_f = { f : 'a. 'a nonmodal_axis -> 'a }

  let make { f } = { externality = f Externality }

  let get : type a. t -> a nonmodal_axis -> a =
    fun t axis ->
    match axis with
    | Externality -> t.externality
  ;;

  let set : type a. t -> a nonmodal_axis -> a -> t =
    fun { externality = _ } axis x ->
    match axis with
    | Externality -> { externality = x }
  ;;

  let of_nonmodals nonmodals =
    make
      { f =
          (fun (type a) (axis : a Nonmodal_axis.t) ->
            let module M = (val Nonmodal_axis.module_ axis) in
            match List.filter_map nonmodals ~f:M.of_nonmodal with
            | [] -> M.top
            | [ nonmodal ] -> nonmodal
            | _ :: _ :: _ ->
              failwith
                "[Modes_lib.Nonmodals.of_nonmodals]: multiple inputs from same axis")
      }
  ;;

  let to_nonmodals_explicit t =
    List.map Nonmodal_axis.all ~f:(fun (Nonmodal_axis.P axis) ->
      let module M = (val Nonmodal_axis.module_ axis) in
      get t axis |> M.to_nonmodal)
  ;;

  let to_nonmodals t =
    List.filter_map Nonmodal_axis.all ~f:(fun (Nonmodal_axis.P axis) ->
      let module M = (val Nonmodal_axis.module_ axis) in
      let val_ = get t axis in
      if M.equal val_ M.top then None else Some (M.to_nonmodal val_))
  ;;

  let top =
    make
      { f =
          (fun (type a) (axis : a Nonmodal_axis.t) ->
            let module M = (val Nonmodal_axis.module_ axis) in
            M.top)
      }
  ;;

  let bottom =
    make
      { f =
          (fun (type a) (axis : a Nonmodal_axis.t) ->
            let module M = (val Nonmodal_axis.module_ axis) in
            M.bottom)
      }
  ;;

  let join t1 t2 =
    make
      { f =
          (fun (type a) (axis : a Nonmodal_axis.t) ->
            let module M = (val Nonmodal_axis.module_ axis) in
            M.join (get t1 axis :> a) (get t2 axis :> a))
      }
  ;;

  let meet t1 t2 =
    make
      { f =
          (fun (type a) (axis : a Nonmodal_axis.t) ->
            let module M = (val Nonmodal_axis.module_ axis) in
            M.meet (get t1 axis :> a) (get t2 axis :> a))
      }
  ;;

  let le t1 t2 =
    List.for_all
      Nonmodal_axis.all
      ~f:(fun (Nonmodal_axis.P (type a) (axis : a Nonmodal_axis.t)) ->
        let module M = (val Nonmodal_axis.module_ axis) in
        M.le (get t1 axis :> a) (get t2 axis :> a))
  ;;

  let equal t1 t2 =
    List.for_all
      Nonmodal_axis.all
      ~f:(fun (Nonmodal_axis.P (type a) (axis : a Nonmodal_axis.t)) ->
        let module M = (val Nonmodal_axis.module_ axis) in
        M.equal (get t1 axis :> a) (get t2 axis :> a))
  ;;
end

module Jkind_modifiers = struct
  type 'a wrapper =
    | Modal : 'a Crossing.t * 'a Axis.t -> 'a wrapper
    | Nonmodal : 'a * 'a Nonmodal_axis.t -> 'a wrapper
    | Scannable : 'a * 'a Scannable_axes.Axis.t -> 'a wrapper

  type t =
    { modals : Crossings.t
    ; nonmodals : Nonmodals.t
    ; scannable_axes : Scannable_axes.t
    }

  type make_f = { f : 'a. 'a jkind_axis -> 'a wrapper }

  let make { f } =
    { modals =
        Crossings.make
          { f =
              (fun (type a) (axis : a Axis.t) ->
                match axis, f (Modal axis) with
                | _, Modal (m, _) -> m
                | _, (Nonmodal _ | Scannable _) -> .)
          }
    ; nonmodals =
        Nonmodals.make
          { f =
              (fun (type a) (axis : a nonmodal_axis) ->
                match axis, f (Nonmodal axis) with
                | _, Nonmodal (m, _) -> m
                | _, (Modal _ | Scannable _) -> .)
          }
    ; scannable_axes =
        (let scannable (type a) (axis : a scannable_axis) : a =
           match axis, f (Scannable axis) with
           | _, Scannable (m, _) -> m
           | _, (Modal _ | Nonmodal _) -> .
         in
         { nullability = scannable Nullability; separability = scannable Separability })
    }
  ;;

  let get : type a. t -> a jkind_axis -> a =
    fun t axis ->
    match axis with
    | Modal axis -> Crossings.get t.modals axis |> Crossing.unwrap
    | Nonmodal axis -> Nonmodals.get t.nonmodals axis
    | Scannable axis -> Scannable_axes.get t.scannable_axes axis
  ;;

  let set : type a. t -> a wrapper -> t =
    fun t x ->
    match x with
    | Modal (x, axis) -> { t with modals = Crossings.set t.modals axis x }
    | Nonmodal (x, axis) -> { t with nonmodals = Nonmodals.set t.nonmodals axis x }
    | Scannable (x, axis) ->
      { t with scannable_axes = Scannable_axes.set t.scannable_axes axis x }
  ;;

  let of_jkind_mods jkind_mods =
    let filter (f : Jkind_mod.t -> _ option) = List.filter_map jkind_mods ~f in
    { modals =
        Crossings.of_modals
          (filter (function
            | Modal m -> Some m
            | Nonmodal _ | Scannable_axis _ -> None))
    ; nonmodals =
        Nonmodals.of_nonmodals
          (filter (function
            | Nonmodal n -> Some n
            | Modal _ | Scannable_axis _ -> None))
    ; scannable_axes =
        Scannable_axes.of_scannable_axes
          (filter (function
            | Scannable_axis s -> Some s
            | Modal _ | Nonmodal _ -> None))
    }
  ;;

  let to_jkind_mods t =
    let modals = Crossings.to_modals t.modals in
    let nonmodals = Nonmodals.to_nonmodals t.nonmodals in
    List.map modals ~f:(fun m : Jkind_mod.t -> Modal m)
    @ List.map nonmodals ~f:(fun m : Jkind_mod.t -> Nonmodal m)
    @ (Scannable_axis.non_redundant (Nullability t.scannable_axes.nullability)
       |> List.map ~f:(fun s : Jkind_mod.t -> Scannable_axis s))
    @ (Scannable_axis.non_redundant (Separability t.scannable_axes.separability)
       |> List.map ~f:(fun s : Jkind_mod.t -> Scannable_axis s))
  ;;

  let to_jkind_mods_explicit t =
    let modals = Crossings.to_modals_explicit t.modals in
    let nonmodals = Nonmodals.to_nonmodals_explicit t.nonmodals in
    List.map modals ~f:(fun m : Jkind_mod.t -> Modal m)
    @ List.map nonmodals ~f:(fun m : Jkind_mod.t -> Nonmodal m)
    @ [ (Scannable_axis (Nullability t.scannable_axes.nullability) : Jkind_mod.t)
      ; Scannable_axis (Separability t.scannable_axes.separability)
      ]
  ;;
end

module Kind = struct
  type t =
    { layout : Layout.t
    ; crossings : Crossings.t
    ; nonmodals : Nonmodals.t
    }

  let apply_mods t (per_jkind_axis : Jkind_modifiers.t) =
    { layout = Layout.apply_scannable_axes t.layout per_jkind_axis.scannable_axes
    ; crossings = Crossings.meet t.crossings per_jkind_axis.modals
    ; nonmodals = Nonmodals.meet t.nonmodals per_jkind_axis.nonmodals
    }
  ;;

  let apply_scannable_axes t axes =
    { t with layout = Layout.apply_scannable_axes t.layout axes }
  ;;

  let modifiers { layout; crossings; nonmodals } : Jkind_modifiers.t =
    let scannable_axes =
      match (layout : Layout.t) with
      | Scannable axes | Any axes -> axes
      | Void
      | Untagged_immediate
      | Float64
      | Float32
      | Word
      | Bits8
      | Bits16
      | Bits32
      | Bits64
      | Vec128
      | Vec256
      | Vec512 -> Scannable_axes.bottom
    in
    { modals = crossings; nonmodals; scannable_axes }
  ;;

  let interpreted_abbreviations : (string * (Layout.t * Jkind_mod.t list)) list =
    let modals = List.map ~f:(fun m : Jkind_mod.t -> Modal m) in
    let nonmodals = List.map ~f:(fun j : Jkind_mod.t -> Nonmodal j) in
    let scannable = List.map ~f:(fun s : Jkind_mod.t -> Scannable_axis s) in
    let immediate_crossings =
      Crossings.set Crossings.bottom Staticity (Crossings.get Crossings.top Staticity)
    in
    let value_or_null : Layout.t = Scannable Scannable_axes.top in
    [ ( "immutable_data"
      , ( value_or_null
        , modals [ Many; Portable; Forkable; Unyielding; Contended; Stateless; Immutable ]
          @ scannable [ Nullability Non_null; Separability Non_float ] ) )
    ; ( "sync_data"
      , ( value_or_null
        , modals [ Many; Portable; Forkable; Unyielding; Contended; Stateless ]
          @ scannable [ Nullability Non_null; Separability Non_float ] ) )
    ; ( "mutable_data"
      , ( value_or_null
        , modals [ Many; Portable; Forkable; Unyielding; Stateless ]
          @ scannable [ Nullability Non_null; Separability Non_float ] ) )
    ; ( "immediate"
      , ( value_or_null
        , modals (Crossings.to_modals immediate_crossings)
          @ nonmodals (Nonmodals.to_nonmodals Nonmodals.bottom)
          @ scannable [ Nullability Non_null; Separability Non_pointer ] ) )
    ; ( "immediate_or_null"
      , ( value_or_null
        , modals (Crossings.to_modals immediate_crossings)
          @ nonmodals [ External_ ]
          @ scannable [ Separability Non_pointer ] ) )
    ; ( "immediate64"
      , ( value_or_null
        , modals (Crossings.to_modals immediate_crossings)
          @ nonmodals [ External64 ]
          @ scannable [ Nullability Non_null; Separability Non_pointer64 ] ) )
    ; ( "immediate64_or_null"
      , ( value_or_null
        , modals (Crossings.to_modals immediate_crossings)
          @ nonmodals [ External64 ]
          @ scannable [ Separability Non_pointer64 ] ) )
    ]
  ;;

  let of_ident_exn ~ident =
    List.find_opt interpreted_abbreviations ~f:(fun (abbrev, _) ->
      String.equal abbrev ident)
    |> (function
          | Some (_, (layout, mod_)) -> layout, mod_
          | None -> Layout.of_string ident, [])
    |> fun (layout, mod_) ->
    apply_mods
      { layout; crossings = Crossings.top; nonmodals = Nonmodals.top }
      (Jkind_modifiers.of_jkind_mods mod_)
  ;;

  let is_subkind
    { layout = layout1; crossings = crossings1; nonmodals = nonmodals1 }
    ~of_:{ layout = layout2; crossings = crossings2; nonmodals = nonmodals2 }
    =
    Layout.le layout1 layout2
    && Crossings.le crossings1 crossings2
    && Nonmodals.le nonmodals1 nonmodals2
  ;;

  module Private = struct
    let interpreted_abbreviations = List.map interpreted_abbreviations ~f:fst
  end
end

let apply_modalities modalities modes =
  Modes.make
    { f =
        (fun (type a) (axis : a Axis.t) ->
          let module M = (val Axis.module_ axis) in
          M.apply_modality (Modalities.get modalities axis) (Modes.get modes axis))
    }
;;

let cross crossings modes =
  Modes.make
    { f =
        (fun (type a) (axis : a Axis.t) ->
          let module M = (val Axis.module_ axis) in
          M.cross (Crossings.get crossings axis) (Modes.get modes axis))
    }
;;

let uncross crossings modes =
  Modes.make
    { f =
        (fun (type a) (axis : a Axis.t) ->
          let module M = (val Axis.module_ axis) in
          M.uncross (Crossings.get crossings axis) (Modes.get modes axis))
    }
;;
