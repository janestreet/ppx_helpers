open Base_lite_for_ppxes_intf.Definitions
include Stdppx

module Sexp = struct
  include Sexplib0.Sexp
end

(* Note on testing: Most of the code below is either directly copied from [Base], or is
   obviously correct. Functions with non-trivial implementations will have a note saying
   whether they are directly copied, or if they are tested. *)

exception Exn of Sexp.t

let () =
  Basement.Stdlib_shim.Printexc.Safe.register_printer (function
    | Exn sexp -> Some (Sexp.to_string_hum sexp)
    | _ -> None)
;;

let force = Lazy.force
let raise_s sexp = raise (Exn sexp)
let phys_equal = ( == )

(* deriver functions for primitives *)

let compare_int = Int.compare
let compare_string = Stdlib.String.compare
let all_of_option all_of_a = None :: List.map all_of_a ~f:(fun a -> Some a)
let all_of_unit = [ () ]
let hash_fold_int = Base_internalhash_types.fold_int

module Printf = struct
  include Printf

  let[@inline never] failwithf fmt = Printf.ksprintf (fun s () -> failwith s) fmt
  let[@inline never] invalid_argf fmt = Printf.ksprintf (fun s () -> invalid_arg s) fmt
end

module Map = struct
  module type S = Map

  module Make (Key : Map.OrderedType) : Map with type key := Key.t = struct
    module M = Map.Make (Key)

    type 'a t = 'a M.t

    let find t key = M.find_opt key t
    let find_exn t key = M.find key t
    let mem t key = M.mem key t
    let set t ~key ~data = M.add key data t

    let add_exn t ~key ~data =
      if mem t key then failwith "Map.add_exn: duplicate key" else set t ~key ~data
    ;;

    let change t key ~f = M.update key f t
    let update t key ~f = M.update key (fun data -> Some (f data)) t
    let to_alist = M.to_list

    (* tested *)
    let of_alist list =
      List.fold_left list ~init:(`Ok M.empty) ~f:(fun acc (key, data) ->
        match acc with
        | `Duplicate_key _ -> acc
        | `Ok acc ->
          if M.mem key acc then `Duplicate_key key else `Ok (M.add key data acc))
    ;;

    let of_alist_exn list =
      match of_alist list with
      | `Ok t -> t
      | `Duplicate_key _key -> failwith "Map.of_alist_exn: duplicate key"
    ;;

    (* tested *)
    let of_alist_multi list =
      List.fold_right list ~init:M.empty ~f:(fun (key, data) acc ->
        M.add_to_list key data acc)
    ;;

    let of_alist_reduce list ~f =
      List.fold_left list ~init:M.empty ~f:(fun map (key, data) ->
        M.update
          key
          (function
            | None -> Some data
            | Some previous -> Some (f previous data))
          map)
    ;;

    let keys t = to_alist t |> List.map ~f:fst
    let data t = to_alist t |> List.map ~f:snd
    let empty = M.empty
    let singleton key data = M.of_list [ key, data ]

    let fold t ~init ~f =
      to_alist t |> List.fold_left ~init ~f:(fun acc (key, data) -> f ~key ~data acc)
    ;;

    let map t ~f = M.map f t
    let mapi t ~f = M.mapi (fun key data -> f ~key ~data) t

    let merge_disjoint_exn t1 t2 =
      fold t2 ~init:t1 ~f:(fun ~key ~data acc -> add_exn acc ~key ~data)
    ;;

    let merge_skewed t1 t2 ~combine =
      M.union (fun key left right -> Some (combine ~key left right)) t1 t2
    ;;
  end
end

module Set = struct
  module type S = Set

  module Make (Elt : Set.OrderedType) : Set with type elt := Elt.t = struct
    module S = Set.Make (Elt)

    type t = S.t

    let add t elt = S.add elt t
    let mem t elt = S.mem elt t
    let empty = S.empty
    let is_empty = S.is_empty
    let to_list = S.to_list
    let of_list = S.of_list
    let singleton = S.singleton
    let inter = S.inter
    let diff = S.diff
    let union = S.union
    let union_list ts = List.fold_left ts ~init:empty ~f:union
    let fold t ~init ~f = to_list t |> List.fold_left ~init ~f
    let fold_right t ~init ~f = to_list t |> List.fold_right ~init ~f
  end
end

module Array = struct
  include Array

  let create ~len a = make len a

  (* directly copied from [Base.Array], with template annotations removed *)
  let of_list_map xs ~f =
    match xs with
    | [] -> [||]
    | hd :: tl ->
      let a = create ~len:(1 + List.length tl) (f hd) in
      let rec fill i = function
        | [] -> a
        | hd :: tl ->
          unsafe_set a i (f hd);
          fill (i + 1) tl
      in
      fill 1 tl [@nontail]
  ;;
end

module Bool = struct
  module T = struct
    include Bool

    type t = bool
  end

  include T
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_bool
  let t_of_sexp = Sexplib0.Sexp_conv.bool_of_sexp
  let all = [ false; true ]
  let to_int t = if t then 1 else 0
end

module Char = struct
  include Char

  let to_string t = String.make 1 t
  let lowercase = Stdlib.Char.lowercase_ascii
  let uppercase = Stdlib.Char.uppercase_ascii

  let is_lowercase = function
    | 'a' .. 'z' -> true
    | _ -> false
  ;;

  let is_uppercase = function
    | 'A' .. 'Z' -> true
    | _ -> false
  ;;

  (* directly copied from [Base.Char] *)
  let is_whitespace = function
    | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' ->
      true
    | _ -> false
  ;;
end

module Comparable = struct
  module type S = Comparable

  let lift cmp ~f a b = cmp (f a) (f b)

  (* directly copied from [Base.Comparable], with template annotations removed *)
  let rec lexicographic cmps x y =
    match cmps with
    | cmp :: cmps ->
      let res = cmp x y in
      if res = 0 then lexicographic cmps x y else res
    | [] -> 0
  ;;
end

module Container = struct
  module type Summable = Summable
end

module Dynamic = struct
  include Basement.Dynamic
end

module Error = struct
  type t =
    | String of string
    | Sexp of Sexp.t

  let of_string string = String string
  let of_sexp sexp = Sexp sexp

  let sexp_of_t = function
    | String string -> Sexp.Atom string
    | Sexp sexp -> sexp
  ;;

  let of_list = function
    | [ t ] -> t
    | ts -> Sexp (Sexp.List (List.map ts ~f:sexp_of_t))
  ;;

  let createf format = Printf.ksprintf of_string format

  let to_string_hum = function
    | String string -> string
    | Sexp sexp -> Sexp.to_string_hum sexp
  ;;

  let to_string_mach t = Sexp.to_string_mach (sexp_of_t t)
end

module Or_error = struct
  type 'a t = ('a, Error.t) result

  let error_string string = Error (Error.of_string string)
  let errorf format = Printf.ksprintf error_string format

  let map t ~f =
    match t with
    | Ok ok -> Ok (f ok)
    | Error _ as error -> error
  ;;

  let combine_errors ts =
    match
      List.filter_map ts ~f:(function
        | Ok _ -> None
        | Error error -> Some error)
    with
    | [] ->
      Ok
        (List.map ts ~f:(function
          | Ok ok -> ok
          | Error _ -> assert false))
    | errors -> Error (Error.of_list errors)
  ;;
end

module Exn = struct
  include Exn

  type t = exn

  external clear_backtrace : unit -> unit = "Base_lite_clear_caml_backtrace_pos"

  (* directly copied from [Base.Exn] *)
  let raise_without_backtrace e =
    clear_backtrace ();
    Stdlib.raise_notrace e
  ;;

  (* directly copied from [Base.Exn] *)
  let to_string t = Sexp.to_string_hum (Sexplib0.Sexp_conv.sexp_of_exn t)
end

module Fn = struct
  include Fun

  let flip f x y = f y x
end

module Hash = struct
  include Base_internalhash_types

  let create () = create_seeded 0
end

module Hashable = struct
  module type Key = Hashable
end

module Hashtbl = struct
  module type S = Hashtbl

  module Make (Key : Hashtbl.HashedType) = struct
    module H = Hashtbl.Make (Key)

    type 'a t = 'a H.t

    let create () = H.create 0
    let clear = H.clear
    let length = H.length
    let data t = H.fold (fun _ data acc -> data :: acc) t []
    let find = H.find_opt
    let to_alist t = H.fold (fun key data alist -> (key, data) :: alist) t []

    let add_exn t ~key ~data =
      if H.mem t key then failwith "add_exn: duplicate key" else H.add t key data
    ;;

    let find_or_add t key ~default =
      match H.find_opt t key with
      | Some data -> data
      | None ->
        let data = default () in
        add_exn t ~key ~data;
        data
    ;;
  end
end

module Hash_set = struct
  module type S = Hash_set

  module Make (Elt : Stdlib.Hashtbl.HashedType) = struct
    module H = Stdlib.Hashtbl.Make (Elt)

    type t = unit H.t

    let create () = H.create 0
    let length = H.length
    let mem = H.mem
    let add t elt = H.replace t elt ()
    let to_list t = H.fold (fun elt () acc -> elt :: acc) t []
  end
end

module Iarray = struct
  include Basement.Stdlib_iarray_labels

  module O = struct
    external ( .:() )
      : 'a.
      ('a t[@local_opt]) -> int -> ('a[@local_opt])
      = "%array_safe_get"
    [@@layout_poly]
  end
end

include Iarray.O

module Identifiable = struct
  module type S = Identifiable
end

module Intable = struct
  module type S = Intable
end

module Int = struct
  module T = struct
    include Int

    type t = int
  end

  include T
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  module O = struct
    external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
    external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
    external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
    external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
    external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
    external ( + ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%addint"
  end

  include O

  let hash = Stdlib.Int.hash
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_int
  let t_of_sexp = Sexplib0.Sexp_conv.int_of_sexp
  let to_int_exn t = t
  let of_int_exn t = t
  let zero = 0

  external of_string : string -> int = "caml_int_of_string"

  let incr r = r := !r + 1
  let decr r = r := !r - 1
end

module Int64 = struct
  include Int64

  external of_string : string -> int64 = "caml_int64_of_string"
end

module Int63 = struct
  include Stdlib.Sys.Immediate64.Make (Int) (Int64)

  let of_string str : t =
    match repr with
    | Immediate -> Int.of_string str
    | Non_immediate -> Int64.of_string str
  ;;
end

module List = struct
  include List

  let return x = [ x ]

  let hd t =
    match t with
    | [] -> None
    | hd :: _ -> Some hd
  ;;

  let hd_exn t =
    match t with
    | [] -> invalid_arg "List.hd_exn"
    | hd :: _ -> hd
  ;;

  let split_n t_orig n =
    if n <= 0
    then [], t_orig
    else (
      let rec loop n t ~acc =
        match t with
        | [] -> t_orig, []
        | hd :: tl -> if n = 0 then rev acc, t else loop (n - 1) tl ~acc:(hd :: acc)
      in
      loop n t_orig ~acc:[])
  ;;

  (* directly copied from [Base.List] *)
  let mem t a ~equal =
    let rec mem_loop equal a = function
      | [] -> false
      | b :: bs -> equal a b || mem_loop equal a bs
    in
    mem_loop equal a t
  ;;

  let find t ~f = find_opt ~f t
  let init len ~f = init ~len ~f

  let split_while t ~f =
    let rec loop acc = function
      | x :: rest when f x -> loop (x :: acc) rest
      | rest -> List.rev acc, rest
    in
    loop [] t
  ;;

  let for_all2 t1 t2 ~f =
    if List.length t1 <> List.length t2
    then `Unequal_lengths
    else `Ok (List.for_all2 t1 t2 ~f)
  ;;

  let partition_map t ~f = partition_map f t
  let partition_tf t ~f = partition_map t ~f:(fun a -> if f a then Left a else Right a)

  let partition_mapi t ~f =
    let rec partition_mapi_loop t i left right =
      match t with
      | [] -> rev left, rev right
      | x :: t ->
        (match f i x with
         | Either.Left y -> partition_mapi_loop t (i + 1) (y :: left) right
         | Either.Right y -> partition_mapi_loop t (i + 1) left (y :: right))
    in
    partition_mapi_loop t 0 [] []
  ;;

  (* copied directly from [Base.List] *)
  let partition3_map t ~f =
    let rec partition3_map_loop t fst snd trd =
      match t with
      | [] -> rev fst, rev snd, rev trd
      | x :: t ->
        (match f x with
         | `Fst y -> partition3_map_loop t (y :: fst) snd trd
         | `Snd y -> partition3_map_loop t fst (y :: snd) trd
         | `Trd y -> partition3_map_loop t fst snd (y :: trd))
    in
    partition3_map_loop t [] [] [] [@nontail]
  ;;

  (* tested *)
  let map2_exn t1 t2 ~f =
    let[@cold] raise () =
      Printf.invalid_argf
        "length mismatch in %s: %d <> %d"
        "map2_exn"
        (List.length t1)
        (List.length t2)
        ()
    in
    let[@tail_mod_cons] rec map2_exn_loop t1 t2 ~f =
      match t1, t2 with
      | [], [] -> []
      | hd1 :: tl1, hd2 :: tl2 -> f hd1 hd2 :: map2_exn_loop tl1 tl2 ~f
      | _ -> (raise [@tailcall false]) ()
    in
    map2_exn_loop t1 t2 ~f
  ;;

  (* tested *)
  let map3_exn t1 t2 t3 ~f =
    let[@cold] raise () =
      let n1 = List.length t1 in
      let n2 = List.length t2 in
      let n3 = List.length t3 in
      Printf.invalid_argf
        "length mismatch in %s: %d <> %d || %d <> %d"
        "map3_exn"
        n1
        n2
        n2
        n3
        ()
    in
    let[@tail_mod_cons] rec map3_exn_loop t1 t2 t3 ~f =
      match t1, t2, t3 with
      | [], [], [] -> []
      | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 ->
        f hd1 hd2 hd3 :: map3_exn_loop tl1 tl2 tl3 ~f
      | _ -> (raise [@tailcall false]) ()
    in
    map3_exn_loop t1 t2 t3 ~f
  ;;

  module Or_unequal_lengths = struct
    type 'a t =
      | Ok of 'a
      | Unequal_lengths
  end

  let zip t1 t2 =
    let rec loop t1 t2 acc =
      match t1, t2 with
      | [], [] -> Or_unequal_lengths.Ok (rev acc)
      | hd1 :: tl1, hd2 :: tl2 -> loop tl1 tl2 ((hd1, hd2) :: acc)
      | _ -> Unequal_lengths
    in
    loop t1 t2 []
  ;;

  let zip_exn t1 t2 = map2_exn t1 t2 ~f:(fun a1 a2 -> a1, a2)

  (* copied directly from [Base.List] *)
  let unzip list =
    let rec unzip_loop list l1 l2 =
      match list with
      | [] -> l1, l2
      | (x, y) :: tl -> unzip_loop tl (x :: l1) (y :: l2)
    in
    unzip_loop (rev list) [] []
  ;;

  (* copied directly from [Base.List] *)
  let unzip3 list =
    let rec unzip3_loop list l1 l2 l3 =
      match list with
      | [] -> l1, l2, l3
      | (x, y, z) :: tl -> unzip3_loop tl (x :: l1) (y :: l2) (z :: l3)
    in
    unzip3_loop (rev list) [] [] []
  ;;

  let fold t ~init ~f = List.fold_left ~f ~init t

  (* copied directly from [Base.List] with unboxed and template annotations removed *)
  let foldi t ~init ~f =
    let _, acc = fold t ~init:(0, init) ~f:(fun (i, acc) v -> i + 1, f i acc v) in
    acc
  ;;

  let fold_map t ~init ~f = List.fold_left_map t ~init ~f

  (* copied directly from [Base.List] with the [@nontail] annotation removed *)
  let is_sorted l ~compare =
    let rec is_sorted_loop l =
      match l with
      | [] | [ _ ] -> true
      | x1 :: (x2 :: _ as rest) -> compare x1 x2 <= 0 && is_sorted_loop rest
    in
    is_sorted_loop l
  ;;

  let sum (type sum) (module M : Container.Summable with type t = sum) t ~f =
    fold t ~init:M.zero ~f:(fun sum elt -> M.(sum + f elt))
  ;;

  (* copied directly from [Base.List] with template annotations removed *)
  let reduce l ~f =
    match l with
    | [] -> None
    | hd :: tl -> Some (fold ~init:hd ~f tl)
  ;;

  (* copied directly from [Base.List] with the [@nontail] annotation removed *)
  let folding_mapi t ~init ~f =
    let acc = ref init in
    mapi t ~f:(fun i x ->
      let new_acc, y = f i !acc x in
      acc := new_acc;
      y)
  ;;

  let filter_opt t = filter_map t ~f:(fun x -> x)
  let folding_map t ~init ~f = snd (fold_map t ~init ~f)

  module Let_syntax = struct
    let return = return

    module Let_syntax = struct
      let return = return
      let map = map
      let bind t ~f = concat_map t ~f
    end
  end

  (* copied directly from [Base.List] with template annotations removed *)
  let reduce_exn l ~f =
    match l with
    | [] -> invalid_arg "List.reduce_exn"
    | hd :: tl -> fold ~init:hd ~f tl
  ;;

  (* tested *)
  let filter_mapi t ~f =
    let[@tail_mod_cons] rec filter_mapi_loop i t ~f =
      match t with
      | [] -> []
      | hd :: tl ->
        (match f i hd with
         | None -> filter_mapi_loop (i + 1) tl ~f
         | Some hd' -> hd' :: filter_mapi_loop (i + 1) tl ~f)
    in
    filter_mapi_loop 0 t ~f
  ;;

  let sort t ~compare = sort ~cmp:compare t

  let last_exn t =
    match last t with
    | Some x -> x
    | None -> failwith "List.last_exn: empty list"
  ;;

  let drop_last_exn = function
    | [] -> failwith "List.drop_last_exn: empty list"
    | _ :: _ as t -> rev (tl (rev t))
  ;;

  let find_all_dups l ~compare =
    let sorted = sort l ~compare in
    (* Walk the list and record the first of each consecutive run of identical elements *)
    let[@tail_mod_cons] rec find_all_dups_loop sorted prev ~already_recorded =
      match sorted with
      | [] -> []
      | hd :: tl ->
        if compare prev hd <> 0
        then find_all_dups_loop tl hd ~already_recorded:false
        else if already_recorded
        then find_all_dups_loop tl hd ~already_recorded:true
        else hd :: find_all_dups_loop tl hd ~already_recorded:true
    in
    match sorted with
    | [] -> []
    | hd :: tl -> find_all_dups_loop tl hd ~already_recorded:false [@nontail]
  ;;

  (* tested *)
  let dedup_and_sort t ~compare =
    let sorted_by_elt =
      mapi t ~f:(fun i x -> i, x) |> sort ~compare:(fun (_, x1) (_, x2) -> compare x1 x2)
    in
    let rec reduce_loop list =
      match list with
      | [] | [ _ ] -> list
      | (i1, x1) :: (i2, x2) :: rest ->
        if compare x1 x2 = 0
        then (
          (* keep the later one *)
          let pick = if i1 > i2 then i1, x1 else i2, x2 in
          reduce_loop (pick :: rest))
        else (i1, x1) :: reduce_loop ((i2, x2) :: rest)
    in
    reduce_loop sorted_by_elt |> map ~f:snd
  ;;

  (* directly copied from [Base.List] *)
  let stable_dedup list ~compare =
    match list with
    | [] | [ _ ] -> list (* special case for performance *)
    | _ :: _ :: _ ->
      let open struct
        type 'a dedup =
          { elt : 'a
          ; mutable dup : bool
          }
      end in
      (* [stable_dedup] keeps the first of each set of duplicates. [dedup_and_sort] keeps
         the last. We define one in terms of the other by passing the values in reverse
         order, hence the [rev_map] in the definition of [dedups]. We restore the order in
         the final [fold]. *)
      let dedups = rev_map list ~f:(fun elt -> { elt; dup = true }) in
      let unique = dedup_and_sort dedups ~compare:(fun x y -> compare x.elt y.elt) in
      iter unique ~f:(fun dedup -> dedup.dup <- false);
      fold dedups ~init:[] ~f:(fun acc dedup ->
        if dedup.dup then acc else dedup.elt :: acc)
  ;;

  module Assoc = struct
    type ('k, 'v) t = ('k * 'v) list

    let find t ~equal key =
      match find t ~f:(fun (key', _) -> equal key key') with
      | None -> None
      | Some (_, value) -> Some value
    ;;
  end

  module Cartesian_product = struct
    (* copied directly from [Base.List] *)
    let map2 a b ~f = concat_map a ~f:(fun x -> map b ~f:(fun y -> f x y))
  end

  (* copied directly from [Base.List] *)
  let fold2_exn t1 t2 ~init ~f =
    let rec loop acc a b =
      match a, b with
      | [], [] -> acc
      | x :: xs, y :: ys -> loop (f acc x y) xs ys
      | _ -> invalid_arg "List.fold2_exn: lists of unequal length"
    in
    loop init t1 t2
  ;;
end

module Monad = struct
  module type Infix = Monad_infix
  module type S = Monad
end

module Nonempty_list = struct
  (* The type comes from [nonempty_list_type], the same one [Base.Nonempty_list] uses, so
     values are interchangeable with Base's. Once it is declared, a bare [::] in this
     module builds a nonempty list, so plain lists are built with [Stdlib.List.cons]. *)
  type 'a t = 'a Nonempty_list_type.Nonempty_list.t = ( :: ) of 'a * 'a list

  let of_list_exn : _ list -> _ t = function
    | [] -> invalid_arg "Nonempty_list.of_list_exn: empty list"
    | hd :: tl -> hd :: tl
  ;;

  let to_list (hd :: tl : _ t) = Stdlib.List.cons hd tl

  let of_list : _ list -> _ t option = function
    | [] -> None
    | hd :: tl -> Some (hd :: tl)
  ;;

  let cons hd t : _ t = hd :: to_list t
  let length (_ :: tl : _ t) = 1 + List.length tl
  let map (hd :: tl : _ t) ~f : _ t = f hd :: List.map tl ~f
  let mapi (hd :: tl : _ t) ~f : _ t = f 0 hd :: List.mapi tl ~f:(fun i x -> f (i + 1) x)

  let fold_map (hd :: tl : _ t) ~init ~f : _ * _ t =
    let acc, hd = f init hd in
    let acc, tl = List.fold_map tl ~init:acc ~f in
    acc, hd :: tl
  ;;

  let fold_right t ~init ~f = List.fold_right (to_list t) ~init ~f
  let exists (hd :: tl : _ t) ~f = f hd || List.exists tl ~f
  let reduce (hd :: tl : _ t) ~f = List.fold tl ~init:hd ~f
  let append (hd :: tl : _ t) l : _ t = hd :: List.append tl l
  let ( @ ) t1 t2 = append t1 (to_list t2)
  let filter_map t ~f = List.filter_map (to_list t) ~f
  let reverse t = of_list_exn (List.rev (to_list t))
  let sort t ~compare = of_list_exn (List.sort (to_list t) ~compare)

  let concat_map (hd :: tl : _ t) ~f : _ t =
    let rest = List.map tl ~f:(fun x -> to_list (f x)) in
    let (hd' :: tl' : _ t) = f hd in
    hd' :: List.concat (Stdlib.List.cons tl' rest)
  ;;

  let last (hd :: tl : _ t) =
    match List.last tl with
    | None -> hd
    | Some x -> x
  ;;

  let drop_last t = List.rev (Stdlib.List.tl (List.rev (to_list t)))
end

module Option = struct
  include Option

  type 'a t = 'a option =
    | None
    | Some of 'a

  let return x = Some x
  let some x = Some x

  (* copied directly from [Base.Option] *)
  let first_some x y =
    match x with
    | Some _ -> x
    | None -> y
  ;;

  let fold t ~init ~f =
    match t with
    | None -> init
    | Some x -> f init x
  ;;

  let merge a b ~f =
    match a, b with
    | None, x | x, None -> x
    | Some a, Some b -> Some (f a b)
  ;;

  (* copied directly from [Base.Option] *)
  let some_if_thunk cond thunk = if cond then Some (thunk ()) else None

  (* copied directly from [Base.Option] *)
  let both x y =
    match x, y with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (x, y)
  ;;

  let map2 x y ~f =
    match x, y with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (f x y)
  ;;

  let mem t a ~equal =
    match t with
    | None -> false
    | Some b -> equal a b
  ;;

  let equal equal a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> equal a b
    | None, Some _ | Some _, None -> false
  ;;

  let some_if cond x = if cond then Some x else None

  (* copied directly from [Base.Option] *)
  let try_with f =
    match f () with
    | x -> Some x
    | exception _ -> None
  ;;

  let is_none = function
    | None -> true
    | Some _ -> false
  ;;

  let value_exn = function
    | Some x -> x
    | None -> failwith "Option.value_exn called on None"
  ;;

  let value_or_thunk t ~default =
    match t with
    | None -> default ()
    | Some x -> x
  ;;

  let value_map t ~f ~default =
    match t with
    | Some a -> f a
    | None -> default
  ;;

  let all ts =
    let rec all_loop ~acc = function
      | [] -> Some (List.rev acc)
      | None :: _ -> None
      | Some x :: xs -> all_loop ~acc:(x :: acc) xs
    in
    all_loop ~acc:[] ts
  ;;

  let bind t ~f =
    match t with
    | None -> None
    | Some x -> f x
  ;;

  module Let_syntax = struct
    let return = return

    module Let_syntax = struct
      let map = map
      let bind = bind
    end
  end
end

module Queue = struct
  include Queue

  let enqueue t a = add a t
  let dequeue t = take_opt t

  let dequeue_exn t =
    match take_opt t with
    | Some a -> a
    | None -> invalid_arg "Queue.dequeue_exn"
  ;;

  let to_list t = to_seq t |> List.of_seq
  let of_list list = List.to_seq list |> of_seq
end

module Ref = struct
  type 'a t = 'a ref = { mutable contents : 'a }

  let replace t f = t := f !t
end

module Result = struct
  include Result

  type ('ok, 'err) t = ('ok, 'err) result =
    | Ok of 'ok
    | Error of 'err

  let return ok = Ok ok
  let fail err = Error err

  (* tested *)
  let all ts =
    List.fold_right ts ~init:(Ok []) ~f:(fun elt acc ->
      match elt, acc with
      | Ok elt, Ok acc -> Ok (elt :: acc)
      | Error elt, _ -> Error elt
      | _, Error _ -> acc)
  ;;

  let to_either t =
    match t with
    | Ok x -> Either.Left x
    | Error x -> Either.Right x
  ;;

  (* directly copied from [Base.Result] *)
  let combine_errors l =
    let ok, errs = List.partition_map l ~f:to_either in
    match errs with
    | [] -> Ok ok
    | _ :: _ -> Error errs
  ;;

  (* directly copied from [Stdppx] (awaiting upstreaming) *)
  let map_error t ~f =
    match t with
    | Ok a -> Ok a
    | Error e -> Error (f e)
  ;;

  module Let_syntax = struct
    let ( >>| ) t f = map t ~f
    let ( >>= ) t f = bind t ~f
    let return = return

    module Let_syntax = struct
      let map = map
      let bind = bind

      let both a b =
        match a, b with
        | Ok a, Ok b -> Ok (a, b)
        | Error e, _ -> Error e
        | _, Error e -> Error e
      ;;
    end
  end
end

module Stringable = struct
  module type S = Stringable
end

module Sexpable = struct
  module Of_stringable (M : sig
      type t

      val to_string : t -> string
      val of_string : string -> t
    end) =
  struct
    (* directly copied from [Base.Sexpable] *)
    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom s ->
        (try M.of_string s with
         | exn -> Sexplib0.Sexp_conv.of_sexp_error_exn exn sexp)
      | Sexp.List _ ->
        Sexplib0.Sexp_conv.of_sexp_error
          "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list"
          sexp
    ;;

    let sexp_of_t t = Sexp.Atom (M.to_string t)
  end
end

module Staged = struct
  type 'a t = 'a

  let stage x = x
  let unstage x = x
end

module Stack = struct
  include Stack

  let push t a = push a t
  let pop_exn t = pop t

  let pop t =
    match pop t with
    | x -> Some x
    | exception Empty -> None
  ;;
end

module String = struct
  module Map' = Map
  module Set' = Set
  include String
  open Int.O
  module Map = Map'.Make (String)
  module Set = Set'.Make (String)
  module Table = Hashtbl.Make (String)

  let of_string t = t
  let to_string t = t
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_string

  (* directly copied from [Base.String.rfindi_or_null] with minimal changes *)
  let rfindi ?pos t ~f =
    let rec loop i = if i < 0 then None else if f i t.[i] then Some i else loop (i - 1) in
    let pos =
      match pos with
      | Some pos -> pos
      | None -> length t - 1
    in
    loop pos
  ;;

  (* directly copied from [Base.String] with minimal changes *)
  let rsplit2_exn =
    let not_found () =
      raise (Sexplib0.Sexp.Not_found_s (Atom "String.rsplit2_exn: not found"))
    in
    let rsplit2_exn line ~on:delim =
      let len = length line in
      let pos =
        match rindex_from_opt line (len - 1) delim with
        | None -> not_found ()
        | Some pos -> pos
      in
      sub line ~pos:0 ~len:pos, sub line ~pos:(pos + 1) ~len:(len - pos - 1)
    in
    (* named to preserve symbol in compiled binary *)
    rsplit2_exn
  ;;

  (* directly copied from [Base.String] *)
  let rsplit2 line ~on =
    try Some (rsplit2_exn line ~on) with
    | Sexplib0.Sexp.Not_found_s _ | Stdlib.Not_found -> None
  ;;

  (* tested *)
  let substr_index ?(pos = 0) t ~pattern =
    let pat_len = String.length pattern in
    let rec substr_index_loop start end_ =
      if end_ > String.length t
      then None
      else if String.equal pattern (String.sub t ~pos:start ~len:pat_len)
      then Some start
      else substr_index_loop (start + 1) (end_ + 1)
    in
    substr_index_loop pos (pos + pat_len)
  ;;

  (* directly copied from [Base.String], with the [Null]/[This] option type spelled as an
     ordinary option *)
  let findi t ~f =
    let n = length t in
    let rec loop i =
      if i >= n then None else if f i t.[i] then Some (i, t.[i]) else loop (i + 1)
    in
    loop 0
  ;;

  (* [Base.String] routes this through [Search_pattern]; a direct scan is equivalent and
     avoids pulling that machinery in. *)
  let substr_index_all t ~may_overlap ~pattern =
    let n = length t
    and m = length pattern in
    let step = if may_overlap then 1 else Int.max m 1 in
    let rec loop i acc =
      if i + m > n
      then List.rev acc
      else if String.equal (sub t ~pos:i ~len:m) pattern
      then loop (i + step) (i :: acc)
      else loop (i + 1) acc
    in
    loop 0 []
  ;;

  let lstrip ?(drop = Char.is_whitespace) t =
    let n = length t in
    let rec first_non_drop i =
      if i < n && drop t.[i] then first_non_drop (i + 1) else i
    in
    match first_non_drop 0 with
    | i when i = n -> ""
    | 0 -> t
    | i -> sub t ~pos:i ~len:(n - i)
  ;;

  let split_lines t =
    let back_up_pos_and_eol_at_newline ~t ~pos =
      let pos = pos - if pos > 0 && Char.equal t.[pos - 1] '\r' then 2 else 1 in
      pos, pos + 1
    in
    let n = length t in
    if n = 0
    then []
    else (
      let pos = n - 1 in
      let eol = n in
      let pos, eol =
        if Char.equal t.[pos] '\n'
        then back_up_pos_and_eol_at_newline ~t ~pos
        else pos, eol
      in
      let rec loop ~pos ~eol ac =
        if pos >= 0
        then
          if not (Char.equal t.[pos] '\n')
          then loop ~pos:(pos - 1) ~eol ac
          else (
            let start = pos + 1 in
            let ac = sub t ~pos:start ~len:(eol - start) :: ac in
            let pos, eol = back_up_pos_and_eol_at_newline ~t ~pos in
            loop ~pos ~eol ac)
        else sub t ~pos:0 ~len:eol :: ac
      in
      loop ~pos ~eol [])
  ;;

  (* directly copied from [Base.String] *)
  let is_suffix t ~suffix =
    let n = length t
    and m = length suffix in
    m <= n && String.equal (sub t ~pos:(n - m) ~len:m) suffix
  ;;

  (* directly copied from [Base.String] *)
  let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c))

  (* directly copied from [Base.String] *)
  let rstrip ?(drop = Char.is_whitespace) t =
    match last_non_drop t ~drop with
    | None -> ""
    | Some i -> if i = length t - 1 then t else prefix t (i + 1)
  ;;

  (* directly copied from [Base.String] with minimal changes *)
  let strip ?(drop = Char.is_whitespace) t =
    let length = length t in
    if length = 0 || not (drop t.[0] || drop t.[length - 1])
    then t
    else (
      let rec first_non_drop i =
        if i < length && drop t.[i] then first_non_drop (i + 1) else i
      in
      match first_non_drop 0 with
      | first when first = length -> ""
      | first ->
        (match last_non_drop t ~drop with
         | None -> assert false
         | Some last -> sub t ~pos:first ~len:(last - first + 1)))
  ;;

  let filter t ~f = String.to_seq t |> Seq.filter f |> String.of_seq
  let index_from t i c = index_from_opt t i c

  (* directly copied from [Base.String] *)
  let chop_prefix t ~prefix =
    if is_prefix t ~prefix then Some (drop_prefix t (length prefix)) else None
  ;;

  (* directly copied from [Base.String] *)
  let chop_suffix t ~suffix =
    if is_suffix t ~suffix then Some (drop_suffix t (length suffix)) else None
  ;;

  (* directly copied from [Base.String] *)
  let chop_suffix_exn t ~suffix =
    match chop_suffix t ~suffix with
    | Some s -> s
    | None -> Printf.invalid_argf "String.chop_suffix_exn %S %S" t suffix ()
  ;;

  let split t ~on = String.split_on_char t ~sep:on
  let concat ?(sep = "") strings = concat ~sep strings
  let mem = contains

  (* directly copied from [Base.String] with template annotations removed *)
  let tr ~target ~replacement s =
    if Char.equal target replacement
    then s
    else if mem s target
    then map s ~f:(fun c -> if Char.equal c target then replacement else c)
    else s
  ;;

  (* directly copied from [Stdppx] (awaiting upstreaming) *)
  let is_substring t ~substring =
    let len_t = String.length t in
    let len_sub = String.length substring in
    if len_sub = 0
    then true
    else if len_sub > len_t
    then false
    else (
      let rec matches_at pos sub_pos =
        if sub_pos = len_sub
        then true
        else if Char.equal (get t pos) (get substring sub_pos)
        then matches_at (pos + 1) (sub_pos + 1)
        else false
      in
      let rec is_substring_at pos =
        if pos + len_sub > len_t
        then false
        else if matches_at pos 0
        then true
        else is_substring_at (pos + 1)
      in
      is_substring_at 0)
  ;;

  let lowercase = Stdlib.String.lowercase_ascii
  let uppercase = Stdlib.String.uppercase_ascii
  let capitalize = Stdlib.String.capitalize_ascii
  let uncapitalize = Stdlib.String.uncapitalize_ascii
end

module Source_code_position = struct
  type t = Stdlib.Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }

  (* directly copied from [Base.Source_code_position] *)
  let of_pos (pos_fname, pos_lnum, pos_cnum, _) =
    { Stdlib.Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol = 0 }
  ;;

  (* directly copied from [Base.Source_code_position] *)
  let make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol =
    String.concat
      [ pos_fname; ":"; Int.to_string pos_lnum; ":"; Int.to_string (pos_cnum - pos_bol) ]
  ;;

  (* directly copied from [Base.Source_code_position] *)
  let to_string { Stdlib.Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol } =
    make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
end

module Type_equal = struct
  type ('a, 'b) t = T : ('a, 'a) t
end

module With_return = struct
  type 'a return = { return : 'b. 'a -> 'b } [@@unboxed]

  (* directly copied from [Base.With_return] *)
  let with_return (type a) f =
    (* Raised to indicate ~return was called. Local so that the exception is tied to a
       particular call of [with_return]. *)
    let exception Return of a in
    let is_alive = ref true in
    let return a =
      if not !is_alive
      then failwith "use of [return] from a [with_return] that already returned";
      Exn.raise_without_backtrace (Return a)
    in
    try
      let a = f { return } in
      is_alive := false;
      a
    with
    | exn ->
      is_alive := false;
      (match exn with
       | Return a -> a
       | _ -> raise exn)
  ;;
end
