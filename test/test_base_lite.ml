open Base_quickcheck.Export
open Base.Export
module Base_lite = Ppx_helpers.Base_lite

(* Test functions in [Ppx_helpers.Base_lite] that have non-trivial new implementations. *)

let require_equal (type a) ?(here = Stdlib.Lexing.dummy_pos) ~equal ~sexp_of a1 a2 =
  Expect_test_helpers_base.require_equal
    ~here
    (module struct
      type t = a

      let equal = equal
      let sexp_of_t = sexp_of
    end)
    a1
    a2
;;

module%test [@name "Base_lite additions"] _ = struct
  let%expect_test "findi" =
    let%quick_test pred (s : string) (c : char) =
      require_equal
        ~equal:[%equal: (int * char) option]
        ~sexp_of:[%sexp_of: (int * char) option]
        (Base_lite.String.findi s ~f:(fun _ x -> Base.Char.equal x c))
        (Base.String.findi s ~f:(fun _ x -> Base.Char.equal x c))
    in
    [%expect {| |}]
  ;;

  let%expect_test "substr_index_all" =
    let%quick_test pred (s : string) (pattern : string) (may_overlap : bool) =
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.String.substr_index_all s ~may_overlap ~pattern)
        (Base.String.substr_index_all s ~may_overlap ~pattern)
    in
    [%expect {| |}]
  ;;

  let%expect_test "is_sorted" =
    let%quick_test pred (l : int list) =
      require_equal
        ~equal:[%equal: bool]
        ~sexp_of:[%sexp_of: bool]
        (Base_lite.List.is_sorted l ~compare:Base.compare_int)
        (Base.List.is_sorted l ~compare:Base.compare_int)
    in
    [%expect {| |}]
  ;;

  let%expect_test "lstrip" =
    let%quick_test pred (s : string) =
      require_equal
        ~equal:[%equal: string]
        ~sexp_of:[%sexp_of: string]
        (Base_lite.String.lstrip s)
        (Base.String.lstrip s)
    in
    [%expect {| |}]
  ;;

  let%expect_test "strip" =
    let%quick_test pred (s : string) =
      require_equal
        ~equal:[%equal: string]
        ~sexp_of:[%sexp_of: string]
        (Base_lite.String.strip s)
        (Base.String.strip s)
    in
    [%expect {| |}]
  ;;

  let%expect_test "split_lines" =
    let%quick_test pred (s : string) =
      require_equal
        ~equal:[%equal: string list]
        ~sexp_of:[%sexp_of: string list]
        (Base_lite.String.split_lines s)
        (Base.String.split_lines s)
    in
    [%expect {| |}]
  ;;

  let%expect_test "folding_mapi" =
    let%quick_test pred (l : int list) =
      let f i acc x = acc + x, (i, acc) in
      require_equal
        ~equal:[%equal: (int * int) list]
        ~sexp_of:[%sexp_of: (int * int) list]
        (Base_lite.List.folding_mapi l ~init:0 ~f)
        (Base.List.folding_mapi l ~init:0 ~f)
    in
    [%expect {| |}]
  ;;
end

module%test Map = struct
  module Int_map_lite = Base_lite.Map.Make (Base_lite.Int)

  let%expect_test "of_alist" =
    let%quick_test pred (alist : (int * string) list) =
      require_equal
        ~equal:[%equal: (int * string) list option]
        ~sexp_of:[%sexp_of: (int * string) list option]
        (Base.Option.try_with (fun () ->
           Int_map_lite.of_alist_exn alist |> Int_map_lite.to_alist))
        (Base.Option.try_with (fun () ->
           Base.Map.of_alist_exn (module Base.Int) alist |> Base.Map.to_alist))
    in
    [%expect {| |}]
  ;;

  let%expect_test "of_alist_multi" =
    let%quick_test pred (alist : (int * string) list) =
      require_equal
        ~equal:[%equal: (int * string list) list]
        ~sexp_of:[%sexp_of: (int * string list) list]
        (Int_map_lite.of_alist_multi alist |> Int_map_lite.to_alist)
        (Base.Map.of_alist_multi (module Base.Int) alist |> Base.Map.to_alist)
    in
    [%expect {| |}]
  ;;

  let%test_unit "map operations" =
    let left = Int_map_lite.of_alist_exn [ 1, 10; 2, 20 ] in
    let right = Int_map_lite.of_alist_exn [ 2, 3; 3, 30 ] in
    [%test_result: (int * int) list]
      (Int_map_lite.map left ~f:Int.succ |> Int_map_lite.to_alist)
      ~expect:[ 1, 11; 2, 21 ];
    [%test_result: (int * int) list]
      (Int_map_lite.merge_disjoint_exn left (Int_map_lite.singleton 3 30)
       |> Int_map_lite.to_alist)
      ~expect:[ 1, 10; 2, 20; 3, 30 ];
    [%test_result: (int * int) list]
      (Int_map_lite.merge_skewed left right ~combine:(fun ~key left right ->
         key + left + right)
       |> Int_map_lite.to_alist)
      ~expect:[ 1, 10; 2, 25; 3, 30 ];
    [%test_result: (int * int) list]
      (Int_map_lite.add_exn left ~key:3 ~data:30 |> Int_map_lite.to_alist)
      ~expect:[ 1, 10; 2, 20; 3, 30 ];
    [%test_result: bool]
      (match Int_map_lite.add_exn left ~key:1 ~data:30 with
       | exception _ -> true
       | _ -> false)
      ~expect:true;
    [%test_result: bool]
      (match Int_map_lite.merge_disjoint_exn left right with
       | exception _ -> true
       | _ -> false)
      ~expect:true
  ;;

  let%expect_test "of_alist_reduce" =
    let%quick_test pred (alist : (int * string) list) =
      let f previous data = previous ^ data in
      require_equal
        ~equal:[%equal: (int * string) list]
        ~sexp_of:[%sexp_of: (int * string) list]
        (Int_map_lite.of_alist_reduce alist ~f |> Int_map_lite.to_alist)
        (Base.Map.of_alist_reduce (module Base.Int) alist ~f |> Base.Map.to_alist)
    in
    [%expect {| |}]
  ;;
end

module%test Hashtbl = struct
  let%expect_test "to_alist" =
    let%quick_test pred (alist : (string * int) list) =
      let base_lite = Base_lite.String.Table.create () in
      let base = Base.Hashtbl.create (module Base.String) in
      Base.List.iter alist ~f:(fun (key, data) ->
        if not (Base.Hashtbl.mem base key)
        then (
          Base_lite.String.Table.add_exn base_lite ~key ~data;
          Base.Hashtbl.add_exn base ~key ~data));
      require_equal
        ~equal:
          (Base.Comparable.lift
             [%equal: (string * int) list]
             ~f:(Base.List.sort ~compare:[%compare: string * _]))
        ~sexp_of:[%sexp_of: (string * int) list]
        (Base_lite.String.Table.to_alist base_lite)
        (Base.Hashtbl.to_alist base)
    in
    [%expect {| |}]
  ;;
end

module%test Hash_set = struct
  module String_set = Base_lite.Hash_set.Make (Base.String)

  let%expect_test "add/mem/to_list" =
    let%quick_test pred (list : string list) =
      let base_lite = String_set.create () in
      let base = Base.Hash_set.create (module Base.String) in
      Base.List.iter list ~f:(fun elt ->
        String_set.add base_lite elt;
        Base.Hash_set.add base elt);
      Base.List.iter list ~f:(fun elt ->
        Expect_test_helpers_base.require (String_set.mem base_lite elt));
      require_equal
        ~equal:
          (Base.Comparable.lift
             [%equal: string list]
             ~f:(Base.List.sort ~compare:[%compare: string]))
        ~sexp_of:[%sexp_of: string list]
        (String_set.to_list base_lite)
        (Base.Hash_set.to_list base)
    in
    [%expect {| |}]
  ;;
end

module%test List = struct
  let%expect_test "partition_mapi" =
    let%quick_test pred (list : int list) =
      require_equal
        ~equal:[%equal: int list * (int * int) list]
        ~sexp_of:[%sexp_of: int list * (int * int) list]
        (Base_lite.List.partition_mapi list ~f:(fun i x ->
           if x mod 2 = 0 then Base_lite.Either.Left x else Base_lite.Either.Right (i, x)))
        (Base.List.partition_mapi list ~f:(fun i x ->
           if x mod 2 = 0 then Base.Either.First x else Base.Either.Second (i, x)))
    in
    [%expect {| |}]
  ;;

  let%expect_test "split_n" =
    let%quick_test pred (list : int list) (n : int) =
      let lite = Base_lite.List.split_n list n in
      let base =
        let left, right = Base.List.split_n list n in
        left, right
      in
      require_equal
        ~equal:[%equal: int list * int list]
        ~sexp_of:[%sexp_of: int list * int list]
        lite
        base
    in
    [%expect {| |}]
  ;;

  let%expect_test "map2_exn" =
    let%quick_test pred (list1 : int list) (list2 : int list) =
      let f x1 x2 = x1, x2 in
      require_equal
        ~equal:[%equal: (int * int) list option]
        ~sexp_of:[%sexp_of: (int * int) list option]
        (Base.Option.try_with (fun () -> Base_lite.List.map2_exn list1 list2 ~f))
        (Base.Option.try_with (fun () -> Base.List.map2_exn list1 list2 ~f))
    in
    [%expect {| |}]
  ;;

  let%expect_test "map3_exn" =
    let%quick_test pred (list1 : int list) (list2 : int list) (list3 : int list) =
      let f x1 x2 x3 = x1, x2, x3 in
      require_equal
        ~equal:[%equal: (int * int * int) list option]
        ~sexp_of:[%sexp_of: (int * int * int) list option]
        (Base.Option.try_with (fun () -> Base_lite.List.map3_exn list1 list2 list3 ~f))
        (Base.Option.try_with (fun () -> Base.List.map3_exn list1 list2 list3 ~f))
    in
    [%expect {| |}]
  ;;

  let%expect_test "filter_mapi" =
    let%quick_test pred (list : int list) =
      let f i x = if i mod 2 = 0 then None else Some (i, x) in
      require_equal
        ~equal:[%equal: (int * int) list]
        ~sexp_of:[%sexp_of: (int * int) list]
        (Base_lite.List.filter_mapi list ~f)
        (Base.List.filter_mapi list ~f)
    in
    [%expect {| |}]
  ;;

  let%expect_test "split_while" =
    let%quick_test pred (list : int list) (threshold : int) =
      let f x = x < threshold in
      require_equal
        ~equal:[%equal: int list * int list]
        ~sexp_of:[%sexp_of: int list * int list]
        (Base_lite.List.split_while list ~f)
        (Base.List.split_while list ~f)
    in
    [%expect {| |}]
  ;;

  let%expect_test "dedup_and_sort" =
    let%quick_test pred (tlist : (int * int) list) =
      let compare (x1, _) (x2, _) = compare x1 x2 in
      require_equal
        ~equal:[%equal: (int * int) list]
        ~sexp_of:[%sexp_of: (int * int) list]
        (Base_lite.List.dedup_and_sort tlist ~compare)
        (Base.List.dedup_and_sort tlist ~compare)
    in
    [%expect {| |}]
  ;;

  let%expect_test "find_all_dups" =
    let%quick_test pred (list : int list) =
      (* Reduce the domain so that duplicates are common. *)
      let list = Base.List.map list ~f:(fun x -> x % 4) in
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.List.find_all_dups list ~compare:Base.compare_int)
        (Base.List.find_all_dups list ~compare:Base.compare_int)
    in
    [%expect {| |}]
  ;;

  let%expect_test "drop_last_exn" =
    let%quick_test pred (list : int list) =
      require_equal
        ~equal:[%equal: int list option]
        ~sexp_of:[%sexp_of: int list option]
        (Base.Option.try_with (fun () -> Base_lite.List.drop_last_exn list))
        (Base.Option.try_with (fun () -> Base.List.drop_last_exn list))
    in
    [%expect {| |}]
  ;;
end

module%test Result = struct
  module Gen_result = struct
    open Base

    type ('ok, 'err) t = ('ok, 'err) Stdlib.result =
      | Ok of 'ok
      | Error of 'err
    [@@deriving equal, quickcheck, sexp_of]
  end

  let%expect_test "all" =
    let%quick_test pred (list : (int, string) Gen_result.t list) =
      require_equal
        ~equal:[%equal: (int list, string) Gen_result.t]
        ~sexp_of:[%sexp_of: (int list, string) Gen_result.t]
        (Base_lite.Result.all list)
        (Base.Result.all list)
    in
    [%expect {| |}]
  ;;
end

module%test Nonempty_list = struct
  let%expect_test "to_list / map / append / last / drop_last" =
    let%quick_test pred (hd : int) (tl : int list) (extra : int list) =
      let lite : int Base_lite.Nonempty_list.t = hd :: tl in
      let base : int Base.Nonempty_list.t = hd :: tl in
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.to_list lite)
        (Base.Nonempty_list.to_list base);
      let f x = x + 1 in
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.to_list (Base_lite.Nonempty_list.map lite ~f))
        (Base.Nonempty_list.to_list (Base.Nonempty_list.map base ~f));
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.to_list (Base_lite.Nonempty_list.append lite extra))
        (Base.Nonempty_list.to_list (Base.Nonempty_list.append base extra));
      require_equal
        ~equal:[%equal: int]
        ~sexp_of:[%sexp_of: int]
        (Base_lite.Nonempty_list.last lite)
        (Base.Nonempty_list.last base);
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.drop_last lite)
        (Base.Nonempty_list.drop_last base)
    in
    [%expect {| |}]
  ;;

  let%expect_test "length / mapi / fold_map / fold_right" =
    let%quick_test pred (hd : int) (tl : int list) =
      let lite : int Base_lite.Nonempty_list.t = hd :: tl in
      let base : int Base.Nonempty_list.t = hd :: tl in
      require_equal
        ~equal:[%equal: int]
        ~sexp_of:[%sexp_of: int]
        (Base_lite.Nonempty_list.length lite)
        (Base.Nonempty_list.length base);
      let f i x = i, x in
      require_equal
        ~equal:[%equal: (int * int) list]
        ~sexp_of:[%sexp_of: (int * int) list]
        (Base_lite.Nonempty_list.to_list (Base_lite.Nonempty_list.mapi lite ~f))
        (Base.Nonempty_list.to_list (Base.Nonempty_list.mapi base ~f));
      let f acc x = acc + x, acc in
      let lite_acc, lite_result = Base_lite.Nonempty_list.fold_map lite ~init:0 ~f in
      let base_acc, base_result = Base.Nonempty_list.fold_map base ~init:0 ~f in
      require_equal
        ~equal:[%equal: int * int list]
        ~sexp_of:[%sexp_of: int * int list]
        (lite_acc, Base_lite.Nonempty_list.to_list lite_result)
        (base_acc, Base.Nonempty_list.to_list base_result);
      let f x acc = x :: acc in
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.fold_right lite ~init:[] ~f)
        (Base.Nonempty_list.fold_right base ~init:[] ~f)
    in
    [%expect {| |}]
  ;;
end

module%test Or_error = struct
  let print (ts : int Base_lite.Or_error.t list) =
    Stdlib.print_endline
      (match Base_lite.Or_error.combine_errors ts with
       | Ok oks -> "Ok: " ^ Base.Sexp.to_string_hum [%sexp (oks : int list)]
       | Error error -> "Error: " ^ Base_lite.Error.to_string_hum error)
  ;;

  let%expect_test "combine_errors" =
    print [ Ok 1; Ok 2 ];
    [%expect {| Ok: (1 2) |}];
    print [ Ok 1; Base_lite.Or_error.error_string "boom" ];
    [%expect {| Error: boom |}];
    print
      [ Base_lite.Or_error.error_string "boom"
      ; Ok 2
      ; Base_lite.Or_error.errorf "bang %d" 3
      ];
    [%expect {| Error: (boom "bang 3") |}]
  ;;
end

module%test String = struct
  let%expect_test "substr_index" =
    let%quick_test pred
      (pos :
        (int[@quickcheck.generator Base_quickcheck.Generator.small_positive_or_zero_int])
          option)
      (str : string)
      (pattern : string)
      =
      require_equal
        ~equal:[%equal: int option]
        ~sexp_of:[%sexp_of: int option]
        (Base_lite.String.substr_index ?pos str ~pattern)
        (Base.String.substr_index ?pos str ~pattern)
    in
    [%expect {| |}]
  ;;
end

module%test Nonempty_list = struct
  (* [Base_lite.Nonempty_list] reuses the type from [nonempty_list_type], which is the
     same one [Base.Nonempty_list] is built on, so the two are directly comparable without
     a conversion. *)
  let lite hd tl = Base_lite.Nonempty_list.( :: ) (hd, tl)
  let base hd tl = Base.Nonempty_list.( :: ) (hd, tl)

  let%expect_test "concat_map" =
    let%quick_test pred (hd : int) (tl : int list) =
      let f x = Base_lite.Nonempty_list.(x :: [ x + 1 ]) in
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        Base_lite.Nonempty_list.(concat_map (hd :: tl) ~f |> to_list)
        (Base.List.concat_map (hd :: tl) ~f:(fun x -> [ x; x + 1 ]))
    in
    [%expect {| |}]
  ;;

  let%expect_test "map" =
    let%quick_test pred (hd : int) (tl : int list) =
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.map (lite hd tl) ~f:succ
         |> Base_lite.Nonempty_list.to_list)
        (Base.Nonempty_list.map (base hd tl) ~f:succ |> Base.Nonempty_list.to_list)
    in
    [%expect {| |}]
  ;;

  let%expect_test "exists" =
    let%quick_test pred (hd : int) (tl : int list) =
      require_equal
        ~equal:[%equal: bool]
        ~sexp_of:[%sexp_of: bool]
        (Base_lite.Nonempty_list.exists (lite hd tl) ~f:(fun x -> x > 0))
        (Base.Nonempty_list.exists (base hd tl) ~f:(fun x -> x > 0))
    in
    [%expect {| |}]
  ;;

  let%expect_test "reduce" =
    let%quick_test pred (hd : int) (tl : int list) =
      require_equal
        ~equal:[%equal: int]
        ~sexp_of:[%sexp_of: int]
        (Base_lite.Nonempty_list.reduce (lite hd tl) ~f:( + ))
        (Base.Nonempty_list.reduce (base hd tl) ~f:( + ))
    in
    [%expect {| |}]
  ;;

  let%expect_test "append" =
    let%quick_test pred (hd : int) (tl : int list) (extra : int list) =
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.append (lite hd tl) extra
         |> Base_lite.Nonempty_list.to_list)
        (Base.Nonempty_list.append (base hd tl) extra |> Base.Nonempty_list.to_list)
    in
    [%expect {| |}]
  ;;

  let%expect_test "filter_map" =
    let%quick_test pred (hd : int) (tl : int list) =
      let f x = if x > 0 then Some (x * 2) else None in
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.filter_map (lite hd tl) ~f)
        (Base.Nonempty_list.filter_map (base hd tl) ~f)
    in
    [%expect {| |}]
  ;;

  let%expect_test "reverse" =
    let%quick_test pred (hd : int) (tl : int list) =
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.reverse (lite hd tl) |> Base_lite.Nonempty_list.to_list)
        (Base.Nonempty_list.reverse (base hd tl) |> Base.Nonempty_list.to_list)
    in
    [%expect {| |}]
  ;;

  let%expect_test "sort" =
    let%quick_test pred (hd : int) (tl : int list) =
      let compare = Base.Int.compare in
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.Nonempty_list.sort (lite hd tl) ~compare
         |> Base_lite.Nonempty_list.to_list)
        (Base.Nonempty_list.sort (base hd tl) ~compare |> Base.Nonempty_list.to_list)
    in
    [%expect {| |}]
  ;;

  let%expect_test "last" =
    let%quick_test pred (hd : int) (tl : int list) =
      require_equal
        ~equal:[%equal: int]
        ~sexp_of:[%sexp_of: int]
        (Base_lite.Nonempty_list.last (lite hd tl))
        (Base.Nonempty_list.last (base hd tl))
    in
    [%expect {| |}]
  ;;

  let%expect_test "of_list_exn" =
    let%quick_test pred (list : int list) =
      require_equal
        ~equal:[%equal: int list option]
        ~sexp_of:[%sexp_of: int list option]
        (Base.Option.try_with (fun () ->
           Base_lite.Nonempty_list.of_list_exn list |> Base_lite.Nonempty_list.to_list))
        (Base.Option.try_with (fun () ->
           Base.Nonempty_list.of_list_exn list |> Base.Nonempty_list.to_list))
    in
    [%expect {| |}]
  ;;
end

module%test List = struct
  let%expect_test "filter_opt" =
    let%quick_test pred (list : int option list) =
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (Base_lite.List.filter_opt list)
        (Base.List.filter_opt list)
    in
    [%expect {| |}]
  ;;
end

module%test String = struct
  let%expect_test "is_suffix" =
    let%quick_test pred (str : string) (suffix : string) =
      require_equal
        ~equal:[%equal: bool]
        ~sexp_of:[%sexp_of: bool]
        (Base_lite.String.is_suffix str ~suffix)
        (Base.String.is_suffix str ~suffix)
    in
    [%expect {| |}]
  ;;
end

module%test Stack = struct
  (* [Stdlib.Stack] takes the element before the stack and raises on an empty pop, so
     these check the argument order and the option/exn split against [Base]. *)
  let drain_lite t =
    let rec loop acc =
      match Base_lite.Stack.pop t with
      | None -> Base.List.rev acc
      | Some x -> loop (x :: acc)
    in
    loop []
  ;;

  let drain_base t =
    let rec loop acc =
      match Base.Stack.pop t with
      | None -> Base.List.rev acc
      | Some x -> loop (x :: acc)
    in
    loop []
  ;;

  let%expect_test "push then pop drains in the same order as Base" =
    let%quick_test pred (list : int list) =
      let lite = Base_lite.Stack.create () in
      let base = Base.Stack.create () in
      Base.List.iter list ~f:(fun x -> Base_lite.Stack.push lite x);
      Base.List.iter list ~f:(fun x -> Base.Stack.push base x);
      require_equal
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
        (drain_lite lite)
        (drain_base base)
    in
    [%expect {| |}]
  ;;

  let%expect_test "pop_exn matches Base, including on an empty stack" =
    let%quick_test pred (list : int list) =
      let lite = Base_lite.Stack.create () in
      let base = Base.Stack.create () in
      Base.List.iter list ~f:(fun x -> Base_lite.Stack.push lite x);
      Base.List.iter list ~f:(fun x -> Base.Stack.push base x);
      require_equal
        ~equal:[%equal: int option]
        ~sexp_of:[%sexp_of: int option]
        (Base.Option.try_with (fun () -> Base_lite.Stack.pop_exn lite))
        (Base.Option.try_with (fun () -> Base.Stack.pop_exn base))
    in
    [%expect {| |}]
  ;;
end
