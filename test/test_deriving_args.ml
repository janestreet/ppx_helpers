open! Base
open Expect_test_helpers_base
module Using_deriving_args = Ppx_helpers_example.Using_deriving_args

module _ : module type of struct
  include Using_deriving_args
end = struct
  module Default = Using_deriving_args.Default

  let%expect_test "no arguments: flag defaults to [false], args [name] and [count] to \
                   absent"
    =
    print_s Default.t_sexp_args;
    [%expect
      {|
      ((flag false)
       (name  ())
       (count ()))
      |}]
  ;;

  module With_flag = Using_deriving_args.With_flag

  let%expect_test "a flag is [true] when present" =
    print_s With_flag.t_sexp_args;
    [%expect
      {|
      ((flag true)
       (name  ())
       (count ()))
      |}]
  ;;

  module With_args = Using_deriving_args.With_args

  let%expect_test "arguments are correctly matched" =
    print_s With_args.t_sexp_args;
    [%expect
      {|
      ((flag false)
       (name  (foo))
       (count (3)))
      |}]
  ;;

  module With_everything = Using_deriving_args.With_everything

  let%expect_test "flags and arguments combine" =
    print_s With_everything.t_sexp_args;
    [%expect
      {|
      ((flag true)
       (name  (foo))
       (count (3)))
      |}]
  ;;
end
