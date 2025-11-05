open BoolexprLib.Main

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

let test_trace1 expr exp_result =
  (expr |> parse |> trace1) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "if true then true else false" true;;

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = test_trace1 "if true then true else false" True;;
