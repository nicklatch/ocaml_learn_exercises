open OUnit2
open Beginner

let string_of_int_option = function
  | None -> "None"
  | Some v -> "Some " ^ string_of_int v

let string_of_int_int_option = function
  | None -> "None"
  | Some (v1, v2) -> "Some (" ^ string_of_int v1 ^ ", " ^ string_of_int v2 ^ ")"

let ae test_name expected ~fn_to_test input ~printer =
  test_name >:: fun _ -> assert_equal expected (fn_to_test input) ~printer

let last_tests =
  let ae_last = ae ~fn_to_test:last ~printer:string_of_int_option in
  "tests for last"
  >::: [ ae_last "empty_list" None []
       ; ae_last "one_element" (Some 1) [ 1 ]
       ; ae_last "two_elements" (Some 2) [ 1; 2 ]
       ]

let last_two_test =
  let ae_last_two = ae ~fn_to_test:last_two ~printer:string_of_int_int_option in
  "tests for last_two"
  >::: [ ae_last_two "empty_list" None []
       ; ae_last_two "one_element" None [ 1 ]
       ; ae_last_two "two_elements" (Some (1, 2)) [ 1; 2 ]
       ; ae_last_two "many_elements" (Some (10, 109)) [ 0; 2; 49; 21; 87; 42; 10; 109 ]
       ]

let () =
  let all_tests = [ last_tests; last_two_test ] in
  List.iter run_test_tt_main all_tests
