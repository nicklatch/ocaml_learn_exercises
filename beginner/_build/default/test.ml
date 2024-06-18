open OUnit2
open Beginner

let ae test_name want ~fn_to_test input =
  test_name >:: fun _ -> assert_equal want (fn_to_test input)

let last_tests =
  let ae_last = ae ~fn_to_test:last in
  "tests for last"
  >::: [ ae_last "empty_list" None []
       ; ae_last "one_element" (Some 1) [ 1 ]
       ; ae_last "two_elements" (Some 2) [ 1; 2 ]
       ]

let last_two_test =
  let ae_last_two = ae ~fn_to_test:last_two in
  "tests for last_two"
  >::: [ ae_last_two "empty_list" None []
       ; ae_last_two "one_element" None [ 1 ]
       ; ae_last_two "two_elements" (Some (1, 2)) [ 1; 2 ]
       ; ae_last_two "many_elements" (Some (10, 109)) [ 0; 2; 49; 21; 87; 42; 10; 109 ]
       ]

let () =
  let all_tests = [ last_tests; last_two_test ] in
  List.iter run_test_tt_main all_tests
