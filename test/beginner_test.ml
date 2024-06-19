open OUnit2
open Exercises.Beginner
open Exercises.Utils

let ae test_name expected got ~printer =
  test_name >:: fun _ -> assert_equal expected got ~printer

let last_tests =
  let ae_last = ae ~printer:string_of_int_option in
  "tests for last"
  >::: [ ae_last "empty_list" None (last [])
       ; ae_last "one_element" (Some 1) (last [ 1 ])
       ; ae_last "two_elements" (Some 2) (last [ 1; 2 ])
       ; ae_last "many_elements" (Some 109) (last [ 0; 2; 49; 21; 87; 42; 10; 109 ])
       ]

let last_two_test =
  let ae_last_two = ae ~printer:string_of_int_int_option in
  "tests for last_two"
  >::: [ ae_last_two "empty_list" None (last_two [])
       ; ae_last_two "one_element" None (last_two [ 1 ])
       ; ae_last_two "two_elements" (Some (1, 2)) (last_two [ 1; 2 ])
       ; ae_last_two
           "many_elements"
           (Some (10, 109))
           (last_two [ 0; 2; 49; 21; 87; 42; 10; 109 ])
       ]

let nth_el_tests =
  let ae_nth_el = ae ~printer:string_of_int_option in
  "tests for nth_el"
  >::: [ ae_nth_el "empty_list" None (nth_el 1 [])
       ; ae_nth_el "one_element_valid_idx" (Some 1) (nth_el 0 [ 1 ])
       ; ae_nth_el "one_element_invalid_idx" None (nth_el 5 [ 2 ])
         (* TODO: Write more tests*)
       ]

let length_tests =
  let ae_length = ae ~printer:string_of_int in
  "tests for length"
  >::: [ ae_length "empty_list" 0 (length [])
       ; ae_length "one_element" 1 (length [ 1 ])
       ; ae_length "two_elements" 2 (length [ 'x'; 'y' ])
       ; ae_length "three_elements" 3 (length [ 'x'; 'y'; 'z' ])
       ; ae_length "one_thousand_elements" 1000 (length (0 -- 999))
       ; ae_length "ten_thousand_elements" 10000 (length (0 -- 9999))
       ]

let rev_tests =
  let printer = string_of_list string_of_int in
  let ae_rev = ae ~printer in
  "test for rev"
  >::: [ ae_rev "empty_list" [] (rev [])
       ; ae_rev "one_element" [ 1 ] (rev [ 1 ])
       ; ae_rev "two_elements" [ 2; 1 ] (rev [ 1; 2 ])
       ]

let () =
  List.iter
    run_test_tt_main
    [ last_tests; last_two_test; nth_el_tests; length_tests; rev_tests ]
