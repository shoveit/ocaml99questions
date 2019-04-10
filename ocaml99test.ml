open OUnit2;;

let test1_1 test_ctxt = assert_equal 3 (99-ocaml.my_last [1;2;3]);;
let test1_2 test_ctxt = assert_equal None (99-ocaml.my_last2 []);;


(* Name the test cases and group them together *)
let suite =
"suite-A" >:::
 ["test1-1" >:: test1_1;
  "test1-2" >:: test1_2]
;;

let () =
  run_test_tt_main suite
;;


