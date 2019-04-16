open OUnit2;;

(*--- helpers ---*)
let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

(*--- test data ---*)
let largeList = 0 -- 10000000;;
let smallList = 0 -- 1000;;
let smallListRev = 1000 -- 0;;
(*--- test functions ---*)
let test1_1 test_ctxt = assert_equal (Some 3) (Ocaml99.my_last [1;2;3]) ;;
let test1_2 test_ctxt = assert_equal None (Ocaml99.my_last2 []) ;;
let test2 test_ctxt = assert_equal (Some ("c","d")) (Ocaml99.my_last2 ["a" ; "b" ; "c" ; "d"]);;
let test3 test_ctxt = assert_equal (Some 99999) (Ocaml99.my_kth 99999 largeList);;
let test4_1 test_ctxt = assert_equal 1001 (Ocaml99.my_llen smallList);;
let test4_2 test_ctxt = assert_equal 10000001 (Ocaml99.my_llen2 largeList);;
let test5_1 test_ctxt = assert_equal []  (Ocaml99.my_rev []);;
let test5_2 test_ctxt = assert_equal [1]  (Ocaml99.my_rev [1]);;
let test5_3 test_ctxt = assert_equal [1;2;3]  (Ocaml99.my_rev [3;2;1]);;
let test6_1 test_ctxt = assert_equal true (Ocaml99.my_palindrome []);;
let test6_2 test_ctxt = assert_equal true (Ocaml99.my_palindrome [1]);;
let test6_3 test_ctxt = assert_equal false (Ocaml99.my_palindrome [1;2;1;2]);;
let test7_1 test_ctxt = assert_equal [] (Ocaml99.my_flatten []);;
let test7_1 test_ctxt = assert_equal ["a";"b";"c";"d"] (Ocaml99.my_flatten  [ Ocaml99.One "a" ; Ocaml99.Many [Ocaml99.One "b" ; Ocaml99.Many [ Ocaml99.One "c" ; Ocaml99.One "d" ]]]);;
let test7_2 test_ctxt = assert_equal [1;2] (Ocaml99.my_flatten [Ocaml99.One 1; Ocaml99.One 2]);;
let test8 test_ctxt = assert_equal ["a";"b";"c";"a"] (Ocaml99.remove_consecutive_duplicates ["a";"a";"a";"a";"b";"c";"c";"a"]);;
let test9 test_ctxt = assert_equal [["a"; "a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]] (Ocaml99.pack_duplicates ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]);;
let test10_1 test_ctxt = assert_equal [] (Ocaml99.rlc []);;
let test10_2 test_ctxt = assert_equal [("a",4); ("b",1); ("c",2); ("a",2); ("d",1); ("e",4)] (Ocaml99.rlc  ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;
let test11 test_ctxt = assert_equal [Ocaml99.Plural ("a",2); Ocaml99.Single "b"; Ocaml99.Plural ("c",2); Ocaml99.Plural ("a",2)] (Ocaml99.mrlc ["a";"a";"b";"c";"c";"a";"a"]);;
let test12 test_ctxt = assert_equal ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] (Ocaml99.decode_mrlc [Ocaml99.Plural ("a",4); Ocaml99.Single "b"; Ocaml99.Plural ("c",2); Ocaml99.Plural ("a",2); Ocaml99.Single "d"; Ocaml99.Plural ("e",4)]);;
(* Name the test cases and group them together *)
let suite =
"suite-A" >:::
 ["test1-1" >:: test1_1;
  "test1-2" >:: test1_2;
  "test2" >:: test2;
  "test3" >:: test3;
  "test4_1" >:: test4_1;
  "test4_2" >:: test4_2;
  "test5_1" >:: test5_1;
  "test5_2" >:: test5_2;
  "test5_3" >:: test5_3;
  "test6_1" >:: test6_1;
  "test6_2" >:: test6_2;
  "test6_3" >:: test6_3;
  "test7_1" >:: test7_1; 
  "test7_2" >:: test7_2;
  "test8" >:: test8;
  "test9" >:: test9;
  "test10_1" >:: test10_1;
  "test10_2" >:: test10_2;
  "test11" >:: test11;
  "test12" >:: test12;
 ]
;;

let () =
  run_test_tt_main suite
;;

