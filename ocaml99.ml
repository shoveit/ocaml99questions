(*----------------------------------------------------*)
(* 1. last element *)
let rec my_last ll  = match ll with
| [] -> None 
| [a] -> Some a
| _ :: rest -> my_last rest

(* val my_last : 'a list -> 'a option = <fun> *)

(*----------------------------------------------------*)
(* 2. last two elements *)
let rec my_last2 ll = match ll with
| [a;b] -> Some (a,b)
| _::rest -> my_last2 rest
| _ -> None 

(*----------------------------------------------------*)
(* 3. K-th element *)
let rec my_kth k = function (*--- Curry ---*)
| [] -> None
| a :: rest -> if k = 0 then Some a else my_kth (k-1) rest

(*----------------------------------------------------*)
(* 4. list length  *)
(* my_llen fail to return length of a integer list=range(1,10000000) *)             
let rec my_llen ll = match ll with
| [] -> 0
| _ :: rest -> 1 + my_llen rest

(* my_llen2 is ok with list=range(1,10000000) *)
let my_llen2 ll = 
  let rec _curry k = function
    | [] -> k
    | _ :: rest -> _curry (k+1) rest in
  _curry 0 ll
                 
(*----------------------------------------------------*)
(* 5. reverse list  *)
let my_rev ll =
  let rec _curry acc = function
    | [] -> acc
    | a::rest -> _curry (a::acc) rest in
  _curry [] ll


(*----------------------------------------------------*)
(* 6. palindrome list  *)
(* Assert "forward > backward" only happens when input list is []. by defination it's a palindrome *)
let my_palindrome ll =
  let rec _curry forward backward ll = match backward - forward with
    (*    | (fun x -> x < 0) -> true ???????????????? should be a idomatic grammer for this pattern   *)
    | 0 -> true
    | 1 -> if (List.nth ll forward) = (List.nth ll backward) then true else false
    | _ -> if forward > backward then true
           else  _curry (forward +1) (backward -1) ll
  in _curry 0 (List.length ll -1) ll                               

(*----------------------------------------------------*)
(* 7. flatten nested list  *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;
       
let rec my_flatten nl =
  let rec _curry acc = function 
  | [] -> acc
  | One  x :: rest ->  _curry (x :: acc) rest
  | Many x :: rest ->  _curry (_curry acc x) rest
  in List.rev (_curry [] nl) ;;

(*----------------------------------------------------*)
(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
let remove_consecutive_duplicates ll =
  let rec  _curry acc = function
    | []  -> acc
    | a :: rest -> if a = (List.hd acc) then _curry acc rest
                   else _curry (a::acc) rest in
  List.rev(_curry [List.hd ll] ll);;

remove_consecutive_duplicates ["a";"a";"a";"a";"b";"c";"c";"a"];; 

(*----------------------------------------------------*)
(* 9. Pack consecutive duplicates of list elements into sublists. (medium)*)
let pack_duplicates ll =
  let rec _curry acc = function
    | [] -> acc
    | a :: rest -> if List.hd (List.hd acc) = a then _curry ((a::(List.hd acc)) :: (List.tl acc)) rest
                   else _curry ([[a]] @ acc) rest
  in List.rev(_curry [[List.hd ll]] ll);;
