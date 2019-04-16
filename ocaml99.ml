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

(*----------------------------------------------------*)
(* 9. Pack consecutive duplicates of list elements into sublists. (medium)*)
let pack_duplicates ll =
  let rec _curry acc = function
    | [] -> acc
    | a :: rest ->  if List.hd (List.hd acc) = a
                    then _curry ((a :: List.hd acc) :: (List.tl acc)) rest
                    else _curry ([[a]] @ acc) rest
  in List.rev(_curry [[List.hd ll]] ll);;

(*----------------------------------------------------*)
(* 10. run length coding *)
let rlc ll =
  let rec _curry acc = function
    | [] -> acc
    | a :: rest -> let (x,count) = List.hd acc in
                   if x = a then _curry ((x,count+1) :: List.tl acc) rest
                   else _curry ((a,1) :: acc) rest
  in match ll with
     | [] -> []
     | a ::rest -> List.rev (_curry [(a,1)] rest) ;;

(*----------------------------------------------------*)
(* 11. modified run length coding *)
type 'a rle =
    | Single of 'a
    | Plural of 'a * int ;;


let mrlc ll =
  let rec _curry acc = function
    | [] ->  acc
    | x :: rest -> match (List.hd acc) with
                | Single e       -> if e = x 
                                    then _curry (Plural (e , 2) :: List.tl acc) rest
                                    else _curry (Single x :: acc) rest
                | Plural (e , c) -> if e = x
                                    then _curry (Plural (e , c+1) :: List.tl acc) rest
                                    else _curry (Single x :: acc) rest
  in match ll with
     | [] -> []
     | a::rest -> List.rev(_curry [Single a] rest) ;;

(*----------------------------------------------------*)
(* 12. decode a modified run length coding *)
let nx n x = 
  let rec _iter i acc =
    if i < n then _iter (i+1) (x::acc) else acc
    in _iter 0 [] ;;
  
let decode_mrlc ll =
  let rec _curry acc = function
    | [] -> acc
    | a::rest -> match a with
                 | Single x -> _curry (x :: acc) rest
                 | Plural (x,count) -> _curry ((nx count x) @ acc) rest
  in List.rev(_curry [] ll);;

(*----------------------------------------------------*)
(* 13. PASS:decode a modified run length coding *)

(*----------------------------------------------------*)
(* 14. duplicateList *)
let duplicateList ll =
  let rec _curry acc = function
    |[] -> acc
    |a::rest -> _curry ([a;a] @ acc) rest in
  List.rev(_curry [] ll);;


(* 15. replicate K times  *)
let replicateKtimes ll k =
  let rec ktimes k x acc =
    if k = 0 then acc else ktimes (k-1) x (x::acc) in
  let rec _curry acc = function
    |[] -> acc
    |a::rest -> _curry ((ktimes k a []) @ acc) rest in
  _curry [] ll;;


(* 16. drop every K element *)
let dropEveryKth ll k =
  let rec _curry count acc = function
    |[] -> List.rev(acc)
    |a::rest -> if count=k
                then _curry 1 acc rest
                else _curry (count+1) (a::acc) rest in
  _curry 1 [] ll;;
    
(* dropEveryKth [1;2;3;4;5;6;7] 1;; *)

(* 17. split a list into 2 parts, the length of the first part is given *)
let splitAtKth ll k =
  let rec _curry acc count = function
    |[] -> (List.rev(acc),[])
    |a::rest -> if count=0
                then (List.rev(acc),rest)
                else _curry (a::acc) (count-1) rest in
  _curry [] k ll;;

(* 18. extract a slice from list *)
(* --------- pull request :an imperative way *) 
type pos = Left | Inbetween | Right;;
let sliceI2J_imperative ll i j =
  let _pos min max x =
    if x < min then Left
    else if x >= max then Right 
    else Inbetween in
  let rec _curry acc x = function
    |[] -> acc
    |a::rest -> match  (_pos i j x)  with
                | Left -> _curry acc (x+1) rest
                | Right -> acc
                | Inbetween -> _curry (a::acc) (x+1) rest in
  List.rev(_curry [] 0 ll);;

(* from hereon, I decide to make stack allocate recursion rather than stack-free version, I believe Ocaml compiler will do it automatically , eventually *)
let sliceI2J ll i j =
  (* take from left end; not exactly take-n,  *)
  let rec take n = function
    |[] -> []
    |a::rest -> if n=0 then [] else a::(take (n-1) rest) in
  (* drop from left end *)
  let rec drop n = function
    |[] -> []
    |ll -> if n=0 then ll else drop (n-1) (List.tl ll) in
  drop i (take j ll);;

  
(* 19. Rotate a list N places to the left. (medium) *)
(* ---------pull request: *)
let rec rotateN2left n = function
  |[] -> []
  |a::rest -> if n=0 then (a::rest)
              else rotateN2left (n-1) (rest @ [a]);;

(* 20. Remove the K'th element from a list. (easy) *)
let rec removeKth k = function
  |[] -> []
  |a::rest -> if k > 0 then (a::(removeKth (k-1) rest))
              else  rest;;

(* 21. Insert an element at a given position into a list. (easy) *)
let rec insertAtK k x = function
  |[] -> [x]
  |a::rest -> if k > 0 then a::(insertAtK (k-1) x rest)
              else x::a::rest;;

(* 22. Create a list containing all integers within a given range. (easy) *)
let rec range min max =
  if min <= max then min:: range (min+1) max
  else [];;

(* 23. Extract a given number of randomly selected elements from a list. (medium) *)
(* assume Random.int is uniformly distributed, is it ?? *)
(* ----------pull request to ocaml.org *)
let rec randomChooseK k = function
  |[] -> []
  |(a::rest) as ll -> if k = 0 then []
                      else
                        if (Random.float 1.0) < (float k) /. (float (List.length ll))
                        then a:: randomChooseK (k-1) (rest @ [a])
                        else randomChooseK k (rest @ [a]);;

(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let drawNfromM n m = 
  let rec _curry n = function
    |[] -> []
    |a::rest -> if n = 0 then []
                else
                  if (Random.float 1.0) < (float n) /. (float (List.length (a::rest)))
                  then a:: _curry (n-1) rest
                  else _curry n (rest @ [a]) in
  _curry n (range 1 m);;
    
          
(* 25. Generate a random permutation of the elements of a list. (easy) *)
let randomPermutation_notSoRandom ll =
  let rec _curry k = function
    |[] -> []
    |[a] -> [a]
    |(a::b::rest) as ll -> if k = 0 then ll
                   else
                     if (Random.float 1.0) < 0.5
                     then _curry (k-1) (rest @ [a;b])
                     else _curry (k-1) (a::rest @ [b;a]) in
  _curry (10*(List.length ll)) ll;;

(* notice the _notSoRandom version can only make adjacent switch while the next one can do long-distance *)
let randomPermutation ll =
  let rec _curry k = function
    |[] -> []
    |[a] -> [a]
    |(a::b::rest) as ll -> if k = 0 then ll
                   else
                     if (Random.float 1.0) < 0.5
                     then _curry (k-1) (b::rest @ [a])
                     else _curry (k-1) (a::rest @ [b]) in
  _curry (10*(List.length ll)) ll;;

(* randomPermutation (range 1 20);; *)
