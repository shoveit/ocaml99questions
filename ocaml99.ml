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

(* 26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)

let rec extract k list =
  if k <= 0 then [[]]
  else match list with
       | [] -> []
       | h :: tl ->
          let with_h = List.map (fun l -> h :: l) (extract (k-1) tl) in
          let without_h = extract k tl in
          with_h @ without_h;;

let rec kombination k ll = match ll with
    |[] -> [[]] 
    |a::rest -> if k <= 0 then [[]]
                else List.filter (fun x -> List.length x = k) (* If run without filter, kombination will return a lot of list shorter than K, because of the uncompleted list concatenated when ll=[] && K>0 *)
                       (kombination k rest) @ (List.map (fun x -> a::x) (kombination (k-1) rest)) ;;

(*
'extract' and 'kombination' are similar but notice the subtle difference:
kombination explicitly Fliter out shorter list in the result.
Why there're shorter lists? because the default exit on ll = [] is [[]], 
it ensures there's always a exit return even K>0 at the time, that return will be concatenate to its prefix part...---> shorter return than K!!

Why 'extract' doesn't have explicit Filter? exit return [] is the key!
For the same reason 'kombination' keeps shorter returns, 'extract' will filter out shorter return implicitly with List.map to a empty list.

'kombination 5 [1;2]' return [[1;2]] but 'extract 5 [1;2]' return [].
*)



(* 27. Group the elements of a set into disjoint subsets. (medium) *)
let mapi lambda ls =
  let rec _curry i  = function
    |[] -> []
    |a::rest -> (lambda i a) :: _curry (i+1) rest in _curry 0 ls 

let partition  ll sizes =
  let ps = List.map (fun x -> (x,[]) ) sizes in
  let rec add2kthPartition k x = function
    |[] -> []
    |(cnt, plist) ::rest -> if k = 0
                            then (cnt-1,(x::plist)) :: rest
                            else (cnt, plist) :: add2kthPartition (k-1) x rest in
  let rec expand2allPartition x ps = function
    |(-1) -> []
    |k -> (add2kthPartition k x ps) ::  expand2allPartition x ps (k-1) in
  let rec _curry pslist = function
    |[] -> pslist
    |a::rest -> _curry  (List.concat (List.map  (fun ps -> expand2allPartition a ps) pslist)) rest in
  _curry [ps] ll ;; 
      
      
  let group list sizes =
      let initial = List.map (fun size -> size, []) sizes in
  
      let prepend p list =
        let emit l acc = l :: acc in
        let rec aux emit acc = function
          | [] -> emit [] acc
        | (n,l) as h :: t ->
           let acc = if n > 0 then emit ((n-1, p::l) :: t) acc
                     else acc in
           aux (fun l acc -> emit (h :: l) acc) acc t
      in
       aux emit [] list
    in
    let rec aux = function
      | [] -> [ initial ]
      | h :: t -> List.concat (List.map (prepend h) (aux t))
    in
    let all = aux list in
    (* Don't forget to eliminate all group sets that have non-full
       groups *)
    let complete = List.filter (List.for_all (fun (x,_) -> x = 0)) all in
     List.map (List.map snd) complete;;                          

(*-----------------*)
