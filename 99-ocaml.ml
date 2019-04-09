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
let rec my_llen ll = match ll with
| [] -> 0
| _ :: rest -> 1 + my_llen rest

let rec my_llen2 k = function
| [] -> k
| _ :: rest -> my_llen2 (k+1) rest

(*
let in_channel = open_in "/usr/lib/ocaml/unix.mli" in
try
	while true do
		  let line = input_line in_channel in
		  	  print_endline line
		  done
with End_of_file -> close_in in_channel		  

let line_stream_of_channel channel =
    Stream.from
    (fun _ ->
       try Some (input_line channel) with End_of_file -> None);;
 *)
             
let in_channel = open_in "/usr/lib/ocaml/arg.ml";;
let line = line_stream_of_channel in
	print_endline line

let f n = Sequence.(range 0 10 |> filter_map ~f:(fun x -> if x * x > 3 then Some (x + 1) else None)) ;;

