let witch_floor str =
	let rec _witch_floor str idx len acc =
		if idx = len then acc
		else match str.[idx] with
		| '(' -> _witch_floor str (idx+1) len (acc+1)
		| ')' -> _witch_floor str (idx+1) len (acc-1)
		| _   -> _witch_floor str (idx+1) len acc 
	in
	_witch_floor str 0 (String.length str) 0
;;

let when_basement str =
	let rec _when_basement str idx len acc =
		if acc = -1 then idx
		else if idx = len then failwith "never in basement"
		else match str.[idx] with
		| '(' -> _when_basement str (idx+1) len (acc+1)
		| ')' -> _when_basement str (idx+1) len (acc-1)
		| _   -> _when_basement str (idx+1) len acc
	in
	_when_basement str 0 (String.length str) 0
;;

(* Problem 1 *)
let () = input_line (open_in "input/day1.txt")
	  |> witch_floor
	  |> Printf.printf "Résultat pb1 : %d\n";;

(* Problem 2 *)
let () = input_line (open_in "input/day1.txt")
	  |> when_basement
	  |> Printf.printf "Résultat pb2 : %d\n";;
