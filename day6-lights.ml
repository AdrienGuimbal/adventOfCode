let get_lines filename =
	let rec _get_lines file acc =
		try
			let line = input_line file in
			_get_lines file @@ line::acc
		with e ->
			close_in_noerr file;
			List.rev acc
	in
	_get_lines (open_in filename) []
;;

(* let input = get_lines "input/day2.txt" |> List.map 
	(fun str -> Scanf.sscanf str "%ix%ix%i" 
		(fun l w h -> l, w, h)
	)
;; *)
type instruct = Toggle | On | Off;;

let rec load_instructions input (acc : (instruct*int*int*int*int) list) =
	match input with
	| [] -> List.rev acc
	| h::t -> load_instructions t ( (
		if Str.string_match (Str.regexp "turn on.*") h 0 then
			Scanf.sscanf h "turn on %i,%i trougth %i,%i" (fun a b c d -> On, a, b, c, d)
		else if Str.string_match (Str.regexp "turn off.*") h 0 then
			Scanf.sscanf h "turn off %i,%i trougth %i,%i" (fun a b c d -> Off, a, b, c, d)
		else if Str.string_match (Str.regexp "toggle.*") h 0 then
			Scanf.sscanf h "toggle %i,%i trougth %i,%i" (fun a b c d -> Toggle, a, b, c, d)
		else failwith "WTF"
		)::acc )
;;

let rec execute arr instruct =
	match instruct with
	| [] -> ()
	| (On,a,b,c,d)::t -> (
		for i = a to c do
			for j = b to d do
				Array.set (Array.get arr i) j 1
			done
		done; execute arr t )

	| (Off,a,b,c,d)::t -> (
		for i = a to c do
			for j = b to d do
				Array.set (Array.get arr i) j 0
			done
		done; execute arr t )
	
	| (Toggle,a,b,c,d)::t -> (
		for i = a to c do
			for j = b to d do
				Array.set (Array.get arr i) j (if (Array.get (Array.get arr j) i) = 1 then 0 else 1)
			done
		done; execute arr t )
;;

let sum arr =
	let s = ref 0 in
	for i = 0 to (Array.length arr)-1 do
		for j = 0 to (Array.length arr)-1 do
			(s := (!s + (Array.get (Array.get arr j) i)))
		done
	done;
	!s
;;

(* Problem 1 *)
let () =
	let input = load_instructions (get_lines "input/day6.txt") in
	let lights = Array.make 1000 (Array.make 1000 0)  in
	execute lights input;
	Printf.printf "Resultat pb1 : %d\n" @@ sum (lights);
	Printf.printf "Resultat pb2 : %d\n" 0
;;
