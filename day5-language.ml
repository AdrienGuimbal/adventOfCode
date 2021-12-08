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

let is_nice word =
	let is_voyel c = match c with 
		| 'a' | 'e' | 'i' | 'o' | 'u' -> 1
		| _ -> 0
	and len = String.length word in 
	let rec _is_nice idx voyels double =
		if idx = len then voyels >= 3 && double else
		match word.[idx-1], word.[idx] with
		| 'a','b' | 'c','d' | 'p','q' | 'x','y' -> false
		| _ -> _is_nice (idx+1) (voyels + is_voyel word.[idx]) (if word.[idx] = word.[idx-1] then true else double)
	in
	_is_nice 1 (is_voyel word.[0]) false
;;

let is_nice_new_and_improved word =
	let len = String.length word in
	let rec contain pair i =
			if i = len-1 then false
			else if pair = (word.[i], word.[i+1]) then true
			else contain pair (i+1)
	and double_pair i =
		if i = len-3 then false
		else if contain (word.[i], word.[i+1]) (i+2) then true
		else double_pair (i+1)
	in
	let rec sandwich i =
		if i = len-1 then false
		else if word.[i-1] = word.[i+1] then true
		else sandwich (i+1)
	in
	sandwich 1 && double_pair 0
;;


let rec nice_words criter words acc =
	match words with
	| [] -> acc
	| w::t -> nice_words criter t (acc + if criter w then 1 else 0)
;;

(* Problem 1 *)
let () = 
	Printf.printf "Resultat pb1 : %d\n" @@ nice_words is_nice (get_lines "input/day5.txt") 0;
	Printf.printf "Resultat pb2 : %d\n" @@ nice_words is_nice_new_and_improved (get_lines "input/day5.txt") 00
;;
