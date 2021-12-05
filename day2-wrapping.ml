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

let input = get_lines "input/day2.txt" |> List.map 
	(fun str -> Scanf.sscanf str "%ix%ix%i" 
		(fun l w h -> l, w, h)
	)
;;

let wrapping lst =
	let surface l w h = 2*l*w + 2*w*h + 2*h*l +
		if l>=w && l>=h then w*h
		else if w>=l && w>=h then l*h
		else l*w
	and perimeter l w h = l*w*h +
		if l>=w && l>=h then 2 * (w+h)
		else if w>=l && w>=h then 2 * (l+h)
		else 2 * (l+w)
	in
	let rec _wrapping lst paper ribbon =
		match lst with
		| [] -> paper, ribbon
		| (l,w,h)::t -> _wrapping t ( paper+(surface l w h) )( ribbon+(perimeter l w h) )
	in
	_wrapping lst 0 0
;;

(* Problem 1 *)
let () = 
	let paper, ribbon = wrapping input in
	Printf.printf "Resultat pb1 : %d\nResultat pb2 : %d\n" paper ribbon
;;
