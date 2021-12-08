#load "str.cma";;

let input = "iwrupvqb";;


let brutforce1 input =
	let rec _brutforce1 input i =
		if (  Digest.string (input ^ (string_of_int i))
		   |> Digest.to_hex 
		   |> Str.string_match (Str.regexp "00000")
		   ) 0
		then i
		else _brutforce1 input (i+1)
	in
	_brutforce1 input 0
;;

let brutforce2 input =
	let rec _brutforce2 input i =
		if (  Digest.string (input ^ (string_of_int i))
		   |> Digest.to_hex 
		   |> Str.string_match (Str.regexp "000000")
		   ) 0
		then i
		else _brutforce2 input (i+1)
	in
	_brutforce2 input 0
;;

let () = Printf.printf "Résultat pb1 : %d\n" @@ brutforce1 input;;
let () = Printf.printf "Résultat pb2 : %d\n" @@ brutforce2 input;;