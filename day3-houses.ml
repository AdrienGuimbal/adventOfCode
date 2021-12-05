module IntPairs = struct 
	type t = int*int
	let compare (a,b) (c,d) =
		if a-c <> 0 then a-c
		else b-d
end

module Set_PairInt = Set.Make(IntPairs);;

let get_line filename =
	let file = open_in filename in
	let line = input_line file in
	close_in_noerr file;
	line
;;

let santa_visit direc =
	let nb_direc = String.length direc in
	let rec _visit_houses idx x y visited =
		if idx = nb_direc then Set_PairInt.cardinal visited
		else match direc.[idx] with
		| '^' -> _visit_houses ( idx+1 )(  x  )( y+1 )(  Set_PairInt.add (x,y) visited )
		| 'v' -> _visit_houses ( idx+1 )(  x  )( y-1 )(  Set_PairInt.add (x,y) visited )
		| '>' -> _visit_houses ( idx+1 )( x+1 )(  y  )(  Set_PairInt.add (x,y) visited )
		| '<' -> _visit_houses ( idx+1 )( x-1 )(  y  )(  Set_PairInt.add (x,y) visited )
		| _ -> failwith "WTF is input??"
	in
	_visit_houses 0 0 0 Set_PairInt.empty
;;

let santa_and_robot_visit direc =
	let nb_direc = String.length direc
	and mean_dirX dir =
		match dir with
		| '>' -> 1
		| '<' -> -1
		| _ -> 0
	and mean_dirY dir =
		match dir with
		| '^' -> 1
		| 'v' -> -1
		| _ -> 0
	in
	let rec _visit_houses idx xS yS xR yR visited =
		if idx = nb_direc then Set_PairInt.cardinal visited
		else _visit_houses ( idx+2 )
		    ( xS + mean_dirX direc.[ idx ] )
			( yS + mean_dirY direc.[ idx ] )
		    ( xR + mean_dirX direc.[idx+1] )
			( yR + mean_dirY direc.[idx+1] )
			(  Set_PairInt.add (xS,yS) visited |> Set_PairInt.add (xR,yR) )
	in
	_visit_houses 0 0 0 0 0 Set_PairInt.empty
;;

(* Problem 1 *)
let () = Printf.printf "Resultat pb1 : %d\n" @@ santa_visit (get_line "input/day3.txt");;
let () = Printf.printf "Resultat pb2 : %d\n" @@ santa_and_robot_visit (get_line "input/day3.txt");;
