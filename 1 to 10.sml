
(* Solution to problem 1 *)
fun last [] = NONE
	| last [x] = SOME x
	| last (x :: xs) = last xs;

val ans1a = last [ "a" , "b" , "c" , "d" ];
val ans1b = last [];

(* Solution to problem 2 *)
fun last_two [] = NONE
	| last_two [x] = NONE
	| last_two [x, y] = SOME (x, y)
	| last_two (x :: xs) = last_two xs;

val ans2a = last_two [ "a" , "b" , "c" , "d" ];
val ans2b = last_two [ "a" ];

(* Solution to problem 3 *)
fun at idx xs =
	let 
		fun helper _ [] = NONE
			| helper 1 (y :: ys) = SOME y
			| helper n (y :: ys) = helper (n - 1) ys
	in
		helper idx xs
	end;;

val ans3a = at 3 ["a", "b", "c", "d"];
val ans3b = at 3 [ "a" ];

(* Solution to problem 4 (without foldl) *)
fun length xs = 
	let 
		fun helper [] acc = acc
			| helper (_ :: ys) acc = helper ys (acc + 1)
	in
		helper xs 0
	end;

val ans4aa = length ["a", "b", "c"];
val ans4ab = length [ ];

(* Solution to problem 4 (with foldl) *)
fun length2 xs = 
	List.foldl (fn (x, acc) => acc + 1) 0 xs;

val ans4ba = length2 ["a", "b", "c"];
val ans4bb = length2 [ ];

(* Solution to problem 5 *)
fun reverse xs = 
	let 
		fun helper [] acc = acc
			| helper (y :: ys) acc = helper ys (y :: acc)
	in
		helper xs []
	end;

val ans5a = reverse ["a" , "b", "c"];

(* Solution to problem 6 *)
fun is_palindrome xs =
	(reverse xs) = xs;

val ans6a = is_palindrome [ "x" , "a" , "m" , "a" , "x" ];
val ans6b = is_palindrome ["a", "b"];


(* Solution to problem 7 *)

datatype 'a node = 
	One of 'a 
	| Many of 'a node list 

fun flatten xs = 
	let
		fun helper (One(elem)) = [elem]
			| helper (Many(elems)) = flatten elems
 	in
		List.concat (List.map helper xs) 
	end;

val ans7a = flatten [ One "a" , Many [ One "b" , Many [ One "c" , One "d" ] , One "e" ] ];

(* Solution to problem 8 *)

fun compress xs = 
	let 
		fun helper x [] = [x]
			| helper y (z :: zs) = if y = z then (z :: zs) else (y :: z :: zs)
	in
		List.rev (List.foldl (fn (x, acc) => helper x acc) [] xs)
	end;

val ans8a = compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];

(* Solution to problem 9 *)

fun pack xs = 
	let 
		fun helper x ([], acc) = ([x], acc)
			| helper x ((y :: ys), acc) = 
				if x = y then
					((x :: y :: ys), acc)
				else
					([x], (y :: ys) :: acc)
	in
		let
			val (cont, acc) = (List.foldl (fn (x, container) => helper x container) ([], []) xs)
		in
			List.rev (cont :: acc)
		end
	end;

val ans9a = pack ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];


(* Solution to problem 10 *)

fun encode xs =
	List.map (fn (x :: xs) => (List.length (x :: xs), x)) (pack xs)

val ans10a = encode ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"];

















