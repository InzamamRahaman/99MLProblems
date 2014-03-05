
(* Problem 1 *)

fun last [] = NONE
	| last [x] = SOME x
	| last (x :: xs) = last xs;

fun last2 xs = 
	List.foldl (fn (x, _) => SOME x) NONE xs;

fun last3 [] = NONE
	| last3 xs = 
		SOME (List.hd (List.rev xs));

(* Problem 2 *)

fun lastTwo [] = NONE
	| lastTwo [x] = NONE
	| lastTwo [x, y] = SOME (x, y)
	| lastTwo (x :: xs) = lastTwo xs;

(* Problem 3 *)

fun at 1 [] = NONE
	| at 1 (x :: xs) = SOME x
	| at n (x :: xs) = at (n - 1) xs;


(* Problem 4 *)

fun length [] = 0
	| length (x :: xs) = 1 + (length xs);

fun length2 xs = 
		let
			fun helper [] acc = acc
				| helper (y :: ys) acc = helper ys (acc + 1)
		in
			helper xs 0
		end;

fun length3 xs = 
	List.foldl (fn (_, acc) => acc + 1) 0 xs;

(* Problem 5 *)

fun reverse xs = 
	let
		fun helper [] acc = acc
			| helper (y :: ys) acc = helper ys (y :: acc)
	in
		helper xs []
	end;

fun reverse2 xs = 
	List.foldl (fn (x, acc) => x :: acc) [] xs;

(* Problem 6 *)

fun isPalindrome xs = 
	(List.rev xs = xs);

(* Problem 7 *)

fun collect f xs = 
	List.concat (List.map f xs);

datatype 'a node = 
	One of 'a 
	| Many of 'a node list;



fun flatten xs =
	let
		fun helper (One(y)) = [y]
			| helper (Many(ys)) = collect helper ys
	in
		collect helper xs
	end;



(* Problem 8 *)

fun dropWhile f [] = []
	| dropWhile f (x :: xs) = 
		if (f x) = true then dropWhile f xs
		else x :: xs;

fun compress [] = []
	| compress (x :: xs) = 
		x :: (compress (dropWhile (fn y => y = x) xs));

(* Problem 9 *)

fun takeWhile f xs = 
	let
		fun helper [] acc = (List.rev acc, [])
			| helper (y :: ys) acc = 
				if (f y) = true then helper ys (y :: acc)
				else (List.rev acc, y :: ys)
	in
		helper xs []
	end;

fun pack [] = []
	| pack (x :: xs) = 
		let
			val (head, rest) = takeWhile (fn y => y = x) xs
		in
			(x :: head) :: (pack rest)
		end;


(* Problem 10 *)

fun encode1 xs = 
	List.map (fn ys => (List.hd ys, List.length ys)) (pack xs);

(* Problem 11 *)

datatype 'a encoding = 
	Single of 'a
	| Mutliple of 'a * int;


fun listToEncoding xs = 
	if (List.length xs) = 1 then
		Single(List.hd xs)
	else 
		Mutliple(List.hd xs, List.length xs);

fun encode2 xs = 
	List.map listToEncoding (pack xs);


(* Problem 12 *)

fun replicate 0 x = []
	| replicate n x = x :: (replicate (n - 1) x)

fun encodingToList (Single(x)) = [x]
	| encodingToList (Mutliple(x, n)) = replicate n x;

fun decode xs = 
	collect encodingToList xs;


(* Problem 13 *)

fun countWhile f [] = 0
	| countWhile f (x :: xs) =
		case (f x) of
			true => 1 + (countWhile f xs)
			| false => 0

fun numTupleToEncoding ((x, 1)) = Single(x)
	| numTupleToEncoding ((x, n)) = Mutliple(x, n);

fun encode3 [] = []
	| encode3 (x :: xs) = 
		let
			fun equals y = x = y
			val (count, rest) = ((countWhile equals (x :: xs)), (dropWhile equals xs)) 
		in
			(numTupleToEncoding (x, count)) :: encode3 rest
		end;

(* Problem 14 *)

fun duplicate xs = 
	collect (fn x => [x, x]) xs;

(* Problem 15 *)

(* Solved as part of problem 12's solution *)

(* Problem 16 *)

fun dropByIdx pred f idx [] = []
	| dropByIdx pred f idx (x :: xs) =
		case (pred idx) of
			true => dropByIdx pred f (f idx) xs
			| pattern2 => x :: (dropByIdx pred f (f idx) xs);

fun drop xs n = 
	dropByIdx (fn idx => idx mod n = 0) (fn idx => idx + 1) 1 xs;

(* Problem 17 *)

fun split xs n = 
	let
		fun helper 0 ys acc1  = (List.rev acc1, ys)
			| helper n [] acc1 = (List.rev acc1, [])
			| helper n (y :: ys) acc1 = helper (n - 1) ys (y :: acc1)
	in
		helper n xs []
	end;


(* Problem 18 *)

fun slice xs start fin = 
	split (split xs (start - 1)) (fin - start + 1);

fun slice xs start fin = 
	let
		val (_, rest) = split xs (start - 1)
		val (ans, _) = split rest (fin - start + 1)
	in
		ans
	end;

(* Problem 19 *)

fun rotate xs num = 
	let
		val (start, fin) = 
			if num > 0 then (split xs num)
			else (split xs ((List.length xs) + num))
	in
		fin @ start
	end;


(* Problem 20 *)

fun removeKth xs k = 
	dropByIdx (fn idx => idx  = k) (fn idx => idx + 1) 1 xs;

(* Problem 21 *)









