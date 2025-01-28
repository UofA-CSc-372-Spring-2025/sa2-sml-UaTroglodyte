(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Anthony Martin*)
(* Time spent on HW6: 2 hours
*)

(* Collaborators and references:
  I used the pre assesment reverse function, the included links in the spec,
  and some stack exchange forms to figure out syntax 
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false;

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true


(**** Problem B ****)

fun firstVowel (#"a" :: _) = true
  | firstVowel (#"e" :: _) = true
  | firstVowel (#"i" :: _) = true
  | firstVowel (#"o" :: _) = true
  | firstVowel (#"u" :: _) = true
  |firstVowel _ = false;

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

(**** Problem C ****)

fun reverse [] = []
  | reverse(x::xs) = reverse xs @ [x];

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

(**** Problem D ****)

fun minlist [] = raise Match
| minlist (x::xs) = foldl Int.min x xs;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

(**** Problem E ****)

exception Mismatch

fun zip ([], []) = []
  | zip (x :: xs, y :: ys) = (x, y) :: zip (xs::ys)
  | zip _ = raise Mismatch;

(**** Problem F ****)

fun concat [] = []
  | concat (x :: xs) = x @ concat xs;

(**** Problem G ****)

fun isDigit _  = false
  | isDigit (#'0') = true
  | isDigit (#'1') = true
  | isDigit (#'2') = true
  | isDigit (#'3') = true
  | isDigit (#'4') = true
  | isDigit (#'5') = true
  | isDigit (#'6') = true
  | isDigit (#'7') = true
  | isDigit (#'8') = true
  | isDigit (#'9') = true;

(**** Problem H ****)

fun isAlpha c = 
    val n = Char.ord c
    if n >= Char.ord #"a" andalso n <= Char.ord #"Z" then true
    else (
          false
    );

(**** Problem I ****)

fun svgCircle (cx, cy, r, fill) =
  print ("<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^ "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />")

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
