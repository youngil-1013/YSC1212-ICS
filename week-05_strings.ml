(* week-05_strings.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2018-2019 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Tue 12 Feb 2019 *)

(* ********** *)

(* Exercise 5 *)

let test_to_a candidate =
  (candidate "" = "") &&
  (candidate "x" = "a") &&
  (candidate "xy" = "aa") &&
  (candidate "xyz" = "aaa")
  (* etc. *);;

let to_a s =
  String.map (fun c -> 'a') s;;

let () = assert (test_to_a to_a);;

(*
   # test_to_a to_a;;
   - : bool = true
   # 
*)

(* ***** *)

let test_upper candidate =
  (candidate "abc" = "ABC")
  (* etc. *);;

(*
let upper s =
  ...

let () = assert (test_upper upper);;
*)

(* ***** *)

let test_lower candidate =
  (candidate "ABC" = "abc")
  (* etc. *);;

(*
let lower s =
  ...

let () = assert (test_lower lower);;
*)

(* ********** *)

(* Exercise 6 *)

let test_opportunistic_upper candidate =
  (candidate "abc" = "ABC") &&
  (candidate "ABC" = "ABC") &&
  (candidate "Abc" = "ABC") &&
  (candidate "aBc" = "ABC") &&
  (candidate "abC" = "ABC")
  (* etc. *);;

(*
let opportunistic_upper s =
  ...

let () = assert (test_opportunistic_upper opportunistic_upper);;
*)

(* ***** *)

let test_opportunistic_lower candidate =
  (candidate "ABC" = "abc") &&
  (candidate "abc" = "abc") &&
  (candidate "Abc" = "abc") &&
  (candidate "aBc" = "abc") &&
  (candidate "abC" = "abc")
  (* etc. *);;

(*
let opportunistic_lower s =
  ...

let () = assert (test_opportunistic_lower opportunistic_lower);;
*)

(* ********** *)

let test_blam candidate =
  (candidate "" = "") &&
  (candidate "a" = "0") &&
  (candidate "ab" = "01") &&
  (candidate "abc" = "012") &&
  (candidate "abcd" = "0123")
  (* etc. *);;

let blam s =
  String.mapi (fun i c -> char_of_int (i + 48)) s;;

let () = assert (test_blam blam);;

let blam s =
  String.mapi (fun i c -> char_of_int (i + int_of_char '0')) s;;

let () = assert (test_blam blam);;

(* ********** *)

(* end of week-05_strings.ml *)

"week-05_strings.ml"
