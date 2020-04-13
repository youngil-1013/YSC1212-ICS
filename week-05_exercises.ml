(* week-05_exercise1.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2019-2020 *)
(* Zhao Yuan, Sewen, Joy, Young *)
(* Version of Mon 17 Feb 2020 *)

(* Exercise 1 *)
(* Implementing lower to upper *)
(* Always start by defining a unit test. *)
let test_lower_to_upper_v0 candidate =
  let b1 = (candidate "abcdef" = "ABCDEF")
  and b2 = (candidate "jhgklmnop" = "JHGKLMNOP")
  and b3 = (candidate "iqsruvt" = "IQSRUVT" )
  and b4 = (candidate "zywx" = "ZYWX")
  in b1 && b2 && b3 && b4;;

(* Sanity Testing *)
(* This uses a pre-made OCaml witness function. *)
let () = assert (test_lower_to_upper_v0 String.uppercase_ascii);;

(* Lucidity Testing *)
(* Since our longest test string is 10 characters, we can check the length for our mischevious function! *)
let () = assert (test_lower_to_upper_v0 (fun x -> if (String.length x) < 10 then String.uppercase_ascii x else "abc"));;

(* Strengthen our unit test *)
(* String.make is a simple function to make duplicate characters of n length so it is best for us. *)
let test_lower_to_upper_v1 candidate =
  let b1 = (candidate "abcdef" = "ABCDEF")
  and b2 = (candidate "jhgklmnop" = "JHGKLMNOP")
  and b3 = (candidate "iqsruvt" = "IQSRUVT" )
  and b4 = (candidate "zywx" = "ZYWX")
  and b5 = (let x = Random.int 90 + 10
            in candidate (String.make x 'a') = String.make x 'A')
  in b1 && b2 && b3 && b4 && b5;;

let () = assert ((test_lower_to_upper_v1 (fun x -> if (String.length x) < 10 then String.uppercase_ascii x else "abc"))=false);;

(* Helper function--Defining char to uppercase function *)
(* As always let's define a small unit test for it *)
let test_char_to_upper candidate =
  let b1 = (candidate 'a' = 'A')
  and b2 = (candidate 'b' = 'B')
  and b3 = (candidate 'c' = 'C')
  and b4 = (candidate 'z' = 'Z')
  and b5 = (let x = Random.int 26
            in candidate (String.get "abcdefghijklmnopqrstuvwxzy" x) = String.get "ABCDEFGHIJKLMNOPQRSTUVWXZY" x)
  in b1 && b2 && b3 && b4 && b5;;

(* Sanity testing *)
let () = assert (test_char_to_upper Char.uppercase_ascii);;
(* As this is char function for lowercase alphabetical order only, this test case is complete and no mischevious function is needed to be defined! *)

let char_to_upper_v1 c =
  char_of_int ((int_of_char c) - 32);;

let () = assert (test_char_to_upper char_to_upper_v1);;

(* Shielding the function from uppercase input and other inputs e.g. symbols *)
let char_to_upper_v2 c =
  let () = assert (int_of_char 'a' <= int_of_char c  && int_of_char c <= int_of_char 'z')
  in char_of_int ((int_of_char c) - 32);;

let () = assert (test_char_to_upper char_to_upper_v2);;

(* Utilizing OCaml polymorphic comparators *)
let char_to_upper_v3 c =
  let () = assert (c >= 'a' && c <= 'z')
  in char_of_int ( int_of_char c - 32 );;

let () = assert (test_char_to_upper char_to_upper_v3);;

(* Making it more readable for ourselves and others *)
let char_to_upper_v4 c =
  let () = assert (c >= 'a' && c <= 'z')
  in char_of_int ( int_of_char c -  (int_of_char 'a' - int_of_char 'A') );;

let () = assert (test_char_to_upper char_to_upper_v4);;

(* Mapping it to the main lower to upper function *)
let lower_to_upper_v1 s =
  String.map char_to_upper_v4 s ;;

let () = assert (test_lower_to_upper_v1 lower_to_upper_v1);;
(****************)

(* Implementing upper to lower--first thing first, unit test *)
let test_upper_to_lower_v0 candidate =
  let b1 = (candidate "ABCDEF" = "abcdef")
  and b2 = (candidate "JHGKLMNOP" = "jhgklmnop")
  and b3 = (candidate "IQSRUVT" = "iqsruvt" )
  and b4 = (candidate "ZYWX" = "zywx")
  in b1 && b2 && b3 && b4;;

(* Sanity Testing *)
let () = assert (test_upper_to_lower_v0 String.lowercase_ascii);;

(* Lucidity Testing *)
let () = assert (test_upper_to_lower_v0 (fun x -> if (String.length x) < 10 then String.lowercase_ascii x else "ABC"));;

(* Strengthen unit test *)
let test_upper_to_lower_v1 candidate =
  let b1 = (candidate "ABCDEF" = "abcdef")
  and b2 = (candidate "JHGKLMNOP" = "jhgklmnop")
  and b3 = (candidate "IQSRUVT" = "iqsruvt" )
  and b4 = (candidate "ZYWX" = "zywx")
  and b5 = (let x = Random.int 90 + 10
            in candidate (String.make x 'A') = String.make x 'a')
  in b1 && b2 && b3 && b4 && b5;;

let () = assert ((test_upper_to_lower_v1 (fun x -> if (String.length x) < 10 then String.lowercase_ascii x else "ABC"))=false);;

(* Helper function definition--Again let's start with a unit test *)
let test_char_to_lower candidate =
  let b1 = (candidate 'A' = 'a')
  and b2 = (candidate 'B' = 'b')
  and b3 = (candidate 'C' = 'c')
  and b4 = (candidate 'Z' = 'z')
  and b5 = (let x = Random.int 26
            in candidate (String.get "ABCDEFGHIJKLMNOPQRSTUVWXZY" x) = String.get "abcdefghijklmnopqrstuvwxzy" x)
  in b1 && b2 && b3 && b4 && b5;;
(* Sanity Testing *)
let () = assert (test_char_to_lower Char.lowercase_ascii);;
(* Again no lucidity test needed as the unit test is completed if the function is shielded. *)

(* Adapting from our previous char to upper function *)
let char_to_lower_v1 c =
  let () = assert (c >= 'A' && c <= 'Z')
  in char_of_int ( int_of_char c +  (int_of_char 'a' - int_of_char 'A') );;

let () = assert (test_char_to_lower char_to_lower_v1);;

(* Implementing the mapping for string *)
let upper_to_lower_v1 s =
  String.map char_to_lower_v1 s ;;

let () = assert (test_upper_to_lower_v1 upper_to_lower_v1);;

(* ********** *)

(* end of week-05_exercise1.ml *)

"week-05_exercise1.ml" 
