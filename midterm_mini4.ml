(* midterm_mini4.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2019-2020 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Was version of Fri 13 Mar 2020 *)
(* Version of Wed 18 Mar 2020 *)

(* ********** *)

(* name: Young Il Kim
   email address: youngil.kim@u.yale-nus.edu.sg
   student number: A0207809Y

   other members of the group:
   name: Sewen Thy
   name: Kao Zhao Yuan
   name: Peng Chiao Yin
*)

let parafold_right_nat zero_case succ_case n_given =
 (* parafold_right_nat : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
         in let ih = visit n'
            in succ_case n' ih    (* <-- succ_case takes two arguments *)
  in visit n_given;;

(* EDITTED FOR A MORE EFFICIENT VERSION *)
let fib n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then 0
    else (if n = 1
          then 1
          else visit (n - 2) + visit (n - 1))
  in visit n_given;;

let summation f n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then f (0)
    else let n' = n - 1
         in let ih = visit n'
            in ih + f (succ n')
  in visit n_given;;


module Stringy =
  struct
    let mapi f s =
      let i = ref ~-1
      in String.map (fun c ->
                      i := succ !i;
                      f !i c)
                    s
    (* ***** *)

    (*
       val init : int -> (int -> char) -> string
       String.init n f returns a string of length n, with character i initialized to the result of f i (called in increasing index order).
       Raise Invalid_argument if n < 0 or n > Sys.max_string_length.
       Since 4.02.0
    *)
    let init n f =
      if 0 <= n && n <= Sys.max_string_length
      then mapi (fun i _ -> f i) (String.create n)
      else raise (Invalid_argument "String.create");;
  end;;

(* Question 1 *)

let test_append candidate =
  let b0 = (candidate "" "" = "")
  and b1 = (candidate "a" "b" = "ab")
  and b2 = (candidate "cat" "fish" = "catfish")
  and b3 = let a = Char.escaped (char_of_int (Random.int 255))
           and b = Char.escaped (char_of_int (Random.int 255))
           in (candidate a b = a ^ b)
  in b0 && b1 && b2 && b3;;

let () = assert (test_append (^));;

(* DELETED A REDUNDANT IF-STATEMENT *)
let string_append s1 s2 =
  let len_s1 = String.length s1
  and len_s2 = String.length s2
  in if len_s1 = 0 then s2
     else if len_s2 = 0 then s1
     else
       Stringy.init (len_s1 + len_s2) (fun i ->
	   if i < len_s1 then (String.get s1 i)
	   else (String.get s2 (i - len_s1)));;

let () = assert (test_append string_append);;

(* Question 2 *)

let test_reverse candidate =
  let b0 = (candidate "" = "")
  and b1 = (candidate "a" = "a")
  and b2 = (candidate "ab" = "ba")
  and b3 = (candidate "abc" = "cba")
  and b4 = (candidate (candidate "cake") = "cake")
  in b0 && b1 && b2 && b3 && b4;;

(* EDITTED TO ONLY USE STRINGY.INIT *)
let string_reverse_v0 s =
  let result = Stringy.init (String.length s)(fun i -> ' ')
  in Stringy.mapi (fun i c -> String.get s (String.length s - i - 1)) result;;

let () = assert(test_reverse string_reverse_v0);;

let string_reverse_v1 s =
  Stringy.mapi (fun i c -> String.get s (String.length s - i - 1)) s;;

let () = assert(test_reverse string_reverse_v1);;

let string_reverse_v2 s =
   Stringy.init (String.length s)(fun i -> String.get s (String.length s - i - 1));;

let () = assert(test_reverse string_reverse_v2);;

(* EDITTED TO INCLUDE LAST_INDEX *)
let string_reverse_recurse s =
  let last_index = String.length s - 1 in
  let rec visit n =
    if n = 0 then ""
    else visit (pred n) ^ Char.escaped (String.get s (last_index - pred n))
  in visit (String.length s);;

let () = assert(test_reverse string_reverse_recurse);;

(* THE PURPOSE OF THIS ALTERNATE FUNCTION WAS TO NOT TRUST THE FORCE AND
SEE HOW IT GOES *)
let string_reverse_recurse_alt s =
  if String.length s = 0 then ""
     else let idx = String.length s - 1 in
          let rec visit n =
            if n = 0 then Char.escaped (String.get s idx)
            else visit (pred n) ^ Char.escaped (String.get s (idx - 1 - pred n))
          in visit idx;;

let () = assert(test_reverse string_reverse_recurse_alt);;

(* Question 3 *)

let test_reverse_v1 candidate =
  let b0 = (candidate "" = "")
  and b1 = (candidate "a" = "a")
  and b2 = (candidate "ab" = "ba")
  and b3 = (candidate "abc" = "cba")
  and b4 = (candidate (candidate "cake") = "cake")
  and b5 = let s1 = String.make (Random.int 5) (char_of_int (Random.int 255))
           and s2 = String.make (Random.int 5) (char_of_int (Random.int 255))
           in (candidate s1 ^ candidate s2 = candidate (s2 ^ s1)) 
  in b0 && b1 && b2 && b3 && b4 && b5;;

let () = assert (test_reverse_v1 string_reverse_v1);;

let () = assert (test_reverse_v1 string_reverse_recurse);;

(* Question 4 *)
(* EDITED TO INCLUDE REVERSE_IDX OUTSIDE RECURSION *)
let palindrome_v1 s =
  let len = String.length s
  in let reverse_idx = 2 * len - 1
  in Stringy.init (len * 2) (fun i ->
	 if i < len then (String.get s i)
	 else (String.get s (reverse_idx - i)));;

let palindrome_v2 s =
  s ^ string_reverse_v1 s;;

(* EDITED TO INCLUDE LAST_IDX *)
let palindrome_v3 s =
  let len = String.length s
  in let last_idx = len - 1
     in let rec visit n =
          if n = 0 then ""
          else let n' = n - 1
               in let ih = visit n'
                  in ih ^ Char.escaped (String.get s (last_idx - n'))
        in s ^ visit len;;

let palindrome_v4 s =
  s ^ string_reverse_recurse s;;

let palindrome_short_v1 s =
  let len = String.length s
  in let reverse_idx = 2 * len - 2
     in Stringy.init (len * 2 - 1) (fun i ->
            if i < len then (String.get s i)
            else (String.get s (reverse_idx - i)));;

(* Question 5 *)

let test_palindrome_detector candidate =
  let b0 = (candidate "" = true)
  and b1 = (candidate "a" = true)
  and b2 = (candidate "aa" = true)
  and b3 = (candidate "aba" = true)
  and b4 = (candidate "abba" = true)
  and b5 = (candidate "apd" = false)
  and b6 = (candidate "Aaa" = false)
  and b7 = (candidate "aapp" = false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b6 && b7;;

let palindrome_detector_mapi_even s =
  let len_original = String.length s in
  let half_len = len_original / 2 in
  let empty_half = Stringy.init (half_len) (fun i -> '_') in
  let half = Stringy.mapi (fun i c -> String.get s i) empty_half in
  half ^ string_reverse_v1 half = s;;

let palindrome_detector_mapi_odd s =
  let len_original = String.length s in
  let half_len = len_original / 2 + 1 in
  let empty_half = Stringy.init (half_len) (fun i -> '_') in
  let half = Stringy.mapi (fun i c -> String.get s i) empty_half in
  palindrome_short_v1 half = s;;

let palindrome_detector_mapi s =
  if (String.length s) mod 2 = 0 then palindrome_detector_mapi_even s
  else palindrome_detector_mapi_odd s;;

let () = assert (test_palindrome_detector palindrome_detector_mapi);;

(* EDITTED TO TRUST THE FORCE *)
let palindrome_detector_recurse_even s =
    let half =
      let rec visit n =
        if n = 0 then ""
        else let n' = pred n
             in let ih = visit n'
                in ih ^ Char.escaped (String.get s n')
      in visit (String.length s / 2)
    in half ^ string_reverse_v1 half = s;;

let palindrome_detector_recurse_odd s =
    let half =
      let rec visit n =
        if n = 0 then ""
        else let n' = pred n
             in let ih = visit n'
                in ih ^ Char.escaped (String.get s n')
      in visit (String.length s / 2 + 1)
    in palindrome_short_v1 half = s;;

let palindrome_detector_rec s =
  if (String.length s) mod 2 = 0 then palindrome_detector_recurse_even s
  else palindrome_detector_recurse_odd s;;

let () = assert (test_palindrome_detector palindrome_detector_rec)

(* Question 6 *)
let reverse_palindrome_huh_v0 s =
  let () = assert (palindrome_detector_mapi s) in
  string_reverse_v1 s;;

let reverse_palindrome_huh_v1 s =
  let () = assert (palindrome_detector_mapi s) in
  s;;

let reverse_palindrome_maybe s =
  let () = assert (palindrome_detector_mapi s) in
  let len = String.length s in
  if len mod 2 = 0 then
    Stringy.init (len / 2) (fun i -> String.get s i)
  else
    Stringy.init (len / 2 + 1) (fun i -> String.get s i);;

(* Question 7 *)
(* EDITTED TO TRUST THE FORCE *)
let fib_string s =
  let rec visit n =
    if n = 0 then " "
    else let n' = n - 1
	 in let ih = visit n'
	    in ih ^ String.make (fib n + 1) (String.get s n)
  in visit (String.length s - 1);;
      
(* end of midterm_mini4.ml *)

(* more for the road exercise 7 *)
let string_map_alt f s =
  Stringy.mapi (fun i c -> f c) s;;

(* end of more for the road exercise 7 *)
