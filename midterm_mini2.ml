(* midterm_mini2.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2019-2020 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Was version of Sat 22 Feb 2020 *)
(* Version of Wed 18 Mar 2020 *)

(* ********** *)

(* name: Young Il Kim
   email address:  youngil.kim@u.yale-nus.edu.sg
   student number: A0207809Y

   other members of the group:
   name: Sewen Thy
   name: Kao Zhao Yuan
   name: Peng Chiao Yin
*)

(* ********** *)
module Stringy =
  struct
    let mapi f s =
      let i = ref ~-1
      in String.map (fun c ->
             i := succ !i;
             f !i c)
           s
    let init n f =
      if 0 <= n && n <= Sys.max_string_length
      then mapi (fun i _ -> f i) (String.create n)
      else raise (Invalid_argument "String.create");;
  end;;
    
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

let show_bool b =
 (* show_bool : bool -> string *)
  if b
  then "true"
  else "false";;

let show_char c =
 (* show_char : char -> string *)
  "'" ^ (if c = '\\' then "\\\\" else if c = '\'' then "\\\'" else String.make 1 c) ^ "'";;

let show_string s =
 (* show_string : string -> string *)
  "\"" ^ s ^ "\"";;

let show_int n =
 (* show_int : int -> string *)
  if n < 0
  then "(" ^ string_of_int n ^ ")"
  else string_of_int n;;

let show_unit () =
 (* show_unit : unit -> string *)
  "()";;

(* ********** *)

let an_int n =
  let () = Printf.printf "processing %s...\n" (show_int n)
  in n;;

let a_bool b =
  let () = Printf.printf "processing %s...\n" (show_bool b)
  in b;;

let a_char c =
  let () = Printf.printf "processing %s...\n" (show_char c)
  in c;;

let a_string s =
  let () = Printf.printf "processing %s...\n" (show_string s)
  in s;;

let a_unit () =
  let () = Printf.printf "processing the unit value...\n"
  in ();;

let a_function f =
  let () = Printf.printf "processing a function...\n"
  in f;;

(* ********** *)

(* Question 2 *)

let identity a =
  a;;

let add a b =
  a + b;;

(* INSERTED *)
(*
# a_function identity (an_int 3);;
processing 3...
processing a function...
- : int = 3

# a_function add (an_int 1) (an_int 2);;
processing 2...
processing 1...
processing a function...
- : int = 3
 *)

(* Question 3 *)
(* INSERTED *)
(*
# let a = an_int 1 and b = a_string "2" and c = a_function (+) ;;
processing 1...
processing "2"...
processing a function...
val a : int = 1
val b : string = "2"
val c : int -> int -> int = <fun>
 *)
(* Global Declaration of a and b *)

(*
# let c = an_int 1 and d = an_int 2 in an_int c + d;;
processing 1...
processing 2...
processing 1...
- : int = 3
 *)
(* Local Declaration of c and d *)

(*
# let c = an_int 1 and d = an_int 2 in a_function (+) (an_int c) (an_int d);;
processing 1...
processing 2...
processing 2...
processing 1...
processing a function...
- : int = 3
 *)

(* Question 4 *)
(* EDITED *)
(*
# (a_bool true) && (a_bool false);;
processing true...
processing false...
- : bool = false

# (a_bool true) && (a_bool true);;
processing true...
processing true...
- : bool = true

# (a_bool false) && (a_bool true);;
processing false...
- : bool = false

# (a_bool false) && (a_bool false);;
processing false...
- : bool = false
 *)

(* Question 5 *)

let test_warmup candidate =
  (candidate 'a' 'b' 'c' = "abc");;

let warmup_v0 c0 c1 c2 =
  String.make 1 c0 ^ String.make 1 c1 ^ String.make 1 c2;;

let warmup_v1 c0 c1 c2 =
  Char.escaped c0 ^ Char.escaped c1 ^ Char.escaped c2;; 
(* Special credit to Sewen Thy for telling me about Char.escaped *)

let () = assert (test_warmup warmup_v0);;
let () = assert (test_warmup warmup_v1);;

(* ********** *)

(* Question 6 *)

let order_string_map s =
  String.map (a_char) s;;

(*
# order_string_map "0123";;
processing '0'...
processing '1'...
processing '2'...
processing '3'...
- : string = "0123"
 *)

(* String.map accesses characters from the smallest index to the greatest *)

(* INSERTED CORNER CASE b0 *)
let test_string_map candidate =
  let b0 = (candidate (fun i -> i) "" = "")
  and b1 = (candidate (fun i -> i) "abcde" = "abcde")
  and b2 = (candidate (fun i -> i) "hello" = "hello")
  and b3 = (candidate (fun i -> 'a') "abcde" = "aaaaa")
  in b0 && b1 && b2 && b3;;

let apply_f_to_char_n f s n =
  Char.escaped (f (String.get s n));;

(* EDITED BASE CASE AND IH *)
let string_map_up f s =
  let s_len = String.length s
  in let rec walk n =
       if n = 0
       then ""
       else let n' = n - 1
            in let ih = walk n'
               in ih ^ Char.escaped (f (String.get s n'))
     in walk s_len;;

let () = assert (test_string_map string_map_up);;

(* INCLUDED TO SHOW THAT THE IMPLEMENTATION ABOVE IS FOLD-READY *)

let string_map_up_alt f s =
  let s_len = String.length s
  in parafold_right_nat "" (fun n' ih -> (ih ^ Char.escaped (f (String.get s n')))) (s_len);;

let () = assert (test_string_map string_map_up_alt)

(* EDITED TO TRUST THE FORCE *)
let string_map_down f s =
  let s_len = String.length s
  in let last_index = s_len - 1
     in let rec walk n =
          if n = 0
          then ""
          else let n' = pred n
               in let ih = walk n'
                  in Char.escaped (f (String.get s (last_index - n'))) ^ ih
        in walk s_len;;

let () = assert (test_string_map string_map_down);;

(* INCLUDED TO SHOW THAT THE IMPLEMENTATION ABOVE IS FOLD-READY *)

let string_map_down_alt f s =
  let s_len = String.length s
  in let last_index = s_len - 1
     in parafold_right_nat "" (fun n' ih -> ((Char.escaped (f (String.get s (last_index - n'))) ^ ih))) s_len;;

let () = assert (test_string_map string_map_down_alt)
(* ********** *)

(* Question 7 *)

(* INSERTED CORENER CASE b0 *)
let test_mapi candidate =
  let b0 = (candidate (fun i c -> c) "" = "")
  and b1 = (candidate (fun i c -> c) "abcde" = "abcde")
  and b2 = (candidate (fun i c -> 'a') "world" = "aaaaa")
  and b3 = (candidate (fun i _ -> char_of_int (i + int_of_char '0')) "abcd" = "0123")
	     (* Inspired by Professor Olivier Danvy's Blam *)
  in b0 && b1 && b2 && b3;;

let () = assert (test_mapi Stringy.mapi);;

(* EDITTED TO TRUST THE FORCE *)
let string_mapi_up f s =
  let s_len = String.length s
  in let rec walk n =
       if n = 0
       then ""
       else let n' = n - 1
            in let ih = walk n'
               in ih ^ Char.escaped (f n' (String.get s n'))
     in walk s_len;;

let () = assert (test_mapi string_mapi_up);;

(* EDITTED TO TRUST THE FORCE AND TO USE USEFUL NAMES *)
let string_mapi_down f s =
  let s_len = String.length s
  in let last_index = s_len - 1
     in let rec walk n =
          if n = 0
          then ""
          else let n' = pred n
               in let ih = walk n'
                  in let i = last_index - n'
                  in Char.escaped (f i (String.get s i)) ^ ih
        in walk s_len;;

let () = assert (test_mapi string_mapi_down);;

let string_mapi_down_t1 f s =
  let s_len = String.length s
  in let rec walk n =
       if n = 0
       then ""
       else let n' = pred n
            in let ih = walk n'
               in let () = Printf.printf "s_len - 1 calculated \n"
               in let () = Printf.printf "s_len - 1 calculated \n"
                  in Char.escaped (f (s_len - 1) (String.get s (s_len - 1- n'))) ^ ih
     in walk s_len;;

let string_mapi_down_t2 f s =
  let s_len = String.length s
  in let last_index = s_len - 1
     in let () = Printf.printf "last_index defined\n"
        in let rec walk n =
             if n = 0
             then ""
             else let n' = pred n
                  in let ih = walk n'
                     in Char.escaped (f (last_index -  n') (String.get s (last_index - n'))) ^ ih
           in walk s_len;;

(* INCLUDED TO SHOW THAT THE ABOVE IMPLEMENTATION IS FOLD-READY *)
let string_mapi_down_alt f s =
  let s_len = String.length s
  in let last_index = s_len - 1
     in parafold_right_nat "" (fun n' ih -> Char.escaped (f (last_index -  n') (String.get s (last_index - n'))) ^ ih) s_len;;

let () = assert (test_mapi string_mapi_down_alt);;
 
(* ********** *)

(* end of midterm_mini2.ml *)
