let test_show_bool show_bool_candidate =
  let b0 = (show_bool_candidate true = "true")
  and b1 = (show_bool_candidate false = "false")
  in b0 && b1;;

let show_bool b =
  if b then "true"
  else "false";; 

let () assert (test_show_bool show_bool);;

let test_show_char show_char_candidate =
  let b0 = (show_char_candidate 'a' = "'a'")
  and b1 = (show_char_candidate 'B' = "'B'")
  and b2 = (show_char_candidate '3' = "'3'")
  in b0 && b1 && b2;;

let () assert (test_show_char show_char);;

let show_char c =
"'" ^ String.make 1 c^ "'";;

let test_show_string show_string_candidate =
let b0 = (show_string_candidate "Yale-NUS College" = "\"Yale-NUS College\"")
let b1 = (show_string_candidate "" = "\"\"") 

let test_show_int show_int_candidate =
  let b0 = (show_int_candidate 0 = "0")
  and b1 = (show_int_candidate 1 = "1")
  and b2 = (show_int_candidate ~-123 = "-123")
  in b0 && b1 && b3;;


let show_int n =
  if n < 0
  then "(" ^ string_of_int n ^ ")";;

let test_show_int_cross_bool show_int_cross_bool_candidate =
  let b0 = (show_int_cross_bool_candidate (123, true)
            = "(123, true )")
  and b1 = (show_int_cross_bool_candidate (-123, true)
            = "((-123, true )")
  in b0 && b1;;

let () = assert (test_show_int_cross_bool show_int_cross_bool);;

let fac n =
  let () = assert (n >= 0)
                  (* aways setting as positive n *)
  in let rec visit n =
       if n = 0
       then 1
                  (* base case complete *)
       else n' = pred n
            in let ih = visit n'
               in n * ih
    in visit n;;

let () = assert (test_fac fac)

(* sumtorial 0 = 0
   sumtorial (succ n) = succ n + sumtorial n
 *)

let test_sumtorial sumtorial_candidate =
  let n = Random.int 100
  in sumtorial_candidate n = n * (n + 1) / 2;;

let sumtorial n_given =
  let () = assert (n_given >= 0)
  in let rec visit n =
       if n = 0
       then 0
       else let n' = pred n
            in let ih = visit n'
               in n + ih
     in visit n_given;;

(* sum_odds 0 = 1
   sum_odds (succ n) = (2 * (succ n) + 1) + sum_odds n
 *)

let test_sum_odd candidate_sum_odds =
  let n = Random.int 100
  in candidate_sum_odds n = (n + 1) * (n + 1);;

let sum_odds n_given =
  let () = assert (n_given >= 0)
    in let rec visit n =
         if n = 0
         then 1
         else let n' = pred n
              in let ih = visit n'
                 in (2 * n + 1) + ih
       in visit n_given

(* for all m, 0 x m
and for all n, m; succ n x m = n x m + m
 *)

let () = assert (test_add_mul 1000 add mul);;

let add x_given y =
  let () = assert (x_given >= 0)
  in let rec visit x =
       if x = 0
       then y
       else let x' pred x
            in let ih = visit x'
               in succ ih
     in visit x_given;;

let mul n_given m =
  let () = assert (n_given >= 0)
  in let rec visit n =
       if n = 0
       then 0
       else let n' = pred n
            in let ih = visit n'
               in ih +  m
     in visit_n_given;;
