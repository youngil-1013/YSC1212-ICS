let foo = fun f -> fun x -> fun y -> f x y;;

let test_shift1 candidate =
  let b0 = (candidate "" = "")
  and b1 = (candidate "a" = "a")
  and b2 = (candidate "ab" = "ba")
  and b3 = (candidate "abc" = "cab")
  and b4 = (candidate "abcd" = "dabc")
  in b0 && b1 && b2 && b3 && b4;;

(*let shift1_right s =
  let n = String.length s
  in String.mapi
       (fun i c ->
         String.get s ((i + 1) mod n)
       s;;
 *)

let n' = pred n
    in let ih visit n'
       in succ ih;;

let ih = visit (pred n)
    in succ ih;;

(* if e then true else then false*)

let an_int n =
  let () = Printf.printf "%i\n" n
  in n;;

an_int 10;;

(* order of evaluation matters in computation but not in mathematics *)

(* let x2 = an_int 10 in let x1 = an_int 1 in x1 + x2;; *)
let foo x = foo x (* direct reference *)

let rec evenp n =
  if n = 0
  then true
  else oddp (pred n)
and oddp n =
  if n = 0
  then false
  else evenp (pred n);;
(* Tail call, tail recursion*)
(* if x = 3n = ternary
      x = 3n + 1 = post-ternary
      x = 3n + 2 = pre-ternary*)

let rec ternp n =
  if (n mod 3) = 0
  then true
  else postp (pred n)
and postp n =
  if (n mod 3) = 1
  then false
  else prep (pred n)
and prep n =
  if (n mod 3) = 2
  then false
  else ternp (pred n);;

skeleton:
  let ... n_given =
    let () = assert (n_given >= 0)
    in let rec visit n=
         if n = 0
         then ...
         else let n' = pred n
              in let ih = visit n'
                 in ... ih ...
       in visit n_given;;

let fold_right_nat zero_case succ_case n_given =
    let () = assert (n_given >= 0)
    in let rec visit n=
         if n = 0
         then zero_case
         else let n' = pred n
              in let ih = visit n'
                 in succ_case ih
       in visit n_given;;

let twice_alt n =
  fold_right_nat 0 (fun i -> succ (succ i)) n;;

let add_alt x y =
  fold_right_nat y (fun i -> succ i) x;;

let mult_alt x y =
  fold_right_nat 0 (fun i -> y + i) x;;

let power_alt x n =
  fold_right_nat 1 (fun i -> x * i) n;;

let evenp_alt n =
  fold_right_nat true (fun b -> not b)n;;
(*
let fac_alt n =
    fold_right_nat 1 (fun
 *)

let parafold_right_nat zero_case succ_case n_given =
    let () = assert (n_given >= 0)
    in let rec visit n =
         if n = 0
         then zero_case
         else let n' = pred n
              in let ih = visit n'
                 in succ_case n ih
       in visit n_given;;

(* given an INTEGER *)
