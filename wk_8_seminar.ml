(*
(fun x -> e) e'
=~=
let x = e' in e

let x = v in e
=~=
e [v/x] <- substitution
 *)

let fold_right_nat case_zero case_succ n =
  let () = assert (0 <= n) in
  let rec visit n =
    if n = 0
    then case_zero
    else let n' = n - 1
         in let ih = visit n'
            in case_succ ih
  in visit n;;

let twice_v4 n = fold_right_nat 0 (fun ih -> succ (succ ih)) n;;

(* use unit-tests to verify that we don't introduce an error *)

let test_twice candidate =
  let n = Random.int 10 in candidate n = 2 * n;;

let () = assert (test_twice twice_v4);;

let twice_v5 n =
  let n = n
  and case_succ = (fun ih -> succ (succ ih))
  and case_zero = 0
  in let () = assert (0 <= n) in
  let rec visit n =
    if n = 0
    then case_zero
    else let n' = n - 1
         in let ih = visit n'
            in case_succ ih
  in visit n;;
(* Inlined the definition of fold_right *)

let () = assert (test_twice twice_v5);;

let twice_v6 n =
let () = assert (0 <= n) in
  let rec visit n =
    if n = 0
    then 0
    else let n' = n - 1
         in let ih = visit n'
            in (fun ih -> succ (succ ih)) ih
  in visit n;;

let () = assert (test_twice twice_v6);;

let twice_v7 n =
let () = assert (0 <= n) in
  let rec visit n =
    if n = 0
    then 0
    else let n' = n - 1
         in let ih = visit n'
            in let ih = ih
               in succ(succ ih)
  in visit n;;

let () = assert (test_twice twice_v7);;

let twice_v8 n =
let () = assert (0 <= n) in
  let rec visit n =
    if n = 0
    then 0
    else let n' = n - 1
         in let ih = visit n'
               in succ (succ ih)
  in visit n;;

let () = assert (test_twice twice_v8);;

let test_add add =
  let x = Random.int 10 and y = Random.int 20
  in add x y = x + y;;

let add_v4 x y =
  fold_right_nat y succ x;;

let () = assert (test_add add_v4);;

(* Use f(f(x)) for string reversal *)

let neg_v0 b =
  if b = true
  then false
  else if b = false
  then true
  else assert false;;

let neg_v1 b =
  if b
  then false
  else if b = false
  then true
  else assert false;;
