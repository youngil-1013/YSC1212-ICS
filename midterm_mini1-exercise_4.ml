let fold_right_nat zero_case succ_case n_given =
  (* fold_right_nat : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
         in let ih = visit n'
            in succ_case ih    (* <-- succ_case takes one argument *)
  in visit n_given;;

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

(* Exercise 4*)
(* unit test for sumtorials *)

let test_sumtorial_v0 candidate =
  (* the base case: *)
  let b0 = (candidate 0 = 0)
  (* some intuitive cases: *)
  and b1 = (candidate 1 = 1)
  and b2 = (candidate 2 = 3)
  and b3 = (candidate 3 = 6)
  and b4 = (candidate 4 = 10)
  and b5 = (candidate 5 = 15)
  (* instance of the induction step: *)
  and b6 = (let n = Random.int 20
            in candidate (succ n) = (succ n) + candidate n)
             (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

test_sumtorial_v0 (fun n -> (n * n + n)/2);;
(* sanity testing *)

let sumtorial_v0 n_given =
  (* using parafold_right_nat to define a sumtorial function *)
  let () = assert (n_given >= 0) in
  parafold_right_nat 0 (fun n' ih -> (succ n') + ih) n_given;;

test_sumtorial_v0 sumtorial_v0;;

let sumtorial_v1 n_given =
  (* using a non-recursive function to define a sumtorial function *)
  let () = assert (n_given >= 0) in
  (n_given * (n_given + 1)) / 2;;

(* Exercise 5*)
(* a. Compose a unit-test function for sum, based on its inductive specification. *)

let test_sum_v0 candidate =
  (* applying some intuitive numbers to identity functions *)
  let b0 = (candidate (fun n -> n) 0 = 0)
  and b1 = (candidate (fun n -> n) 1 = 1)
  and b2 = (candidate (fun n -> n) 2 = 3)
  and b3 = (candidate (fun n -> n) 5 = 15) 
  (* applying random integers to identity functions *)
  and b4 = (let i = Random.int 30
            in candidate (fun n -> n) i = sumtorial_v0 i)
  in b0 && b1 && b2 && b3 && b4;;

let sum_v0 f n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then f(0)
    else let n' = n - 1
         in let ih = visit n'
            in ih + f (succ n')
  in visit n_given;;

test_sum_v0 sum_v0
