(* week-08_exercise-11.ml *)
(* Introduction to Computer Science (YSC1212), Sem1, 2019-2020 *)
(* Group 8: Joy, Sewen, Young, and Zhao *)
(* Version of Wed 18 Mar 2020 *)

(* Required methods *)

let fold_right_nat zero_case succ_case n =
  let () = assert (0 <= n) in
  let rec visit n =
    if n = 0
    then zero_case
    else succ_case (visit (pred n))
  in visit n;;

(* ********* *)

let member v vs_init =
  let rec visit vs =
    match vs with
    | [] ->
       false
    | v' :: vs' ->
       if v = v'
       then true
       else visit vs'
  in visit vs_init;;

let belongs_to e es =
  member e es;;

let set_add e es =
  if belongs_to e es
  then es
  else e :: es;;

let is_included_in e1s e2s =
  let rec visit e1s =
    match e1s with
    | [] ->
       true
    | e1 :: e1s' ->
       if belongs_to e1 e2s
       then visit e1s'
       else false
  in visit e1s;;

let includes e2s e1s =
  is_included_in e1s e2s;;

let set_equal e1s e2s =
  (is_included_in e1s e2s) && (includes e1s e2s);;

let set_union_v0 e1s e2s =
  let rec visit e1s =
    match e1s with
    | [] ->
       e2s
    | e1 :: e1s' ->
       if belongs_to e1 e2s
       then visit e1s'
       else e1 :: visit e1s'
  in visit e1s;;

let set_intersection_v0 e1s e2s =
  let rec visit e1s =
    match e1s with
    | [] ->
       []
    | e1 :: e1s' ->
       if belongs_to e1 e2s
       then e1 :: visit e1s'
       else visit e1s'
  in visit e1s;;

let set_minus_v0 e1s e2s =
  let rec visit e1s =
    match e1s with
    | [] ->
       []
    | e1 :: e1s' ->
       if belongs_to e1 e2s
       then visit e1s'
       else e1 :: visit e1s'
  in visit e1s;;

let make_random_set n =
  let () = assert (n >= 0) in
  List.sort (fun i j -> if i < j then 0 else 1)
            (fold_right_nat []
                            (fun ih -> set_add (Random.int 1000) ih)
                            n);;
(* ********* *)

(* solution to exercise 11 *)

let beefed_up_test u n sub =
  let e0s = [0; 1; 2; 3; 4; 5; 6; 7; 8]
  and e1s = [1; 3; 5; 7]
  and e2s = [2; 4; 6; 8]
  in let b0 = (set_equal (n (u e0s e1s) e0s)
                 e0s)
     and b1 = (set_equal (n (u e0s e1s) e1s)
                 e1s)
     and b2 = (is_included_in (sub e0s e1s) e0s)
     and b3 = (set_equal (n (sub e0s e1s) e1s) [])
     (* associative property of union and intersection *)
     and b4 = (set_equal (n (n e0s e1s) e2s) (n e0s (n e1s e2s)))
     and b5 = (set_equal (u (u e0s e1s) e2s) (u e0s (u e1s e2s)))
     (* commutative property of union and intersection *)
     and b6 = (set_equal (n e0s e1s) (n e1s e0s))
     and b7 = (set_equal (u e0s e1s) (u e1s e0s))
     (* ...and the non-commutative property of subtraction *)
     and b8 = not (set_equal (sub e0s e1s) (sub e1s e0s))
     (* distributive property *)
     and b9 = (set_equal (u e0s (n e1s e2s)) (n (u e0s e1s) (u e0s e2s)))
     and b10 = (set_equal (n e0s (u e1s e2s)) (u (n e0s e1s) (n e0s e2s)))
     (* properties of empty lists *)
     and b11 = (set_equal (n e0s []) [])
     and b12 = (set_equal (u e0s []) e0s)
     (* absorption property *)
     and b13 = (set_equal (u e0s (n e0s e1s)) e0s)
     (* alternative definition of union *)
     and b14 = (set_equal (u e0s e1s) (u (u (sub e0s e1s) (n e0s e1s))(sub e1s e0s)))
     (* intersection of mutually exclusive sets *)
     and b15 = (set_equal (n e1s e2s) [])
     in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15;;

let () = assert (beefed_up_test set_union_v0 set_intersection_v0 set_minus_v0);;

let beefed_up_test_v2 u n sub =
  let e0s = ['a'; 'b'; 'c'; 'd'; 'e'; 'q'; 'z']
  and e1s = ['a'; 'b'; 'c']
  and e2s = ['n']
  in let b0 = (set_equal (n (u e0s e1s) e0s)
                 e0s)
     and b1 = (set_equal (n (u e0s e1s) e1s)
                 e1s)
     and b2 = (is_included_in (sub e0s e1s) e0s)
     and b3 = (set_equal (n (sub e0s e1s) e1s) [])
     (* associative property of union and intersection *)
     and b4 = (set_equal (n (n e0s e1s) e2s) (n e0s (n e1s e2s)))
     and b5 = (set_equal (u (u e0s e1s) e2s) (u e0s (u e1s e2s)))
     (* commutative property of union and intersection *)
     and b6 = (set_equal (n e0s e1s) (n e1s e0s))
     and b7 = (set_equal (u e0s e1s) (u e1s e0s))
     (* ...and the non-commutative property of subtraction *)
     and b8 = not (set_equal (sub e0s e1s) (sub e1s e0s))
     (* distributive property *)
     and b9 = (set_equal (u e0s (n e1s e2s)) (n (u e0s e1s) (u e0s e2s)))
     and b10 = (set_equal (n e0s (u e1s e2s)) (u (n e0s e1s) (n e0s e2s)))
     (* properties of empty lists *)
     and b11 = (set_equal (n e0s []) [])
     and b12 = (set_equal (u e0s []) e0s)
     (* absorption property *)
     and b13 = (set_equal (u e0s (n e0s e1s)) e0s)
     (* alternative definition of union *)
     and b14 = (set_equal (u e0s e1s) (u (u (sub e0s e1s) (n e0s e1s))(sub e1s e0s)))
     (* intersection of mutually exclusive sets *)
     and b15 = (set_equal (n e1s e2s) [])
     in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15;;

let () = assert (beefed_up_test_v2 set_union_v0 set_intersection_v0 set_minus_v0);;

let beefed_up_test_v3 u n sub =
  let e0s = make_random_set (Random.int 10)
  and e1s = make_random_set (Random.int 10)
  and e2s = make_random_set (Random.int 10)
  in let b0 = (set_equal (n (u e0s e1s) e0s)
                 e0s)
     and b1 = (set_equal (n (u e0s e1s) e1s)
                 e1s)
     and b2 = (is_included_in (sub e0s e1s) e0s)
     and b3 = (set_equal (n (sub e0s e1s) e1s) [])
     (* associative property of union and intersection *)
     and b4 = (set_equal (n (n e0s e1s) e2s) (n e0s (n e1s e2s)))
     and b5 = (set_equal (u (u e0s e1s) e2s) (u e0s (u e1s e2s)))
     (* commutative property of union and intersection *)
     and b6 = (set_equal (n e0s e1s) (n e1s e0s))
     and b7 = (set_equal (u e0s e1s) (u e1s e0s))
     (* ...and the non-commutative property of subtraction *)
     and b8 = not (set_equal (sub e0s e1s) (sub e1s e0s))
     (* distributive property *)
     and b9 = (set_equal (u e0s (n e1s e2s)) (n (u e0s e1s) (u e0s e2s)))
     and b10 = (set_equal (n e0s (u e1s e2s)) (u (n e0s e1s) (n e0s e2s)))
     (* properties of empty lists *)
     and b11 = (set_equal (n e0s []) [])
     and b12 = (set_equal (u e0s []) e0s)
     (* absorption property *)
     and b13 = (set_equal (u e0s (n e0s e1s)) e0s)
     (* alternative definition of union *)
     and b14 = (set_equal (u e0s e1s) (u (u (sub e0s e1s) (n e0s e1s))(sub e1s e0s)))
     (* intersection of mutually exclusive sets *)
     and b15 = if not (is_included_in e1s e2s) then (set_equal (n e1s e2s) [])
               else true
     in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15;;

let () = assert (beefed_up_test_v3 set_union_v0 set_intersection_v0 set_minus_v0);;

(* ********* *)

(* end of week-08_exercise-11.ml *)

"week-08_exercise-11.ml"
