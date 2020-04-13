(* EXERCISE 10 *)
let test_successor candidate =
  (candidate 0 = 1)
  &&(candidate 1 = 2)
  &&(candidate 2 = 3)
  &&(candidate 3 = 4)
  &&(candidate 4 = 5)
  &&(candidate 5 = 6);;

(*POSITIVE TESTING*)
test_successor (fun i-> i+1);;
test_successor (fun i-> 1+i);;

(*NEGATIVE TESTING*)
test_successor (fun i->i);;
test_successor (fun i->i+2);;

(* EXERCISE 11 *)
let test_not candidate =
  (candidate false = true)
  &&(candidate true = false);;

(*POSITIVE TESTING*)
test_not not;;

(*NEGATIVE TESTING*)
test_not (fun b->b);;

(* EXERCISE 13 *)
let test_twice_less_than candidate=
  (candidate 100 200 300 = true)
  && (candidate 1 2 3 = true)
  && (candidate (-5) (-3) (-2) = true)
  && (candidate 100 100 300 = false)
  && (candidate 0 0 0 = false)
  && (candidate 300 200 100 = false);;

(*POSITIVE TESTING*)
test_twice_less_than (fun i j k -> (i < j) && (j < k));;

(*NEGATIVE TESTING*)
test_twice_less_than (fun i j k -> ( i < j) && (j > k));;
test_twice_less_than (fun i j k -> (i = j) && (j < k));;

(* EXERCISE 14 *)
let test_sumtorial candidate =
  (candidate 0 = 0)
  &&(candidate 1 = 1)
  &&(candidate 2 = 3)
  &&(candidate 3 = 6)
  &&(candidate 4 = 10)
  &&(candidate 5 = 15);;

(* EXERCISE 15 *)
let test_positive_odd candidate =
  (candidate 0 = 1)
  &&(candidate 1 = 4)
  &&(candidate 2 = 9)
  &&(candidate 3 = 16)
  &&(candidate 4 = 25);;

(* POSITIVE TESTING *)
test_positive_odd (fun m -> (m+1)*(m+1));;

(*MISCHIEVOUS IMPLEMENTATION PASSING*)
test_positive_odd (fun m -> if m > -1 && m < 6
                            then (m+1)*(m+1)
                            else 63213);;

