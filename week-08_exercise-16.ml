(* week-08_exercise_16.ml *)
(* Introduction to Computer Science (YSC1212), Sem1, 2019-2020 *)
(* Group 8: Joy, Sewen, Young, and Zhao *)
(* Version of Tue 17 Mar 2020 *)

(* Required methods *)
let fold_right_nat zero_case succ_case n =
  let () = assert (0 <= n) in
  let rec visit n =
    if n = 0
    then zero_case
    else succ_case (visit (pred n))
  in visit n;;

(* ********** *)

(*
  Rationale for list_andmap:
  evaluating
    list_andmap p [v1; v2; v3; ...; vN]
  desugars into
    p v1 && p v2 && p v3 && ... && p vN && true
*)

let list_andmap p vs =
 (* list_andmap : ('a -> bool) -> 'a list -> bool *)
  let rec visit vs =
    match vs with
    | [] ->
       true
    | v :: vs' ->
       p v && visit vs'
  in visit vs;;

(* ***** *)

(*
  Rationale for list_ormap:
  evaluating
    list_ormap p [v1; v2; v3; ...; vN]
  desugars into
    p v1 || p v2 || p v3 || ... || p vN || false
*)

let list_ormap p vs =
 (* list_ormap : ('a -> bool) -> 'a list -> bool *)
  let rec visit vs =
    match vs with
    | [] ->
       false
    | v :: vs' ->
       p v || visit vs'
  in visit vs;;

(* ********** *)

let indent i =
  String.make (2 * i) ' ';;

let show_int n =
 (* show_int : int -> string *)
  if n < 0
  then "~" ^ string_of_int n
  else string_of_int n;;

let show_bool n =
 (* show_int : bool -> string *)
  if n
  then "true"
  else "false";;

let show_list show_yourself vs =
  match vs with
  | [] ->
     "[]"
  | v :: vs' ->
     let rec show_list_aux v vs' =
       match vs' with
       | [] ->
          show_yourself v
       | v' :: vs'' ->
          (show_yourself v) ^ "; " ^ (show_list_aux v' vs'')
     in "[" ^ show_list_aux v vs' ^ "]";;

let show_pair show_yourself_1 show_yourself_2 (v1, v2) =
  "(" ^ show_yourself_1 v1 ^ ", " ^ show_yourself_2 v2 ^ ")";;

let show_triple show_yourself_1 show_yourself_2 show_yourself_3 (v1, v2, v3) =
  "(" ^ show_yourself_1 v1 ^ ", " ^ show_yourself_2 v2 ^ ", " ^ show_yourself_3 v3 ^ ")";;

(* ********** *)

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

(* ********** *)

let belongs_to e es =
  member e es;;

(* ********** *)

let set_add e es =
  if belongs_to e es
  then es
  else e :: es;;

(* ********** *)

let make_random_set n =
  let () = assert (n >= 0) in
  List.sort (fun i j -> if i < j then 0 else 1)
            (fold_right_nat []
                            (fun ih -> set_add (Random.int 1000) ih)
                            n);;
(* ********** *)


(* For Exercise 4 *)

let soundness_of_set_union set_union e1s e2s =
  (* the resulting set only contains elements of e1s or e2s *)
  let rec visit e12s =
    match e12s with
    | [] ->
       true
    | e12 :: e12s' ->
       (belongs_to e12 e1s || belongs_to e12 e2s) && visit e12s'
  in visit (set_union e1s e2s);;

let soundness_of_set_union' set_union e1s e2s =
  (* the resulting set only contains elements of e1s or e2s *)
  list_andmap
    (fun e12 -> belongs_to e12 e1s || belongs_to e12 e2s)
    (set_union e1s e2s);;

let completeness_of_set_union set_union e1s e2s =
  (* all elements of e1s and e2s occur in the resulting set *)
  let e12s = set_union e1s e2s
  in (list_andmap (fun e1 -> belongs_to e1 e12s) e1s) && (list_andmap (fun e2 -> belongs_to e2 e12s) e2s);;

let soundness_and_completeness_of_set_union set_union e1s e2s =
  let sound = soundness_of_set_union' set_union e1s e2s
              || let () = Printf.printf "soundness_of_set_union failed for %s and %s\n"
                                        (show_list show_int e1s)
                                        (show_list show_int e2s)
                 in false
  and complete = completeness_of_set_union set_union e1s e2s
                 || let () = Printf.printf "completeness_of_set_union failed for %s and %s\n"
                                           (show_list show_int e1s)
                                           (show_list show_int e2s)
                    in false
  in sound && complete;;

let test_set_union candidate =
  let b0 = soundness_and_completeness_of_set_union candidate [] []
  and b1 = (soundness_and_completeness_of_set_union candidate [] [0; 1; 2; 3; 4; 5])
           &&
           (soundness_and_completeness_of_set_union candidate [0; 1; 2; 3; 4; 5] [])
  and b2 = soundness_and_completeness_of_set_union candidate [0; 1; 2; 3; 4; 5] [0; 1; 2; 3; 4; 5]
  and b3 = (soundness_and_completeness_of_set_union candidate [1; 2; 3] [0; 4; 5])
           &&
           (soundness_and_completeness_of_set_union candidate [0; 4; 5] [1; 2; 3])
  and b4 = (soundness_and_completeness_of_set_union candidate [0; 1; 2; 3; 4; 5] [3; 4; 5; 6; 7])
           &&
           (soundness_and_completeness_of_set_union candidate [3; 4; 5; 6; 7] [0; 1; 2; 3; 4; 5])
  and b5 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_union candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_union candidate es2 es1)
  and b6 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_union candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_union candidate es2 es1)
  and b7 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_union candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_union candidate es2 es1)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 (* etc. *);;

(* Provided solution: *)

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

let () = assert (test_set_union set_union_v0);;

let sort ns =
 (* sort : int list -> int list *)
  List.sort (fun i j -> if i < j then 0 else 1) ns;;


(* Solution: *)
let set_union_v1 e1s e2s =
  let () = assert (e1s = sort e1s && e2s = sort e2s)
  in let rec visit e1s e2s =
       match e1s with
       | [] ->
          e2s
       | e1 :: e1s' ->
          match e2s with
          | [] -> e1s
          | e2 :: e2s' ->
             if e2 < e1 then
               e2 :: visit e1s e2s'
             else if e2 = e1 then
               e1 :: visit e1s' e2s'
             else e1 :: visit e1s' e2s
     in visit e1s e2s;;

let () = assert (test_set_union set_union_v1);;

let traced_set_union_v1 show_yourself e1s e2s =
    let () = Printf.printf "set_union %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let () = assert (e1s = sort e1s && e2s = sort e2s)
  in let rec visit e1s e2s i=
        let () = Printf.printf "%svisit %s %s ->\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) in
       match e1s with
       | [] ->
          let () = Printf.printf "%svisit [] %s<- %s\n" (indent i) (show_list show_yourself e2s) (show_list show_yourself e2s) in
          e2s
       | e1 :: e1s' ->
          match e2s with
          | [] ->
             let () = Printf.printf "%svisit %s []<- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e1s) in
             e1s
          | e2 :: e2s' ->
             if e2 < e1 then
               let result = e2 :: visit e1s e2s' (i+1) in
               let () = Printf.printf "%svisit %s %s<- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself result) in
               result
             else if e2 = e1 then
               let result = e1 :: visit e1s' e2s' (i+1) in
               let () = Printf.printf "%svisit %s %s<- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself result) in
               result
             else let result = e1 :: visit e1s' e2s (i+1) in
                  let () = Printf.printf "%svisit %s %s<- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself result) in
                  result
     in let final_result = visit e1s e2s 1
        in let () = Printf.printf "set_union %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
           final_result;;

(* using sort on appended list *)

let set_union_v2 e1s e2s =
  let e12s = sort (e1s @ e2s)
  in match e12s with
     | [] ->
        []
     | e12 :: e12s' ->
        let rec visit e es =
          match es with
          | [] ->
             [e]
          | e' :: es' ->
             if e' = e
             then visit e' es'
             else e :: visit e' es'
        in visit e12 e12s';;

let () = assert (test_set_union set_union_v2);;

let traced_set_union_v2 show_yourself e1s e2s =
  let () = Printf.printf "set_union %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let e12s = sort (e1s @ e2s)
  in match e12s with
     | [] ->
        []
     | e12 :: e12s' ->
        let rec visit e es i=
          let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself es) in
          match es with
          | [] ->
             let () = Printf.printf "%svisit [] <- %s\n" (indent i) (show_list show_yourself [e]) in
             [e]
          | e' :: es' ->
             if e' = e
             then visit e' es' i
             else let result = e :: visit e' es' (i + 1)
                  in let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself es) (show_list show_yourself result)
                     in result
        in let final_result = visit e12 e12s' 1 in
           let () = Printf.printf "set_union %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
           final_result;;

(* Traced v0 *)
let traced_set_union_v0 show_yourself e1s e2s =
  let () = Printf.printf "set_union %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let rec visit e1s i =
    let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself e1s) in
    match e1s with
    | [] ->
       let () = Printf.printf "%svisit [] <- %s\n" (indent i) (show_list show_yourself e2s) in
       e2s
    | e1 :: e1s' ->
       if belongs_to e1 e2s
       then visit e1s' i
       else let result = e1 :: visit e1s' (i + 1)
            in let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself result) in
               result
  in let final_result = visit e1s 1
     in let () = Printf.printf "set_union %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
        final_result;;

(* Let's add a traced member *)
let traced_member show_yourself v vs_init =
   let () = Printf.printf "member %s %s ->\n" (show_yourself v) (show_list show_yourself vs_init) in
   let rec visit vs i=
     let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself vs) in
    match vs with
    | [] ->
        let () = Printf.printf "%svisit [] <- false\n" (indent i) in
       false
    | v' :: vs' ->
       if v = v'
       then true
       else let result = visit vs' (i + 1) in
            let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself vs) (show_bool result)
            in result
   in let final_result = visit vs_init 1
      in let () = Printf.printf "visit %s <- %s\n" (show_list show_yourself vs_init) (show_bool final_result)
         in final_result;;


let traced_member_set_union_v0 show_yourself e1s e2s =
  let () = Printf.printf "set_union %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let rec visit e1s i =
    let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself e1s) in
    match e1s with
    | [] ->
       let () = Printf.printf "%svisit [] <- %s\n" (indent i) (show_list show_yourself e2s) in
       e2s
    | e1 :: e1s' ->
       if traced_member show_yourself e1 e2s
       then visit e1s' i
       else let result = e1 :: visit e1s' (i + 1)
            in let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself result) in
               result
  in let final_result = visit e1s 1
     in let () = Printf.printf "set_union %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
        final_result;;

(* ********** *)

(* Exercise 6 *)

let soundness_of_set_intersection set_intersection e1s e2s =
  (* the resulting set only contains elements of e1s and e2s *)
  list_andmap
    (fun e12 -> belongs_to e12 e1s && belongs_to e12 e2s)
    (set_intersection e1s e2s);;

let completeness_of_set_intersection set_intersection e1s e2s =
  (* all common elements of e1s and e2s occur in the resulting set *)
  let e12s = set_intersection e1s e2s
  in list_andmap
       (fun e1 -> if belongs_to e1 e2s
                  then belongs_to e1 e12s
                  else true)
       e1s;;

let soundness_and_completeness_of_set_intersection set_intersection e1s e2s =
  let sound = soundness_of_set_intersection set_intersection e1s e2s
              || let () = Printf.printf "soundness_of_set_intersection failed for %s and %s\n"
                                        (show_list show_int e1s)
                                        (show_list show_int e2s)
                 in false
  and complete = completeness_of_set_intersection set_intersection e1s e2s
                 || let () = Printf.printf "completeness_of_set_intersection failed for %s and %s\n"
                                           (show_list show_int e1s)
                                           (show_list show_int e2s)
                    in false
  in sound && complete;;

let test_set_intersection candidate =
  let b0 = soundness_and_completeness_of_set_intersection candidate [] []
  and b1 = (soundness_and_completeness_of_set_intersection candidate [] [0; 1; 2; 3; 4; 5])
           &&
           (soundness_and_completeness_of_set_intersection candidate [0; 1; 2; 3; 4; 5] [])
  and b2 = soundness_and_completeness_of_set_intersection candidate [0; 1; 2; 3; 4; 5] [0; 1; 2; 3; 4; 5]
  and b3 = (soundness_and_completeness_of_set_intersection candidate [1; 2; 3] [0; 4; 5])
           &&
           (soundness_and_completeness_of_set_intersection candidate [0; 4; 5] [1; 2; 3])
  and b4 = (soundness_and_completeness_of_set_intersection candidate [0; 1; 2; 3; 4; 5] [3; 4; 5; 6; 7])
           &&
           (soundness_and_completeness_of_set_intersection candidate [3; 4; 5; 6; 7] [0; 1; 2; 3; 4; 5])
  and b5 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_intersection candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_intersection candidate es2 es1)
  and b6 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_intersection candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_intersection candidate es2 es1)
  and b7 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_intersection candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_intersection candidate es2 es1)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 (* etc. *);;

(* Provided solution: *)

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

let () = assert (test_set_intersection set_intersection_v0);;

let set_intersection_v1 e1s e2s =
  let () = assert (e1s = sort e1s && e2s = sort e2s)
  in let rec visit e1s e2s =
       match e1s with
       | [] ->
          []
       | e1 :: e1s' ->
          match e2s with
          | [] -> []
          | e2 :: e2s' ->
             if e2 < e1 then
               visit e1s e2s'
             else if e2 = e1 then
               e1 :: visit e1s' e2s'
             else visit e1s' e2s
     in visit e1s e2s;;

let () = assert (test_set_intersection set_intersection_v1);;

let traced_set_intersection_v1 show_yourself e1s e2s =
   let () = Printf.printf "set_intersection %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let () = assert (e1s = sort e1s && e2s = sort e2s)
  in let rec visit e1s e2s i =
        let () = Printf.printf "%svisit %s %s ->\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) in
       match e1s with
       | [] ->
          let () = Printf.printf "%svisit [] %s<- []\n" (indent i) (show_list show_yourself e2s)
          in []
       | e1 :: e1s' ->
          match e2s with
          | [] ->
             let () = Printf.printf "%svisit %s []<- []\n" (indent i) (show_list show_yourself e1s)
             in []
          | e2 :: e2s' ->
             if e2 < e1 then
               visit e1s e2s' i
             else if e2 = e1 then
               let result = e1 :: visit e1s' e2s' (i+1) in
               let () = Printf.printf "%svisit %s %s<- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself result) in
               result
             else visit e1s' e2s i
     in let final_result = visit e1s e2s 1
        in let () = Printf.printf "set_union %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
           final_result;;

(* for fun *)

let set_intersection_v2 e1s e2s =
  let e12s = sort (e1s @ e2s)
  in match e12s with
     | [] ->
        []
     | e12 :: e12s' ->
        let rec visit e es =
          match es with
          | [] ->
             []
          | e' :: es' ->
             if e' = e
             then e :: visit e' es'
             else visit e' es'
        in visit e12 e12s';;

let () = assert (test_set_intersection set_intersection_v2);;

let traced_set_intersection_v2 show_yourself e1s e2s =
  let () = Printf.printf "set_intersection %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let e12s = sort (e1s @ e2s)
  in match e12s with
     | [] ->
        []
     | e12 :: e12s' ->
        let rec visit e es i =
          let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself es) in
          match es with
          | [] ->
             let () = Printf.printf "%svisit [] <- []\n" (indent i) in
             []
          | e' :: es' ->
             if e' = e
             then let result = e :: visit e' es' (i + 1) in
                  let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself es) (show_list show_yourself result)
                     in result
             else visit e' es' i
        in let final_result = visit e12 e12s' 1 in
           let () = Printf.printf "set_intersection %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
           final_result;;

let traced_set_intersection_v0 show_yourself e1s e2s =
  let () = Printf.printf "set_intersection %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let rec visit e1s i =
    let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself e1s) in
    match e1s with
    | [] ->
       let () = Printf.printf "%svisit [] <- []\n" (indent i) in
       []
    | e1 :: e1s' ->
       if belongs_to e1 e2s
       then let result = e1 :: visit e1s' (i + 1)
            in let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself result) in
               result
       else visit e1s' i
  in let final_result = visit e1s 1
     in let () = Printf.printf "set_intersection %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
        final_result;;

let traced_member_set_intersection_v0 show_yourself e1s e2s =
  let () = Printf.printf "set_intersection %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let rec visit e1s i =
    let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself e1s) in
    match e1s with
    | [] ->
       let () = Printf.printf "%svisit [] <- []\n" (indent i) in
       []
    | e1 :: e1s' ->
       if traced_member show_yourself e1 e2s
       then let result = e1 :: visit e1s' (i + 1)
            in let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself result) in
               result
       else visit e1s' i
  in let final_result = visit e1s 1
     in let () = Printf.printf "set_intersection %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
        final_result;;

(* ********** *)

(* Exercise 9 *)

let soundness_of_set_minus set_minus e1s e2s =
  (* the resulting set only contains elements of e1s that do not occur in e2s *)
  let rec visit e12s =
    match e12s with
    | [] ->
       true
    | e12 :: e12s' ->
       (belongs_to e12 e1s && not (belongs_to e12 e2s)) && visit e12s'
  in visit (set_minus e1s e2s);;

let soundness_of_set_minus' set_minus e1s e2s =
  (* the resulting set only contains elements of e1s that do not occur in e2s *)
  list_andmap
    (fun e12 -> belongs_to e12 e1s && not (belongs_to e12 e2s))
    (set_minus e1s e2s);;

let completeness_of_set_minus set_minus e1s e2s =
  (* all elements of e1s and not of e2s occur in the resulting set
     and
     no element of e2s occurs in the resulting set *)
  let e12s = set_minus e1s e2s
  in (list_andmap
        (fun e1 -> if belongs_to e1 e2s
                   then true
                   else belongs_to e1 e12s)
        e1s)
     &&
     (list_andmap
        (fun e2 -> not (belongs_to e2 e12s))
        e2s);;

let soundness_and_completeness_of_set_minus set_minus e1s e2s =
  let sound = soundness_of_set_minus' set_minus e1s e2s
              || let () = Printf.printf "soundness_of_set_minus failed for %s and %s\n"
                                        (show_list show_int e1s)
                                        (show_list show_int e2s)
                 in false
  and complete = completeness_of_set_minus set_minus e1s e2s
                 || let () = Printf.printf "completeness_of_set_minus failed for %s and %s\n"
                                           (show_list show_int e1s)
                                           (show_list show_int e2s)
                    in false
  in sound && complete;;

let test_set_minus candidate =
  let b0 = soundness_and_completeness_of_set_minus candidate [] []
  and b1 = (soundness_and_completeness_of_set_minus candidate [] [0; 1; 2; 3; 4; 5])
           &&
           (soundness_and_completeness_of_set_minus candidate [0; 1; 2; 3; 4; 5] [])
  and b2 = soundness_and_completeness_of_set_minus candidate [0; 1; 2; 3; 4; 5] [0; 1; 2; 3; 4; 5]
  and b3 = (soundness_and_completeness_of_set_minus candidate [1; 2; 3] [0; 4; 5])
           &&
           (soundness_and_completeness_of_set_minus candidate [0; 4; 5] [1; 2; 3])
  and b4 = (soundness_and_completeness_of_set_minus candidate [0; 1; 2; 3; 4; 5] [3; 4; 5; 6; 7])
           &&
           (soundness_and_completeness_of_set_minus candidate [3; 4; 5; 6; 7] [0; 1; 2; 3; 4; 5])
  and b5 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_minus candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_minus candidate es2 es1)
  and b6 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_minus candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_minus candidate es2 es1)
  and b7 = let es1 = make_random_set 100
           and es2 = make_random_set 100
           in (soundness_and_completeness_of_set_minus candidate es1 es2)
              &&
              (soundness_and_completeness_of_set_minus candidate es2 es1)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 (* etc. *);;

(* Provided solution: *)

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
  
let () = assert (test_set_minus set_minus_v0);;

let set_minus_v1 e1s e2s =
  let () = assert (e1s = sort e1s && e2s = sort e2s)
  in let rec visit e1s e2s =
       match e1s with
       | [] ->
          []
       | e1 :: e1s' ->
          match e2s with
          | [] -> e1s
          | e2 :: e2s' ->
             if e2 < e1 then
               visit e1s e2s'
             else if e2 = e1 then
               visit e1s' e2s'
             else e1 :: visit e1s' e2s
     in visit e1s e2s;;

let () = assert (test_set_minus set_minus_v1);;

let traced_set_minus_v1 show_yourself e1s e2s =
  let () = Printf.printf "set_minus %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let () = assert (e1s = sort e1s && e2s = sort e2s)
  in let rec visit e1s e2s i =
        let () = Printf.printf "%svisit %s %s ->\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) in
       match e1s with
       | [] ->
          let () = Printf.printf "%svisit [] %s<- []\n" (indent i) (show_list show_yourself e2s)
          in []
       | e1 :: e1s' ->
          match e2s with
          | [] ->
             let () = Printf.printf "%svisit %s []<- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e1s)
             in e1s
          | e2 :: e2s' ->
             if e2 < e1 then
               visit e1s e2s' i
             else if e2 = e1 then
               visit e1s' e2s' i
             else let result = e1 :: visit e1s' e2s (i+1) in
                  let () = Printf.printf "%svisit %s %s<- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself result) in
                  result
     in let final_result = visit e1s e2s 1 in
        let () = Printf.printf "set_union %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
        final_result;;

(* Experiementing with logic and dynamic programming *)
let set_minus_v2 e1s e2s =
  let e12s = sort (e1s @ e2s @ e2s)
  in match e12s with
     | [] ->
        []
     | e12 :: e12s' ->
        let rec visit hold_e e es =
          match es with
          | [] ->
             if hold_e = e
             then []
             else [e]
          | e' :: es' ->
             if e' = e || hold_e = e
             then visit e e' es'
             else e :: visit e e' es'
        in visit ~-1 e12 e12s';;

let () = assert (test_set_minus set_minus_v2);;

let traced_set_minus_v2 show_yourself e1s e2s =
  let () = Printf.printf "set_minus %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let e12s = sort (e1s @ e2s @ e2s)
  in match e12s with
     | [] ->
        []
     | e12 :: e12s' ->
        let rec visit hold_e e es i =
          let () = Printf.printf "%svisit %s %s %s ->\n" (indent i) (show_yourself hold_e) (show_yourself e) (show_list show_yourself es) in
          match es with
          | [] ->
             if hold_e = e
             then let () = Printf.printf "%svisit [] <- []\n" (indent i) in []
             else  let () = Printf.printf "%svisit [] <- %s\n" (indent i) (show_list show_yourself [e]) in [e]
          | e' :: es' ->
             if e' = e || hold_e = e
             then visit e e' es' i
             else let result = e :: visit e e' es' (i + 1) in
                  let () = Printf.printf "%svisit %s %s %s <- %s\n" (indent i) (show_yourself hold_e) (show_yourself e) (show_list show_yourself es) (show_list show_yourself result)
                  in result
        in let final_result = visit ~-1 e12 e12s' 1 in
           let () = Printf.printf "set_union %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result)
           in final_result;;

let traced_set_minus_v0 show_yourself e1s e2s =
  let () = Printf.printf "set_minus %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let rec visit e1s i =
    let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself e1s) in
    match e1s with
    | [] ->
       let () = Printf.printf "%svisit [] <- []\n" (indent i) in
       []
    | e1 :: e1s' ->
       if belongs_to e1 e2s
       then visit e1s' i
       else let result = e1 :: visit e1s' (i + 1)
            in let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself result) in
               result
  in let final_result = visit e1s 1
     in let () = Printf.printf "set_minus %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
        final_result;;

let traced_member_set_minus_v0 show_yourself e1s e2s =
  let () = Printf.printf "set_minus %s %s ->\n" (show_list show_yourself e1s) (show_list show_yourself e2s) in
  let rec visit e1s i =
    let () = Printf.printf "%svisit %s ->\n" (indent i) (show_list show_yourself e1s) in
    match e1s with
    | [] ->
       let () = Printf.printf "%svisit [] <- []\n" (indent i) in
       []
    | e1 :: e1s' ->
       if traced_member show_yourself e1 e2s
       then visit e1s' i
       else let result = e1 :: visit e1s' (i + 1)
            in let () = Printf.printf "%svisit %s <- %s\n" (indent i) (show_list show_yourself e1s) (show_list show_yourself result) in
               result
  in let final_result = visit e1s 1
     in let () = Printf.printf "set_minus %s %s <- %s\n" (show_list show_yourself e1s) (show_list show_yourself e2s) (show_list show_yourself final_result) in
        final_result;;

(* I am going to import the beefed up test case here *)
#use "week-08_exercise-11.ml";;
let () = assert (beefed_up_test_v3 set_union_v1 set_intersection_v1 set_minus_v1);;
let () = assert (beefed_up_test_v3 set_union_v2 set_intersection_v2 set_minus_v2);;


(* ********** *)

(* Subsidary Question *)
let test_normalized_add candidate n =
  let b0 = (candidate 1 [] = [1])
  and b1 = (candidate 1 [2;3;4] = [1;2;3;4])
  and b2 = (candidate 3 [1;2] = [1;2;3])
  and b3 = (candidate 3 [1;2;4;5] = [1;2;3;4;5])
  and b4 = (candidate 3 [1;2;3;4;5] = [1;2;3;4;5])
  and b5 = (let x = Random.int (n + 1) and y = Random.int (n + 1) in
            let random_set = make_random_set y in
            candidate x random_set = sort (set_add x random_set))
  in b0 && b1 && b2 && b3 && b4 && b5;;

let normalized_add e es =
  let () = assert (sort es = es)
  in let rec visit es =
       match es with
       | [] ->
          [e]
       | e' :: es' ->
          if e' < e
          then e' :: visit es'
          else if e' = e
          then es
          else e :: es
     in visit es;;

let () = assert (test_normalized_add normalized_add (Random.int 100));;

(* ********** *)

(* end of week-08_exercise_16.ml *)

"week-08_exercise_16.ml"
