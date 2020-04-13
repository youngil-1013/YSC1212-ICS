
(* 4a *)

let list_mapi f ns =
  let rec visit ns i =
    match ns with
    | [] ->
       []
    | n :: ns' ->
       let ih = visit ns' (succ i)
       in f i n :: ih
  in visit ns 0;;

list_mapi (fun n v -> (n, v)) ['a'; 'b'; 'c'];;

(* 4b *)
let test_list_polymorphic_identity_int candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate [0] = [0])
  and b2 = (candidate [1; 0] = [1; 0])
  and b3 = (candidate [1; 1; 0] = [1; 1; 0])	   
  and b4 = (let n = 4
            and ns' = [3; 2; 1; 0]
            in let ih = candidate ns'
               in candidate (n :: ns') = n :: ih)
  in bc && b1 && b2 && b3 && b4;;

let test_list_polymorphic_identity_str candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate ["were"] = ["were"])
  and b2 = (candidate ["byte"; "we"] = ["byte"; "we"])
  and b3 = (candidate ["how";"go";"cut"] = ["how";"go";"cut"])	   
  and b4 = (candidate ["same";"fed";"umbrella";"low"] = ["same";"fed";"umbrella";"low"])
  in bc && b1 && b2 && b3 && b4;;

let test_list_polymorphic_identity_bool candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate [true] = [true])
  and b2 = (candidate [false; false] = [false; false])
  and b3 = (candidate [true;false;true] = [true;false;true])	   
  and b4 = (candidate [false;true;true;false] = [false;true;true;false])
  in bc && b1 && b2 && b3 && b4;;

let list_polymorphic_identity s = 
  list_mapi (fun n v -> v) s;;

test_list_polymorphic_identity_int list_polymorphic_identity;;
test_list_polymorphic_identity_str list_polymorphic_identity;;
test_list_polymorphic_identity_bool list_polymorphic_identity;;

(* 4c *)

 let test_list_increase_indices_int candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate [0] = [0])
  and b2 = (candidate [1; 0] = [0; 1])
  and b3 = (candidate [1; 1; 0] = [0; 1; 2])	   
  and b4 = (candidate [23;58;8;34;67] = [0;1;2;3;4])
  in bc && b1 && b2 && b3 && b4;;

let test_list_increase_indices_str candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate ["were"] = [0])
  and b2 = (candidate ["byte"; "we"] = [0; 1])
  and b3 = (candidate ["a";"b";"c"] = [0;1;2])	   
  and b4 = (candidate ["same";"fed";"umbrella";"low"] = [0;1;2;3])
  in bc && b1 && b2 && b3 && b4;;

let test_list_increase_indices_bool candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate [true] = [0])
  and b2 = (candidate [false; false] = [0;1])
  and b3 = (candidate [true;false;true] = [0;1;2] )	   
  and b4 = (candidate [false;true;true;false] = [0;1;2;3])
  in bc && b1 && b2 && b3 && b4;;

 let list_increase_indices s = 
   list_mapi (fun n v -> n) s;;

test_list_increase_indices_int list_increase_indices;;
test_list_increase_indices_str list_increase_indices;;
test_list_increase_indices_bool list_increase_indices;;

 (* 4c *)

 let test_list_decrease_indices_int candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate [0] = [0])
  and b2 = (candidate [1; 0] = [1;0])
  and b3 = (candidate [1; 1; 0] = [2; 1; 0])	   
  and b4 = (candidate [23;58;8;34;67] = [4;3;2;1;0])
  in bc && b1 && b2 && b3 && b4;;

let test_list_decrease_indices_str candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate ["were"] = [0])
  and b2 = (candidate ["byte"; "we"] = [1;0])
  and b3 = (candidate ["a";"b";"c"] = [2;1;0])	   
  and b4 = (candidate ["same";"fed";"umbrella";"low"] = [3;2;1;0])
  in bc && b1 && b2 && b3 && b4;;

let test_list_decrease_indices_bool candidate =
      (* an instance of the base case: *)
  let bc = (candidate [] = [])
      (* a few handpicked lists: *)
  and b1 = (candidate [true] = [0])
  and b2 = (candidate [false; false] = [1;0])
  and b3 = (candidate [true;false;true] = [2;1;0] )	   
  and b4 = (candidate [false;true;true;false] = [3;2;1;0])
  in bc && b1 && b2 && b3 && b4;;

let list_decrease_indices s = 
   list_mapi (fun n v -> List.length s - n - 1) s;;

test_list_decrease_indices_int list_decrease_indices;;
test_list_decrease_indices_str list_decrease_indices;;
test_list_decrease_indices_bool list_decrease_indices;;
