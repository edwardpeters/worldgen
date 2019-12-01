
(*This comment is precisely 80 characters long, write nothing longer than this*)
(*Core: Functional vs. imperative  *)

type expression = 
  | INT of int
  | SUM of expression * expression


let x : expression = SUM (
  (INT 7),
  (SUM(
    (INT 5),
    (INT 6))))
type instruction = 
  | ASSIGNMENT of string * expression 
  | IF_0 of expression * instruction
  | SEQ of instruction * instruction


let example_list = [1 ; 2; 3] ;;

(* 1 :: [2, 3]
 2 :: [3]
 3 :: EMPTY *)

type 'a ned_list =
  | EMPTY
  | CONS of 'a * 'a ned_list


let add_five x = 
  5 + x

let seq x y =
  x;
  y;
  ()

type bool = true | false
type unit = ()

let add_six x= 
  let y = 6 in
  x + y
  

(*
  return x; *)

 (* int_list equals ned_list(int) *)

(* x ; y = y *)
