
open Core;;

(*I need multiple levels of continuation.*)
type 'a continuation = ('a -> 'a)

(*
StoryBase.t


 *)

type 'a t = ..
type 'a t +=
  FIN   
  | END
  | MSG of string
  | INFO of ('a -> string)
  | SEQ of ('a t list)
  | BRANCH of ('a t list * ('a -> int))
  | PICK of (('a t * string) list)
  | PICK_FILTERED of ('a t * ('a -> bool) * string) list
  | RANDOM of ('a t list)
  | RANDOM_FILTERED of ('a t * ('a -> bool)) list
  | WHILE of (('a -> bool) * 'a t)
  | IF of (('a -> bool) * 'a t * 'a t)
  | GATE of 'a t
  | MOD of ('a -> 'a)

let rec run  
  (stry : 'a t) 
  (c :'a continuation) 
  (state : 'a) 
  (x_run : 'a t -> 'a continuation -> 'a -> 'a) : 'a = 
  match stry with
  FIN -> state (*I think we just return here. *)
  | END -> c state
  | MSG text -> 
    begin 
      print_endline text;
      Util.wait_for_user();
      c state
    end
  | INFO (f) -> 
    begin
      print_endline (f state);
      Util.wait_for_user();
      c state;
    end
  | SEQ lst -> (*Whew boy. Execute each, with the next as its continuation?*)
    let seq : 'a continuation  = 
      let fold_opr : (('a t)-> ('a continuation) -> ('a continuation)) =
        fun stry' c'-> (fun state' -> run stry' c' state' x_run) in
      List.fold_right lst ~init:c ~f:fold_opr in
   seq state (*Wow! Test that.*)
  | BRANCH (lst, f) -> let i = f state in 
    let stry' = List.nth_exn lst i in
    run stry' c state x_run
  | PICK lst -> 
    begin
      for i = 0 to (List.length lst) do
        let (_, s) = List.nth_exn lst i in
        Printf.printf "%i: %s\n" i s
      done;
      print_endline "Pick an option from above." ;
      let (stry', _) = (List.nth_exn lst (Util.get_int_bounded (List.length lst))) in
      run stry' c state x_run
      end
  | PICK_FILTERED lst ->
    begin
      let flst = List.filter lst ~f:(fun (_, f, _) -> f state) in
      for i = 0 to (List.length flst)-1 do
       let (_,_, s) = List.nth_exn flst i in
       Printf.printf "%i: %s\n" i s
      done;
      print_endline "Pick an option from above." ;
      let (stry',_, _) = (List.nth_exn flst (Util.get_int_bounded (List.length flst))) in
      run stry' c state x_run
    end
  | RANDOM lst ->
    let n = Random.int (List.length lst) in
    run (List.nth_exn lst n) c state x_run
  | RANDOM_FILTERED lst ->
    let flst = List.filter lst ~f:(fun (_, f) -> f state) in
    let n = Random.int (List.length flst) in
    let (stry', _) = (List.nth_exn lst n) in
    run stry' c state x_run
  | IF (test, tStory, fStory) -> 
    if (test state) then run tStory c state x_run else run fStory c state x_run
  | WHILE (test, loopStory) ->
    let rec c' state' = 
      if (test state') then run loopStory c' state' x_run else c state'  in
    c' state
  | GATE stry' -> 
    let rec f : 'a continuation  = (fun state' -> run stry' f state' x_run) in
    f state
  | MOD f -> c (f state) 
  | x -> x_run x c state

type 'a t +=
  | HIGH_FIVE

let rec x_r stry cont state = match stry with
  |HIGH_FIVE -> print_endline "High five!"; cont state
  | x -> run x cont state x_r


let simple_state = 0
let test_story = RANDOM [ HIGH_FIVE; SEQ [MSG "Cool!"; MSG "Neat!"]]
let test() = run test_story (fun a -> a) simple_state x_r

(*Design tasks on the list:

Continuations:
  We really want continuations to be scrollable. 
Assurances - NOT YET! Not until you have your first bug. 

*)
