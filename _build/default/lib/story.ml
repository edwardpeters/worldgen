
open Core;;

(*I need multiple levels of continuation.*)
type 'a continuation = ('a -> 'a)

type 'a story =
  FIN   
  | END
  | MSG of string
  | INFO of ('a -> string)
  | SEQ of ('a story list)
  | BRANCH of ('a story list * ('a -> int))
  | PICK of (('a story * string) list)
  | PICK_FILTERED of ('a story * ('a -> bool) * string) list
  | RANDOM of ('a story list)
  | RANDOM_FILTERED of ('a story * ('a -> bool)) list
  | WHILE of (('a -> bool) * 'a story)
  | IF of (('a -> bool) * 'a story * 'a story)
  | GATE of 'a story
  | MOD of ('a -> 'a)

let rec run  (stry : 'a story) (c :'a continuation) (state : 'a) : 'a = 
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
      let fold_opr : (('a story)-> ('a continuation) -> ('a continuation)) =
        fun stry' c'-> (fun state' -> run stry' c' state') in
      List.fold_right lst ~init:c ~f:fold_opr in
   seq state (*Wow! Test that.*)
  | BRANCH (lst, f) -> let i = f state in 
    let stry' = List.nth_exn lst i in
    run stry' c state
  | PICK lst -> 
    begin
      for i = 0 to (List.length lst) do
        let (_, s) = List.nth_exn lst i in
        Printf.printf "%i: %s\n" i s
      done;
      print_endline "Pick an option from above." ;
      let (stry', _) = (List.nth_exn lst (Util.get_int_bounded (List.length lst))) in
      run stry' c state
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
      run stry' c state
    end
  | RANDOM lst ->
    let n = Random.int (List.length lst) in
    run (List.nth_exn lst n) c state 
  | RANDOM_FILTERED lst ->
    let flst = List.filter lst ~f:(fun (_, f) -> f state) in
    let n = Random.int (List.length flst) in
    let (stry', _) = (List.nth_exn lst n) in
    run stry' c state
  | IF (test, tStory, fStory) -> 
    if (test state) then run tStory c state else run fStory c state
  | WHILE (test, loopStory) ->
    let rec c' state' = 
      if (test state') then run loopStory c' state' else c state'  in
    c' state
  | GATE stry' -> 
    let rec f : 'a continuation  = (fun state' -> run stry' f state') in
    f state
  | MOD f -> c (f state) 

let start (stry: 'a story) (start_state : 'a)  : 'a = 
  let no_continue (state' : 'a) : 'a = state' in
  run stry no_continue start_state

let random_test =  WHILE ((fun f -> f > 5),
                       SEQ[MOD (fun x -> x - 1); (RANDOM [MSG "Hoo"; MSG "Hah"])])

let random_filter_test = WHILE ((fun x -> x > 0),
                        SEQ[MOD (fun x -> x -1);
                          RANDOM_FILTERED[
                            (MSG ">5", (fun x -> x > 5));
                            (MSG ">10", (fun x -> x > 10));
                            (MSG ">15", (fun x -> x > 15));
                            (MSG ">20", (fun x -> x > 20));
                            (MSG ">25", (fun x -> x > 25));]])


let filter_pick_test = WHILE ((fun x -> x > 0),
                        SEQ[MOD (fun x -> x -1);
                          PICK_FILTERED[
                            (MSG ">10", (fun x -> x > 10), ">10!");
                            (MSG ">15", (fun x -> x > 15), ">15!");
                            (MSG ">20", (fun x -> x > 20), ">20!");
                            (MSG ">25", (fun x -> x > 25), ">25");]])


