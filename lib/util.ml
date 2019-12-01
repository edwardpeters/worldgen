open Core;;
Random.self_init();;

let get_string () : string = 
  begin
  Out_channel.flush Out_channel.stdout;
  Option.value_exn (In_channel.input_line In_channel.stdin)
  end

let rec get_int () : int = 
   try int_of_string (get_string ())
   with Failure _ ->
    begin
      print_endline "Try again";
      get_int()
    end

let rec get_int_bounded (bound: int) : int = 
  begin
    assert (bound > 0);
    try 
      let x= int_of_string (get_string ()) in
      if (x >=0 ) && (x < bound) then x else raise (Failure "Damnit!")
    with Failure _ ->
      begin
        Printf.printf "Enter a number between 0 and %i.\n" (bound - 1);
        get_int_bounded bound
      end
  end

let wait_for_user () : unit = 
  let _ = get_string() in () 

let rec print_all (lst : string list) : unit = 
  match lst with
    | [] -> ()
    | s :: xs -> print_endline s; print_all xs

let pick_random (lst : 'a list) : 'a =
  let len = List.length lst in
  List.nth_exn lst (Random.int len)

let roll (a : int) (b : int) : bool = 
  (Random.int b) < a

let int_range (a : int) (b : int) = 
  assert (a <= b) ;
  let rec f x = if (x = b)
    then []
    else x :: f (x+1) in
  f a

exception ListSetOutOfRange
let rec set_nth (l : 'a list) (i : int) (a : 'a) :'a list =
  match l with
    | [] ->  raise ListSetOutOfRange
    | x::xs -> 
      if i = 0
        then a :: xs
        else x :: (set_nth xs (i-1) a)

let constraint_gen
  (gen : unit -> 'a)
  (i : int)
  (con : 'a -> 'a -> bool) : 'a list =
    let rec good = (fun l c -> match l with
      | [] -> true
      | x::xs -> if (con c x)
        then good xs c
        else false) in
    let rec f i' = (fun l ->
      if (i' = 0)
        then l
        else let candidate = gen() in
          if good l candidate
            then f (i'- 1)(candidate :: l)
            else f i' l)
    in f i []

          
