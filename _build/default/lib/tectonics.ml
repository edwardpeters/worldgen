open Core;;
open Util;;
module MyGrid = Grid.Grid(Grid.SixtyFour)
open MyGrid;;

(*
Assign plate cores/styles
Assign locations to plates (Performance bad!)
Repeat:
  Move plates
  Reconcile
  Erode
??? 
*)

type geology = 
  | OCEAN
  | MOUNTAIN
  | CONTINENT

type square = {index : int; geo : geology}

type plate = {index :int; area : int Partial.t; direction : Compass.t}

let make_plates oceans continents min_distance =
  let locs = Util.constraint_gen
    (fun () -> random grid)
    (oceans + continents)
    (fun a b -> (int_of_float (distance grid a b)) > min_distance) in
  let rec make = (fun l ->
    match l with
      | [] -> []
      | x :: xs ->
        let i = List.length xs in
        let style = (if i > oceans then OCEAN else CONTINENT) in
        let seed_start = let open Partial in
          set empty x {index = i; geo = style} in
        let boundary_start = let open Partial in
          let boundary_locs = neighbors x in
          let boundary = List.map ~f:(fun l -> (l, i)) boundary_locs in
          batch_set empty boundary in
        (seed_start, boundary_start) :: make xs) in
   make locs

let grow boundary plate plates loc i = 
  let can_grow = List.length (Partial.multi_get plates loc) = 0 in
  let grown_plate =
    if can_grow
      then Partial.set plate loc {index = i; geo = CONTINENT}
      else plate in
  let grown_boundary = 
    if can_grow 
      then let open Partial in
      let boundary_locs = orth_neighbors loc in
      let boundary_new = List.map ~f:(fun l -> (l, i)) boundary_locs in
      batch_set boundary boundary_new
    else boundary in
  (grown_plate, Partial.remove grown_boundary loc)

exception Nonsense
let  grow_all (l : ((square Partial.t * int Partial.t) list)) =
  (*Okay, this is an oddity
  To grow a plate, we need:
  Current state of all plates.
  That plate/boundary
  
  Hack: Keep the old plates around as we build the new. This is allowed.*)

  let rec unzip l' = match l' with
    | [] -> [],[]
    | (a,b)::xs -> let (al, bl) = unzip xs in a::al, b::bl in
  let modify l' i = 
    let plates, _ = unzip l' in
    let plate, boundary = List.nth_exn l' i in
    if (Partial.count boundary = 0)
      then l'
    else
      let (loc, index) = Partial.get_random boundary in
      let plate', boundary' = grow boundary plate plates loc index in
      Util.set_nth l' i (plate', boundary') in
  let rec f i l' = 
    if i = List.length l' then l' else f (i+1) (modify l' i) in
  f 0 l

let rec grow_n_steps l i = 
  if i = 0 then l else grow_n_steps (grow_all l) (i-1)

let print (l : (square Partial.t * int Partial.t) list) = 
   let rec unzip l' = match l' with
    | [] -> [],[]
    | (a,b)::xs -> let (al, bl) = unzip xs in a::al, b::bl in
   let plates, _ = unzip l in
   let print l' = match l' with
   | [] -> "_"
   | (x:square)::[] -> string_of_int (x.index)
   | _::_ ->"|" in
   Partial.multi_print plates print

let print_boundaries (l : (square Partial.t * int Partial.t) list) = 
   let rec unzip l' = match l' with
    | [] -> [],[]
    | (a,b)::xs -> let (al, bl) = unzip xs in a::al, b::bl in
   let plates, _ = unzip l in
   let print l' = match l' with
   | [] -> " "
   | _::[] -> "_"
   | _::_ ->"X" in
   Partial.multi_print plates print


let oldGo () =
  let start = make_plates 4 4 4 in
  print start;
  print_endline "";
  let fin = grow_n_steps start 1000 in
  print fin 

let go () = 
  let start = make_plates 4 4 4 in
  print start;
  let rec f (i : int) (state : (square Partial.t * int Partial.t) list) = 
    (let counter entry =(
      let _, boundary = entry in 
      Partial.count boundary) in
    let total = List.fold state ~init:0 ~f:(fun c s -> c + (counter s)) in
    if total = 0 
      then begin
        Printf.printf "Stable at %d moves\n" (800*i);
        print state;
        state 
        end
      else begin
        f (i+1) (grow_n_steps state 100)
    end) in
  let set = f 0 start in
  Util.wait_for_user() ;
  let rec g l = match l with
    | [] -> []
    | (plate, boundary)::xs ->
      let x_adjust = (Random.int 3) - 1 in
      let y_adjust = (Random.int 3) - 1 in
      let new_plate = Partial.translate plate x_adjust y_adjust in
      (new_plate, boundary)::g xs in
  let rec run n state =
    let state' = g state in
    print_endline "";
    if (n%10 = 0) then Util.wait_for_user() else ();
    print_boundaries state';
    if (n=0) then state' else run (n-1) state' in
  run 1000 set
  
  
(*STEPS : 
  Make random grids
  Seed with plates 
  
  TODO: Grow into empty space (and keep border)

  Okay, how does the grow work?
  Options:
    Process Square:
    if valid add to plate, add neighbors to boundary, remove
      else remove
    OR
      For neighbor : if valid add to plate
      Add neighbors to boundary
      Remove from boundary
  
  Then: Handle overlap. How?

  list -> 'a? Check every square per plate per step, and force agreement on result?
   Note - this isn't that bad, if we don't grow our trees then on scale of a regular check
   Task for tomorrow, I think
  
  
  *)

