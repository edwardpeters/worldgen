open Core;;
open Util;;
module MyGrid = Grid.Grid(Grid.SixtyFour)
open MyGrid;;

type growing_plate = {plate : int Partial.t; border : unit Partial.t; index : int}
type growing_plate_world =
  { plates : growing_plate list;
    oceans : int;
    continents : int;}

module Square : sig
  type t = {plate_index : int; elevation : int}
end = struct
  type t = {plate_index : int; elevation : int}
end

module Plate : sig
  type t = Square.t Partial.t
end = struct
  type t = Square.t Partial.t
end
(*Ultimately:
  World := Plate List
  Plate := Square Partial
  Square : = (Plate ref)*)

let start_growing_plate center index = 
  {index = index; 
  plate = Partial.empty;
  border = Partial.set Partial.empty center ()}

let make_plates c_count o_count dist = 
  let con = (fun a b -> 
    (MyGrid.distance a b) > (float_of_int dist)) in
  let count = c_count + o_count in
  let center_list = constraint_gen MyGrid.random (count) con  in
  let rec helper i l = 
    match l with 
      | [] -> []
      | x::xs -> 
        let plate = start_growing_plate x i in
        plate :: (helper (i+1) xs) in
  let plates = helper 0 center_list in
  {plates = plates; oceans = o_count; continents = c_count}
        
let grow_border_square plate loc world = 
  let plates = List.map world.plates ~f:(fun plate -> plate.plate) in
  let locs = Partial.multi_get plates loc in
  let without_loc = Partial.remove plate.border loc in
  if locs=[]
    then
      let with_square = Partial.set plate.plate loc plate.index in
      let neighbors = orth_neighbors loc in
      let zipped = List.map ~f:(fun l -> (l,())) neighbors in
      let with_border = Partial.batch_set without_loc zipped in
      {plate with border = with_border; plate = with_square}
    else
      {plate with border = without_loc}

let grow_plate plate world = 
  if (Partial.count plate.border) = 0 then plate else
  let (loc,_) = Partial.get_random plate.border in
  begin
    let i = Partial.count plate.border in
    Printf.printf ("Size is %d\n") i;
    grow_border_square plate loc world
  end
 
let grow_world world =
  let rec f world i = 
    if i =  (List.length world.plates)
      then world
      else
        let newplate = grow_plate (List.nth_exn world.plates i) world in
        let newworld = {world with plates = set_nth world.plates i newplate} in
        f newworld (i+1) in
  f world 0
  
let border_size world = 
  let borders = List.map world.plates ~f:(fun plate->plate.border) in
  List.fold borders ~init:0 ~f:(fun i b -> i + Partial.count b)

let draw_growing world =
  let plates = List.map world.plates ~f:(fun plate->plate.plate) in
  let draw l = match l with
    | [] -> " "
    | i::[] -> string_of_int i
    | _::_ -> "X" in
  Partial.multi_print plates draw

let make_starting_plates continents oceans min_distance = 
  let rec f world = 
    if (border_size world = 0) then world else
    begin
      let new_world = grow_world world in
      f new_world
    end in
  draw_growing (f (make_plates continents oceans min_distance))



(*
//make plates -> empty plates
list > plate_type > partial_grid > square (index?)
*)
