open Core;;
open Util;;
module MyGrid = Grid.Grid(Grid.SixtyFour)
open MyGrid;;

type plate_type = CONTINENTAL | OCEANIC
type plate = {center : loc_t; plate_type : plate_type; index : int}

let start_plate center plate_type index = 
  {center = center; plate_type = plate_type; index = index;}

let make_plates c_count o_count dist = 
  let con = (fun a b -> 
    (MyGrid.distance a b) < (float_of_int dist)) in
  let count = c_count + o_count in
  let center_list = constraint_gen MyGrid.random (count) con  in
  let rec helper i l = 
    match l with 
      | [] -> []
      | x::xs -> 
        let plate_type = (if i < o_count then OCEANIC else CONTINENTAL) in
        let plate = start_plate x plate_type i in
        plate :: (helper (i-1) xs) in
   helper count center_list 
        



(*
Assign plate cores/styles
Assign locations to plates (Performance bad!)
Repeat:
  Move plates
  Reconcile
  Erode
??? 
*)
