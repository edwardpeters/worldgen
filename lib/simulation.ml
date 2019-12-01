open Core;;

(*Ugh. This problem.

Okay, what sorts of things and what sorts of interactions are going in our map?

Grid or network?

Grid: Larger entities, LOTS of locations (close to a billion), lots of connections.
Cooler world, more Dwarf Fortress

Network: Just plain easier I guess.


Assuming network, we have:

"Rooms" that can update themselves each time step, update the creatures in them, and be updated by the creatures

Rooms can influence adjacent rooms. (WHAT?)

Creatures that can move between rooms

Okay, so at the top level...

map string -> room. Rooms are uh... trees?

map string -> creature, creatures are trees
map room -> creature?

phase one: Rooms update THEMSELVES
  Rooms build a list of creatures by checking the map, then treat that as a temp tree?
  

phase two: Creatures move? Creatures update room list

 *)

module MyUsers = Map.Make(String);;
let m = MyUsers.empty;;
let empty = Map.empty (module String)
let n = Map.set empty ~key:"What" ~data:"What?"
let o = Map.set n ~key:"Huh" ~data:"Huh?" 
let what = Map.find_exn o "What"
