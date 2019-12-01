open Core

(*What is a map?*)
(*For now - locations on a grid, with adjacencies determined by something. 
Look up generation algorithms?

Edge crossing turns out to be hard. 
Use max distance = sqrt2*min_distance for now.*)
(*This comment is precisely 80 characters long; write nothing longer than this*)

module Location = struct
  type t = {x : int; y : int}

  let random (x_size: int) (y_size: int) 
    = {x = Random.int x_size; y = Random.int y_size}

  let distance (first: t) (second : t) : float =
    let xdist = abs (first.x - second.x) in
    let ydist = abs (first.y - second.y) in
    sqrt ( float_of_int (xdist * xdist) +. float_of_int (ydist * ydist))


end

type t = {locations : Location.t list;
   adjacencies : (Location.t * Location.t) list; 
   min_distance : float}

let print_pair first second =
  let open Location in
  Printf.printf "(%i, %i) --(%i, %i)\n" first.x first.y second.x second.y


let rec fits (dist: float) (loc : Location.t) (lst : Location.t list) 
  = match lst with
  | [] -> true
  | x::xs -> if (Location.distance loc x) < dist 
    then false
    else fits dist loc xs

let rec close_enough (dist: float) (loc : Location.t) (lst : Location.t list) 
  = match lst with
  | [] -> false
  | x::xs -> if (Location.distance loc x) < dist 
    then 
      begin
        print_pair loc x;
        true
      end
    else close_enough dist loc xs


let rec make_point min_dist max_dist x_size y_size lst attempts  =
  assert (attempts > 0);
  let candidate = Location.random x_size y_size in
  if (fits min_dist candidate lst) && (close_enough max_dist candidate lst)
    then 
      candidate
    else
      make_point min_dist max_dist x_size y_size lst (attempts-1)

(*Return a list of all pairs closer to each other than sqrt2*min distance;
This is a lazy way to yield a non-crossing graph*)
  
let make_edges locs dist =
  let max_dist = (sqrt 2.) *. dist in
  begin
    Printf.printf "Edge distance : %f)" max_dist;
    let rec f = (fun loc loc_lst edge_lst ->
     (*f maps a location to a list of pairs of valid edges*)
      match loc_lst with
        | [] -> edge_lst
       | x::xs ->  if Location.distance x loc < max_dist
         then f loc xs ((loc, x) :: edge_lst)
         else f loc xs edge_lst) in
   let rec g = (fun loc_lst edge_lst ->
      match loc_lst with
        | [] -> edge_lst
       | x::xs -> f x xs (g xs edge_lst)) in
    g locs []
  end


let make_random 
  (x_size : int) 
  (y_size : int) 
  (min_distance : float) 
  (max_distance : float)
  (max_points : int) 
  : t =
  let rec folder = fun i lst ->
    if i = 0
      then lst
      else 
        let new_lst = (make_point min_distance max_distance x_size y_size lst 5000) :: lst in
        folder (i-1) new_lst in
  let first = Location.random x_size y_size in
  let locs = folder max_points [first] in
  let edges = make_edges locs min_distance in
  {locations = locs; adjacencies = edges; min_distance = min_distance}

let print (map : t) =
  List.iter 
    map.locations 
    ~f:(fun loc ->
      (Printf.printf "(%i, %i)\n" loc.x loc.y));
  List.iter 
    map.adjacencies 
    ~f:(fun (first, second) -> 
         (Printf.printf "(%i, %i)--(%i, %i)\n" first.x first.y second.x second.y))
  
