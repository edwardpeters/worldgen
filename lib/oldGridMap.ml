open Core;;

(* Okay, design


 *)

module Location = struct
  module T = struct
    type t = int * int
    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
  end
  include T
  include Comparable.Make(T)
end

module type Area = sig
  type t
  val string_of_area : t ->  string
end

module Grid(A : Area) = struct
  type area_t = {area : A.t; loc : Location.t}
  type map_t = (Location.t, area_t, Location.comparator_witness) Base.Map.t
  type t = {
    grid_map : map_t;
    x : int;
    y : int;
  }
  let string_of_area = A.string_of_area  
  let map (grid : t) (f : area_t -> area_t) : t =
    let rec handle_square = (fun x' y' (m : map_t) ->
      if (y' = grid.y)
        then m
      else if (x' = grid.x)
        then handle_square 0 (y' + 1) m
      else
        let key = (x', y') in
        let data = f (Map.find_exn m key) in
        handle_square (x'+1) y' (Map.set m ~key:key ~data:data)) in
    {grid with grid_map = handle_square 0 0 grid.grid_map}

  let build (x : int)(y : int) (f : Location.t -> A.t) : t =
    let rec handle_square = (fun x' y' (m : map_t) ->
      if (y' = y)
        then m
      else if (x' = x)
        then handle_square 0 (y' + 1) m
      else
        let open Location in
        let (loc:Location.t) = (x',y') in
        let area = {loc = loc; area = f loc} in
        handle_square (x'+1) y' (Map.set m ~key:loc ~data:area)) in
      let empty = Map.empty(module Location) in
    {grid_map = handle_square 0 0  empty;
    x = x;
    y = y;}

  let iter (grid : t) (f : area_t -> unit) : unit =
    let rec handle_square = (fun x' y' (m : map_t) ->
      if (y' = grid.y)
        then ()
      else if (x' = grid.x)
        then handle_square 0 (y' + 1) m
      else
        let key = (x', y') in
        let data = (Map.find_exn m key) in
        begin
          f data;
          handle_square (x'+1) y' m;
       end ) in
    handle_square 0 0 grid.grid_map

  let print (grid : t) : unit =
    let rec handle_square = (fun x' y' (m : map_t) ->
      if (y' = grid.y)
        then ()
      else if (x' = grid.x)
        then begin
          print_endline "";
          handle_square 0 (y' + 1) m;
        end
      else
        let key = (x', y') in
        let data = (Map.find_exn m key) in
        begin
          print_string (string_of_area data.area);
          handle_square (x'+1) y' m;
       end ) in
    handle_square 0 0 grid.grid_map

    (*Wraparound? Sure.*)
    let distance (grid : t) (a : Location.t) (b : Location.t) : float = 
      let wrap = (fun a' b' width  ->
        min
          (abs a'-b')
          (min (abs (a' + ((width -1) - b')))
          (abs (b' + ((width -1) - a'))))) in
       let (xa, ya) = a in
       let (xb, yb) = b in
       let x_dist = wrap xa xb grid.x in
       let y_dist = wrap ya yb grid.y in
       sqrt (float_of_int((x_dist * x_dist) + (y_dist * y_dist)))

    let random (grid : t) : Location.t = 
      (Random.int (grid.x)), (Random.int (grid.y))

    let neighbors (grid : t) (area : area_t) : area_t list =
      let get = (fun x y ->
        Map.find_exn grid.grid_map ((x % grid.x), (y % grid.y))) in
      let (x, y) = area.loc in
      [
        get (x - 1) (y - 1);
        get (x - 1) (y + 0);
        get (x - 1) (y + 1);
        get (x + 0) (y - 1);
        get (x + 0) (y + 0);
        get (x + 0) (y + 1);
        get (x + 1) (y - 1);
        get (x + 1) (y + 0);
        get (x + 1) (y + 1);
      ]      
      

end

