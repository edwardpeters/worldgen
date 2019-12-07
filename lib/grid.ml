(*Like a hashmap, but for fixed-size full or partial regions*)
open Core;;

type size = 
  | SIXTEEN
  | THIRTY_TWO
  | SIXTY_FOUR
  | ONE_TWENTY_EIGHT
  | TWO_FIFTY_SIX

module Sixteen = struct
  let s = SIXTEEN
end
module ThirtyTwo = struct
  let s = THIRTY_TWO
end
module SixtyFour = struct
  let s = SIXTY_FOUR
end
module OneTwentyEight = struct
  let s = ONE_TWENTY_EIGHT
end
module TwoFiftySix = struct
  let s = TWO_FIFTY_SIX
end

module type Size = sig
  val s : size
end

module Grid(S : Size) : sig

  type t
  type loc_t

  module Compass : sig
    type t = N | E | S | W | SE | SW | NE | NW
  end
   
  module Partial : sig
    type 'a t
    val get : 'a t -> loc_t -> 'a option
    val set : 'a t -> loc_t -> 'a -> 'a t
    val count : 'a t -> int
    val remove : 'a t -> loc_t -> 'a t
    val batch_set : 'a t -> (loc_t * 'a) list -> 'a t
    val get_random : 'a t -> (loc_t * 'a)
    val multi_get : 'a t list -> loc_t -> 'a list
    val multi_print : 'a t list -> ('a list -> string) -> unit  
    val tree_print : 'a t -> unit
    val empty : 'a t
  end
  module Full : sig
    type 'a t
    val get : 'a t -> loc_t -> 'a
    val set : 'a t -> loc_t -> 'a -> 'a t
  end

  val grid : t
  val random : unit -> loc_t
  val string_of_loc : loc_t -> string
  val distance : loc_t -> loc_t -> float
  val neighbors : loc_t -> loc_t list
  val orth_neighbors : loc_t -> loc_t list
end = struct

  type t = unit
  type loc_t = int * int
  let grid = ()
  let length = match S.s with
    | SIXTEEN -> 16
    | THIRTY_TWO -> 32
    | SIXTY_FOUR -> 64
    | ONE_TWENTY_EIGHT -> 128
    | TWO_FIFTY_SIX -> 256

  module Compass = struct
    type t = N | E | S | W | SE | SW | NE | NW
  end

  let random () = (Random.int length),(Random.int length) 

  let wrap loc = 
    let rec helper i = 
      if i >= length then helper (i - length)
      else if (i < 0) then helper (i + length)
      else i in
    let (x, y) = loc in helper x, helper y

(*
  let (+loc) a b = 
    let (x1, y1), (x2, y2) = a, b in
    wrap ((x1+x2),(y1+y2))*)

  let string_of_loc loc =
    let x, y = loc in
    String.concat ["("; string_of_int x; "),("; string_of_int y; ")"]

  let distance loc1 loc2 = 
    let wrap = (fun (a':int) (b':int) ->
      min
        (abs (a'- b'))
        (min (abs (a' + length) - b') (abs (b' + length) - a'))) in
    let (x1, y1) = loc1 in
    let (x2, y2) = loc2 in
    let x_dist = wrap x1 x2 in
    let y_dist = wrap y1 y2 in
    sqrt (float_of_int((x_dist * x_dist) + (y_dist * y_dist)))

  let neighbors loc = 
    let x, y = loc in
    [
      wrap (x - 1, y - 1);
      wrap (x - 1, y - 0);
      wrap (x - 1, y + 1);
      wrap (x - 0, y - 1);
      wrap (x - 0, y + 1);
      wrap (x + 1, y - 1);
      wrap (x + 1, y - 0);
      wrap (x + 1, y + 1);
    ] 

  let orth_neighbors loc = 
    let x, y = loc in
    [
      wrap (x - 1, y - 0);
      wrap (x - 0, y - 1);
      wrap (x - 0, y + 1);
      wrap (x + 1, y - 0);
    ] 


  type branch_params = {x_min : int; x_max : int; y_min: int; y_max: int}


  let find (loc:loc_t) params = 
    let (x,y) = loc in
    let x_div = ((params.x_max + params.x_min)/2) in
    let y_div = ((params.y_max + params.y_min)/2) in
    if (x<x_div && y<y_div)
      then 0
    else if (x>=x_div && y<y_div)
      then 1
    else if (x<x_div && y>=y_div)
      then 2
    else
      3 


  module Partial = struct

    type 'a tree =
     | BRANCH of 'a tree list * branch_params
     | LEAF of 'a
     | NONE

    type 'a t = {tree :  'a tree}
    let rec count_tree tree =
     match tree with
       | NONE -> 0
       | BRANCH (l, _) -> List.fold l ~init:0 ~f:(fun i t -> i + (count_tree t))
       | LEAF _ -> 1

    let count t = count_tree t.tree

    let make_empty_tree parent_params i = 
      if (parent_params.x_max-parent_params.x_min = 1) 
        then NONE 
        else
          let x_half = (parent_params.x_min + parent_params.x_max)/2 in
          let y_half = (parent_params.y_min + parent_params.y_max)/2 in
         let x_min,x_max = if (i % 2) = 0 
           then parent_params.x_min, x_half 
           else x_half, parent_params.x_max in
         let y_min,y_max = if (i < 2)
           then parent_params.y_min, y_half 
            else y_half, parent_params.y_max in
          BRANCH ( [NONE;NONE;NONE;NONE],{x_min=x_min;x_max=x_max;y_min=y_min;y_max=y_max})
      
      

    let get (grid : 'a t) (loc : loc_t) =
     let rec search (tree : 'a tree) : 'a option =
       (match tree with
         | NONE -> None
         | LEAF a -> Some a
         | BRANCH (l, params)->
           let quadrant = List.nth_exn l (find loc  params) in
           search quadrant) in
     search grid.tree

    exception Nonsense
    let get_random grid = 
      let rec f (tree : 'a tree) lx ly = 
        (match tree with
          | NONE -> raise Nonsense
          | LEAF a -> ((lx, ly), a)
          | BRANCH (l, params) ->
            (let rec scan l' i =
              match l' with
                | [] -> raise Nonsense
                | x::xs -> if (i < count_tree x) 
                 then f x params.x_min params.y_min
                 else scan xs (i - count_tree x) in
            scan l (Random.int (count_tree tree)))) in
      f grid.tree 0 0

   let set grid loc a =
     let rec search tree: 'a tree =
       match tree with
         | NONE -> LEAF a
         | LEAF _ -> LEAF a
         | BRANCH (l, params)->
            let quadrant = find loc params in
            let current = (List.nth_exn l quadrant) in
            let space = (match current with
              | NONE -> make_empty_tree params quadrant
              | _ -> current) in
            let q = search space in
            BRANCH ((Util.set_nth l quadrant q), params) in
     {tree = search grid.tree}

    let remove t loc = 
      let rec search tree = 
        match tree with
          | NONE -> NONE
          | LEAF _ -> NONE
          | BRANCH (l, params) ->
            let new_branch = 
              let quadrant = find loc params in
              let q = (search (List.nth_exn l quadrant)) in
              BRANCH ((Util.set_nth l quadrant q), params) in
            (*if count new_branch = 0 then NONE else new_branch in*)
            new_branch in
      {tree = search t.tree}

    (*Improve this!*)
    let batch_set grid l =
      let rec f l' grid' = match l' with
        |  [] -> grid'
        |  (loc, a)::xs ->
          let grid'' = set grid' loc a in
          f xs grid'' in
      f l grid
 
    let empty = 
      let params = {x_min = 0; x_max = length; y_min = 0; y_max = length} in
       {tree = BRANCH ([NONE; NONE; NONE; NONE], params) }
  
     let multi_get l loc = 
      let rec filter_map l' = 
        match l' with
          | [] -> []
          | x::xs -> (match get x loc with
            | None -> filter_map xs
            | Some a -> a :: filter_map xs) in
     filter_map l

   let multi_print (l: 'a t list) (f : ('a list -> string)) : unit =
      let range = Util.int_range 0 length in
      let handle_row y = List.map range ~f:(fun i ->  f (multi_get l (i, y))) in
      let rows = List.map range ~f:(fun i -> handle_row i) in
      List.iter rows ~f:(fun row -> print_endline (String.concat row))

   let rec dumb_print t = match t with
    | NONE -> print_string "NONE"
    | LEAF _ -> print_string "LEAF"
    | BRANCH (l, _) -> 
      print_string "BRANCH(";
        List.iter ~f:(fun b ->
        dumb_print b;
        print_string ",") l ;
     print_string ")" 

   let tree_print t = dumb_print t.tree


end

  module Full = struct

    type 'a tree =
      | BRANCH of 'a tree list * branch_params
      | LEAF of 'a

    type 'a t = {depth : int; tree :  'a tree}

    let get t loc =
      let rec search tree =
        match tree with
          | LEAF a -> a
          | BRANCH (l, params)->
           let quadrant = List.nth_exn l (find loc params) in
           search quadrant in
      search t.tree
  
   let set t loc a =
      let rec search tree =
        match tree with
          | LEAF _ -> LEAF a
          | BRANCH (l, params)->
           let quadrant = find  loc params in
           let q = (search (List.nth_exn l quadrant)) in
           BRANCH ((Util.set_nth l quadrant q), params) in
      {t with tree = search t.tree}
 
  end
end




