
(*This comment is precisely 80 characters long, write nothing longer than this*)
(*Conditions with multiple branches: Maintain indentation, same as pattern matching*)

(*begin end for statements?: Yes, and also nested matches*)

(*First vertical bar on pattern matchings*)

(*Quit your fucking camel casing*)

(*Note : increment x * y = (x +1)*y, not (x * y) + 1*)

(*
module BasicStory = struct
  type ('a, 'b) t =
    | PRINT of string
    | IF of ('a -> bool) * ('a, 'b) t * ('a, 'b) t
    | X of 'b

  
  let rec run (story : ('a, 'b) t) (state : 'a)  (x_run : 'b -> 'a -> 'a) : 'a 
    = match story with
    | PRINT str -> begin print_endline str; state end
    | IF (test, story1, story2) -> 
      if test state then run story1 state x_run else run story2 state x_run
    | X x_story -> x_run x_story state 

end

module type StoryExt = sig
  type t
  type state_t

  val run:  t -> state_t -> state_t
end 

module type ExtendedStory = sig
  type t
  type state_t 
 
  val run : t -> state_t -> state_t 
end

module SeqExt = struct
  type state_t = int
  type t = SEQ of ((state_t, t) BasicStory.t) * ((state_t, t) BasicStory.t)
  
  let rec run story state = 
    let base_run = (fun story1 state1 -> BasicStory.run story1 state1 run) in
  match story with
    |SEQ(story1, story2) -> 
      let state1 = base_run story1 state in
      base_run story2 state1

end

module ExtendStory (Ext : StoryExt) 
  : (ExtendedStory with type t := (Ext.state_t, Ext.t) BasicStory.t and type state_t :=Ext.state_t) = struct 
  type t = (Ext.state_t, Ext.t) BasicStory.t
  type state_t = Ext.state_t

  let run (story : t) (state : state_t) = (BasicStory.run story state Ext.run)
end
   
module StoryWithSeq = ExtendStory (SeqExt)

let awfulStory = BasicStory.X(SeqExt.SEQ(
  (BasicStory.PRINT "Fuck you"),
  (BasicStory.PRINT "And al of you too")))

let fuck_all_yall () = StoryWithSeq.run awfulStory 5
*)
module StoryBase = struct
  type 'a t = ..;;
  type 'a t +=
  | PRINT of string
  | SEQ of ('a t * 'a t)
  | IF of ('a  -> bool) * 'a t * 'a t
  | PARTY
  let rec f (story: 'a t) (state: 'a) (x_run: ('a t -> 'a -> 'a)) : 'a  = match story with
    | PRINT s -> begin print_endline s; state end
    | SEQ (story1, story2) -> 
      let state1 = f story1 state x_run in
      f story2 state1 x_run
    | IF (test, story1, story2) ->
      if test state then f story1 state x_run else f story2 state x_run
    | PARTY -> print_endline "Party!"; state
    | x -> x_run x state
end

module IntExtend = struct
  open StoryBase
  type 'a t += PRINT_REPEAT of string | REPEAT of 'a t
  let rec f (story : int t) (state : int) : int = match story with
    | PRINT_REPEAT s ->  
      for _ = 0 to state do
        print_endline s;
      done;
      state 
    | REPEAT story1 ->
      for i = 0 to state do
        let _ = f story1 i in ()
      done;
      state
    | x -> StoryBase.f x state f
end;;
module BoolExtend = struct
  open StoryBase
  type 'a t += SIMPLE_IF of ('a t * 'a t) | IF of ('a t * 'a t) | PARTY
  let rec f (story : bool t) (state : bool) : bool = match story with
    | IF (story1, story2) -> if state then f story1 state else f story2 state
    | SIMPLE_IF (story1, story2) -> if state then f story1 state else f story2 state  
    | PARTY -> print_endline "PAAARTY!"; state
    | x -> StoryBase.f x state f
end;;

(*
let open StoryBase in
let open IntExtend in
let weirdExtend = SEQ(
    PRINT_REPEAT "Woot!",
    PRINT_REPEAT "Booyah!") in
let weirderExtend = REPEAT weirdExtend in
f weirderExtend 5 *)

module type StoryExt = sig
  type state_t
  type t
end 
module ExtendStory (Ext : StoryExt) = struct
  
end

(*ANY advantage to using a functor instead?)

Answer: Only if we intend that this be applicable to multiple base types. If we need that functionality later, add it later. *)
