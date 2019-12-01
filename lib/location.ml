open Core;;

module T = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
  let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
end
include T
include Comparable.Make(T)
 
