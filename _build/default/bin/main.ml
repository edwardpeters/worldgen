open Lib
open Core

let () =
  let result = Util.get_int() in
  let result2 = Util.get_int() in
  print_endline (string_of_int (result + result2))
