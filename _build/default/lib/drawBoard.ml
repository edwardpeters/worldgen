open Core;;

let f x = x * x



let myMap() =
  let open GraphMap in
  make_random 1500 1500 50. 65. 600


let draw someMap  =
  let open GraphMap in
  Graphics.open_graph " 1500x2000";
  let drawLine = (fun ((a : Location.t), (b : Location.t)) ->
    begin 
      Graphics.moveto a.x a.y;
      Graphics.lineto b.x b.y;
    end) in
  let drawCircle = (fun (a : Location.t) ->
      begin
        Graphics.moveto a.x a.y;
        Graphics.draw_string "Wb";
        Graphics.draw_circle a.x a.y 5;
      end) in
  begin
    List.iter someMap.adjacencies ~f:drawLine;
    List.iter someMap.locations ~f:drawCircle;
    Graphics.moveto 10 10;
    Graphics.lineto 1490 10;
    Graphics.lineto 1490 1990;
    Graphics.lineto 10 1990;
    Graphics.lineto 10 10;
  end
