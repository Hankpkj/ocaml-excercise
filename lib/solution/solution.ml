(* open Core *)
let print x =
  match x with
  | 1 ->
      Day1.answer1 ();
      Day1.answer2 ()
  | 2 ->
      Day2.answer1 ();
      Day2.answer2 ()
  | _ -> Printf.printf "Haven't solved it yet !"
