(* open Core *)
let print x =
  match x with
  | 1 ->
      Day1.answer1 ();
      Day1.answer2 ()
  | 2 ->
      Day2.answer1 ();
      Day2.answer2 ()
  | 3 ->
      Day3.answer1 ();
      Day3.answer2 ();
      (* Day3.answer3 (); *)
  | _ -> Printf.printf "Haven't solved it yet !"
