open Core

let text_split x = String.split_on_chars ~on:[ '\t'; '\n'; '\r' ] x
let rec map f l = match l with [] -> [] | h :: t -> f h :: map f t

(* day2 시작 *)
let day2_txt = In_channel.read_all "../sample/aoc_2021_day2.txt"
let space_split x = String.split_on_chars ~on:[ ' ' ] x
let list_of_list = map (fun x -> space_split x) (text_split day2_txt)

type direction = UP | DOWN | FORWARD | NONETYPE

let string_to_variant x =
  match x with
  | "up" -> UP
  | "down" -> DOWN
  | "forward" -> FORWARD
  | _ -> NONETYPE

let list_to_tuple l =
  match l with
  | [| direction; step |] ->
      Some (string_to_variant direction, int_of_string step)
  | _ -> None

let list_of_tuple =
  List.filter_map list_of_list ~f:(fun x -> list_to_tuple (Array.of_list x))

(* day2 - 1번 문제 *)
let move_with_values direction step depth horizontal =
  match direction with
  | UP -> (depth - step, horizontal)
  | DOWN -> (depth + step, horizontal)
  | FORWARD -> (depth, horizontal + step)
  | NONETYPE -> (depth, horizontal)

let rec move l (d, h) =
  match l with
  | [] -> d * h
  | (d', s') :: t -> move t (move_with_values d' s' d h)
;;

move list_of_tuple (0, 0)

(* day2 - 2번 문제 *)
let move_with_aim direction step depth horizontal aim =
  match direction with
  | UP -> (depth, horizontal, aim - step)
  | DOWN -> (depth, horizontal, aim + step)
  | FORWARD -> (depth + (aim * step), horizontal + step, aim)
  | NONETYPE -> (depth, horizontal, aim)

let rec move2 l (depth, horizontal, aim) =
  match l with
  | [] -> depth * horizontal
  | (direction, step) :: t ->
      move2 t (move_with_aim direction step depth horizontal aim)
;;

move2 list_of_tuple (0, 0, 0)
