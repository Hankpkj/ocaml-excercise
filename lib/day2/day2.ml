open Core
open Utils

let txt =
  Lists.split_with_char_list [ '\n' ] (Read.file "../sample/aoc_2021_day2.txt")

(* day2 시작 *)
let list_of_list = map (Lists.split_with_char_list [ ' ' ]) txt

type direction = UP | DOWN | FORWARD

let string_to_variant x =
  match x with
  | "up" -> UP
  | "down" -> DOWN
  | "forward" -> FORWARD
  | _ -> failwith "invalid input"

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

let rec move l (d, h) =
  match l with
  | [] -> d * h
  | (d', s') :: t -> move t (move_with_values d' s' d h)

let answer1 () =
  Printf.printf "day2_1 answer is %d\n" (move list_of_tuple (0, 0))

(* day2 - 2번 문제 *)
let move_with_aim direction step depth horizontal aim =
  match direction with
  | UP -> (depth, horizontal, aim - step)
  | DOWN -> (depth, horizontal, aim + step)
  | FORWARD -> (depth + (aim * step), horizontal + step, aim)

let rec move2 l (depth, horizontal, aim) =
  match l with
  | [] -> depth * horizontal
  | (direction, step) :: t ->
      move2 t (move_with_aim direction step depth horizontal aim)

let answer2 () =
  Printf.printf "day2_2 answer is %d\n" (move2 list_of_tuple (0, 0, 0))
