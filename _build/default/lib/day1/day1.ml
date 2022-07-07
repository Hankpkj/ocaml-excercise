open Core
open Utils

let txt =
  Lists.split_with_char_list [ '\n' ] (Read.file "../sample/aoc_2021_day1.txt")

let int_list = map (fun x -> int_of_string x) txt

open Int

(* day 1 - 1번 문제 *)
let rec increase_count l prev acc =
  match l with
  | [] -> acc
  | h :: t ->
      increase_count t h (match h > prev with true -> acc + 1 | false -> acc)
;;

increase_count int_list max_value 0

(* day 1 - 2번 문제 *)
let rec count_by_three_words l =
  match l with
  | h1 :: (h2 :: h3 :: h4 :: _ as t) -> (
      match h1 + h2 + h3 < h2 + h3 + h4 with
      | true -> 1 + count_by_three_words t
      | false -> count_by_three_words t)
  | _ -> 0

let answer1 () =
  Printf.printf "day1_1 answer is %d" (increase_count int_list max_value 0)

let answer2 () =
  Printf.printf "day1_2 answer is %d" (count_by_three_words int_list)
