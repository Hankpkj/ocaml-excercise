open Core

let day1_txt = In_channel.read_all "../sample/aoc_2021_day1.txt"

let text_split x = String.split_on_chars ~on:[ '\t'; '\n'; '\r' ] x
let rec map f l = match l with [] -> [] | h :: t -> f h :: map f t
let int_list = map (fun x -> int_of_string x) (text_split day1_txt)

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
;;

count_by_three_words int_list
