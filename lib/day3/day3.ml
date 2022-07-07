(* open Core *)
open Utils

let txt =
  Lists.split_with_char_list [ '\n' ] (Read.file "../sample/aoc_2021_day3.txt")

let list_of_list = map Lists.from_string txt

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: map List.hd xss) :: transpose (xs :: map List.tl xss)

let transposed = transpose list_of_list

let rec count (zero, one) l =
  match l with
  | [] -> if zero < one then '0' else '1'
  | x :: xs -> count (if x = '0' then (zero + 1, one) else (zero, one + 1)) xs

let gamma = map (count (0, 0)) transposed
(* gamma 는 [ '0' , '1', '1', '0' , ... ] 과 같은 list of char 인 상태 *)

let reverse_char c =
  match c with '0' -> '1' | '1' -> '0' | _ -> failwith "invalid input"

let epsilon = map reverse_char gamma
let gamma_string = Base.String.of_char_list gamma
let epsilon_string = Base.String.of_char_list epsilon
let gamma_int = int_of_string ("0b" ^ gamma_string)
let epsilon_int = int_of_string ("0b" ^ epsilon_string)
let answer1 () = Printf.printf "day2_2 answer is %d\n" (gamma_int * epsilon_int)
