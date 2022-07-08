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

let find f l =
  let rec find_loop (zero, one) l =
    match l with
    | [] -> if f zero one then '0' else '1'
    | x :: xs ->
        find_loop (if x = '0' then (zero + 1, one) else (zero, one + 1)) xs
  in
  find_loop (0, 0) l

let bigger = find (fun x y -> x > y)
let lower = find (fun x y -> x <= y)
let gamma_char_list = map bigger transposed
(* gamma 는 [ '0' , '1', '1', '0' , ... ] 과 같은 list of char 인 상태 *)

let reverse_char c =
  match c with '0' -> '1' | '1' -> '0' | _ -> failwith "invalid input"

let char_to_int cl = int_of_string ("0b" ^ Base.String.of_char_list cl)
let gamma = char_to_int gamma_char_list
let epsilon = char_to_int (map reverse_char gamma_char_list)
let answer1 () = Printf.printf "day3_1 answer is %d\n" (gamma * epsilon)

let filter f l idx =
  let most = f (List.nth (transpose l) idx) in
  (List.filter (fun x -> List.nth x idx = most) l, idx + 1)

let take f =
  let rec loop (l, num) =
    match l with
    | [ last ] -> last
    | [] -> failwith "invalid input"
    | xs -> loop (filter f xs num)
  in
  loop (list_of_list, 0)

let oxygen = char_to_int (take bigger)
let co2 = char_to_int (take lower)
let answer2 () = Printf.printf "day3_2 answer is asdasdas %d\n" (oxygen * co2)
