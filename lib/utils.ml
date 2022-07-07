open Core

module Read = struct
  let file filename = Core.In_channel.read_all filename
end

module Lists = struct
  let split_with_char_list char_list = String.split_on_chars ~on:char_list
  (* 비슷한 코드라서 만들 필요는 없지만 module 연습용 *)
end

let rec map f l = match l with [] -> [] | h :: t -> f h :: map f t
