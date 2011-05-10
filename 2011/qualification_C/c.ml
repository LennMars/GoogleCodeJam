open Util
open Read
open Big_int
let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sample.txt"

let parse_int i expr =
  if i = 0 then
    match expr with
      `Int j -> (2, j)
    | _ -> failwith "parse_int"
  else failwith "parse_int"

let (raw_int, raw) = Read.read parse_int input

let num_testcases = extract_int raw_int.(0)
let raw = Array.map Array.of_list raw
let to_weights s = Str.split (Str.regexp "[ \t]+") s |> List.map int_of_string |> List.sort compare

let to_bin y =
  let rec aux y accum =
    if y = 0 then accum
    else if (y mod 2 = 0) then aux (y lsr 1) (0::accum)
    else aux (y lsr 1) (1::accum)
  in
  aux y []

let rec fill_zero y n =
  if n = 0 then y
  else fill_zero (0 :: y) (n - 1)

let solve n =
  let (m, ws') = raw.(n).(0), raw.(n).(1) |> to_weights in
  let ws = List.map to_bin ws' in
  let max_len = List.map List.length ws |> List.find_max identity in
  let fill w = fill_zero w (max_len - List.length w) in
  let ws = List.map fill ws in
  let can_solve =
    let rec count ws accum =
      match ws with
	[] -> List.for_all (fun x -> x mod 2 = 0) accum
      | hd :: tl -> count tl (List.map2 (+) hd accum)
    in
    count ws (List.init (fun _ -> 0) max_len)
  in
  if can_solve then List.fold_left (swap_arg add_int_big_int) zero_big_int (List.tl ws')
  else raise Not_found

let _ =
  for n = 1 to num_testcases do
    let ans = try string_of_big_int (solve (n - 1)) with Not_found -> "NO" in
    Printf.printf "Case #%d: %s\n" n ans
  done
