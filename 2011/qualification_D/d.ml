open Util
open Read

let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sample.txt"
let parse_int i expr =
  if i = 0 then (0, 0)
  else match expr with
    `Int j -> (1, 1)
  | _ -> failwith "read"
let (raw_int, raw) = read parse_int input
let num_testcases = extract_int raw_int.(0)
let raw = Array.map (fun l -> List.hd l |> Str.split (Str.regexp " ") |> List.map (fun s -> int_of_string s - 1) |> Array.of_list) raw

let loop a i =
  if i = a.(i) then 0 else
    let rec aux j acc =
      if a.(j) = a.(i) then acc
      else aux a.(j) (acc + 1)
    in
    aux a.(i) 1

let count_loop a =
  let b = Array.mapi (fun i _ -> loop a i)  a in
  let count = Array.make (Array.length a) 0 in
  Array.iter (fun x -> if x <> 0 then count.(x - 1) <- count.(x - 1) + 1) b;
  Array.fold_left (+) 0 count

let _ =
  for n = 1 to num_testcases do
    Printf.printf "Case #%d: %d.000000\n" n (count_loop raw.(n - 1))
  done
