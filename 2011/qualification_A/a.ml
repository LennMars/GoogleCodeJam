open Util
open Read
open List
let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sample.txt"

let parse_int i expr =
  if i = 0 then
    match expr with
      `Int j -> (j, 1)
    | _ ->  failwith ""
  else failwith ""

let (raw_int, raw) = Read.read parse_int input

let num_testcases = extract_int raw_int.(0)
let raw = raw.(0) |> List.map (Str.split (Str.regexp " ")) |> map tl |> Array.of_list
let rec make_ps accum = function
    [] -> rev accum
  | c :: p :: tl -> make_ps ((c, int_of_string p) :: accum) tl
  | _ -> failwith ""

let is_O p = fst p = "O"
let is_B p = fst p = "B"
let next ps s = try snd (List.find (fun p -> fst p = s) ps) with Not_found -> 0
let move pos term =
  if term = 0 then 0
  else if term - pos > 0 then pos + 1
  else if term - pos < 0 then pos - 1
  else pos

let rec aux ps (posO, posB) (termO, termB) nexts turn =
(*   Printf.printf "pos : (%d, %d), term : (%d, %d)\n" posO posB termO termB; *)
  if is_empty nexts || (termO = 0 && termB = 0) then turn
  else if posO = termO && (hd nexts = "O") then
    let nextO = next ps "O" in
    let posO = if nextO = 0 then 0 else posO in
    let posB = move posB termB in
    aux (remove is_O ps) (posO, posB) (nextO, termB) (tl nexts) (turn + 1)
  else if posB = termB && (hd nexts = "B") then
    let nextB = next ps "B" in
    let posB = if nextB = 0 then 0 else posB in
    let posO = move posO termO in
    aux (remove is_B ps) (posO, posB) (termO, nextB) (tl nexts) (turn + 1)
  else
    let posO = move posO termO in
    let posB = move posB termB in
    aux ps (posO, posB) (termO, termB) nexts (turn + 1)

let solve ps ns =
  aux (remove is_O ps |> remove is_B) (1, 1) (next ps "O", next ps "B") ns 0;;

let _ =
  for n = 1 to num_testcases do
    let ps = make_ps [] raw.(n - 1) in
    let ns = map fst ps in
    Printf.printf "Case #%d: %d\n" n (solve ps ns)
  done
