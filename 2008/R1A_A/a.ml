open Util
open Read
open List

let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else "test.txt"

let parse_expr i expr =
  if i = 0 then (0, 0)
  else match expr with
    `Int j -> (2, 1)
  | _ -> (0, 0)

let (raw_expr, raw) = Read.read parse_expr input
let num_testcase = extract_int raw_expr.(0)

let read i =
  let r = raw.(i) in
  let (a, b) = nth r 0, nth r 1 in
  let sp x = Str.split (Str.regexp " ") x |> map int_of_string in
  sp a ,sp b

let sort_zerofirst = sort (fun x y -> if x = 0 then -1 else if y = 0 then 1 else 0)
let sort_absmaxfirst = sort (fun x y -> compare (abs y) (abs x))

let match_zero a b acc =
  let rec aux a b accum =
    if a = [] || hd a <> 0 then accum, a, b
    else aux (tl a) (tl b) ((hd a, hd b) :: accum)
  in
  let (pairs, a, b) = aux (sort_zerofirst a) (sort_absmaxfirst b) acc
  in
  let rec aux a b accum =
    if a = [] || hd b <> 0 then accum, a, b
    else aux (tl a) (tl b) ((hd a, hd b) :: accum)
  in
  aux (sort_absmaxfirst a) (sort_zerofirst b) pairs


let rec first a b is_swap accum =
try
  if a = [] then accum, a, b
  else if hd a >= 0 || hd b <= 0 then accum, a, b
  else
    let add = if is_swap then (hd b, hd a) else (hd a, hd b) in
    first (tl a) (tl b) is_swap (add :: accum)
with
  Failure s -> print_endline s;accum, a, b

let second a b accum =
  let (pairs, a, b) = match_zero a b accum in
  let (a, b) = sort compare a, sort (swap_arg compare) b in
  let rec aux a b accum =
    if a = [] then accum, a, b
    else aux (tl a) (tl b) ((hd a, hd b) :: accum)
  in
  aux a b pairs

open Big_int
let rec ans ab accum =
  if ab = [] then string_of_big_int accum
  else
    let (a, b) = hd ab in
    let (a, b) = big_int_of_int a, big_int_of_int b in
    ans (tl ab) (add_big_int (mult_big_int a b) accum)

let solve i =
  let (a, b) = read i in
  let (acc, a, b) = first (sort compare a) (sort (swap_arg compare) b) false [] in
  let (acc, a, b) = first (sort compare b) (sort (swap_arg compare) a) true acc in
  let (acc, a, b) = second a b acc in
  if a <> [] || b <> [] then failwith "not finished" else ans acc zero_big_int

let output () =
  for i = 1 to num_testcase do
    Printf.printf "Case #%d: %s\n" i (solve (i - 1))
  done

let _ = output ()
