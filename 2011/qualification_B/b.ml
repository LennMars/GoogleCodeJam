open Util
open Read
open List
open String
let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sample.txt"

let string_to_list s =
  let n = length s in
  let rec aux s acc n =
    if s = "" then List.rev acc
    else aux (sub s 1 (n - 1)) (sub s 0 1 :: acc) (n - 1)
  in
  aux s [] n

let parse_int i expr =
  if i = 0 then (extract_int expr, 1)
  else failwith ""

let (raw_int, raw) = read parse_int input

let raw = raw.(0) |> List.map (Str.split (Str.regexp " ")) |> Array.of_list

let num_testcases = extract_int raw_int.(0)

let make s =
  let num_pair = List.hd s |> int_of_string in
  let s = List.tl s in
  let rec get n acc s =
    if n = 0 then acc, s
    else
      let x = List.hd s in
      get (n-1) ([|sub x 0 1; sub x 1 1; sub x 2 1|] :: acc) (List.tl s)
  in
  let (pair, s) = get num_pair [] s in
  let num_opp = List.hd s |> int_of_string in
  let s = List.tl s in
  let rec get n acc s =
    if n = 0 then acc, s
    else
      let x = List.hd s in
      get (n-1) ([|sub x 0 1; sub x 1 1|] :: acc) (List.tl s)
  in
  let (opp, s) = get num_opp [] s in
  (pair |> Array.of_list, opp |> Array.of_list, List.nth s 1 |> string_to_list)

let pr = List.iter print_string

let solve is_debug (pair, opp, prob) =
  let is_pair (s1, s2) =
    let f = ref "" in
    for p = 0 to Array.length pair - 1 do
      let pair' = pair.(p).(0), pair.(p).(1) in
      if pair' = (s1, s2) || pair' = (s2, s1) then f := pair.(p).(2)
    done;
    !f
  and is_opp (s1, s2) =
    let is = ref false in
    for p = 0 to Array.length opp - 1 do
      let opp' = opp.(p).(0), opp.(p).(1) in
      if opp' = (s1, s2) || opp' = (s2, s1) then is := true
    done;
    !is
  in
  let rec aux prob acc =
    if is_debug then begin
      Printf.printf "\nprob : "; pr prob; print_newline ();
      Printf.printf "acc : "; pr acc; print_newline () end;
    match prob with
      [] -> List.rev acc
    | hd :: tl ->
	if acc = [] then aux tl (hd :: acc)
	else (
	  let f = is_pair (hd, List.hd acc) in
	  if f <> "" then aux tl (f :: (List.tl acc)) (* pair found *)
	  else if List.exists (fun s -> is_opp (s, hd)) acc then aux tl [] (* opposed found *)
	  else aux tl (hd :: acc)
	)
  in
  aux prob []

let solve' is_debug n =
  solve is_debug (make raw.(n))

let _ =
  for m = 1 to num_testcases do
    Printf.printf "Case #%d: [%s]\n" m (solve' false (m - 1) |> String.concat ", ");
  done
