open Util
open Read
let input = "B-large-practice.in"

let parse_expr i expr =
  if i = 0 then (0, 0)
  else match expr with
    `Int j -> (0, 0)
  | `IntList [a;b] -> (a + b, 1)
  | `Empty -> (0, 0)
  | _ -> failwith "parse_expr"

let (raw_expr, raw) = Read.read parse_expr input
let num_testcase = raw_expr.(0)

let read_time x line =
  let depart_hour = String.sub line 0 2 |> int_of_string
  and depart_minute = String.sub line 3 2 |> int_of_string
  and terminate_hour = String.sub line 6 2 |> int_of_string
  and terminate_minute = String.sub line 9 2 |> int_of_string
  in
  (x, depart_hour * 60 + depart_minute, terminate_hour * 60 + terminate_minute)

type station = A | B

let read_testcase i =
  let turnaround = extract_int raw_expr.(2 * i + 1)
  and num_a = List.hd (extract_int_list raw_expr.(2 * i + 2)) in
  let times =
    List.mapi (fun i t -> read_time (if i < num_a then A else B) t) raw.(i) |> List.sort compare
  in
  (turnaround, times)

let solve i =
  let num_trians_a = ref 0 and num_trians_b = ref 0
  and (turnaround, times) = read_testcase i in
  let depart_train time (trains_a, trains_b) =
    let available_time = triple3 time + turnaround
    and is_available = List.exists (fun train -> triple2 time >= train) in
    match triple1 time with
      A ->
	  if is_available trains_a then
	    List.tl trains_a, available_time :: trains_b
	  else(
	    incr num_trians_a;
	    trains_a, available_time :: trains_b)
    | B ->
	  if is_available trains_b then
	    available_time :: trains_a, List.tl trains_b
	  else(
	    incr num_trians_b;
	    available_time :: trains_a, trains_b)
  in
  let rec aux (trains_a, trains_b) = function
      [] -> (!num_trians_a, !num_trians_b)
    | hd :: tl ->
	aux (depart_train hd (List.sort compare trains_a, List.sort compare trains_b)) tl
  in
  aux ([], []) times

let _ =
  for i = 0 to extract_int num_testcase - 1 do
    (fun (j, k) -> Printf.printf "Case #%d: %d %d\n" (i + 1) j k) (solve i)
  done
