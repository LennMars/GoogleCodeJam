open Util
open Read
open Big_int
let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else "A-large-practice.in"

let parse i = function
    `Int j -> (1, j)
  | _ -> failwith "parse"

let (raw1, raw2) = read parse input
let raw2 = Array.map (fun r -> list_of big_int_of_string (List.hd r) |> Array.of_list) raw2

let num_instances = extract_int raw1.(0)

let bi = big_int_of_int
let ib = int_of_big_int

let place i j = 3 * i + j

let ( + ) = add_big_int
let ( * ) = mult_big_int
let ( / ) = div_big_int

let make_table case =
  let r = raw2.(case) in
  let (n, a, b, c, d, x0, y0, m) = (ib r.(0), r.(1), r.(2), r.(3), r.(4), r.(5), r.(6),  r.(7)) in
  let table = Array.make_matrix 3 3 zero_big_int in
  let add x y =
    table.(x mod 3).(y mod 3) <- table.(x mod 3).(y mod 3) + unit_big_int
  in
  add (ib x0) (ib y0);
  let rec aux x y n =
    if n <= 1 then ()
    else
      let x = mod_big_int (a * x + b) m
      and y = mod_big_int (c * y + d) m
      in
      add (ib x) (ib y);
      aux x y (n - 1)
  in
  aux x0 y0 n;
  table

let pattern_same i j table =
  let a = table.(i).(j) in
  let ( - ) = sub_big_int in
  a * (a - bi 1) * (a - bi 2) / (bi 6)

let remain i j = (6 - i - j) mod 3

let pattern_diff (i, j) (a, b) table =
  let x = remain i a
  and y = remain j b
  in
  if place x y > place i j && place x y > place a b then
    table.(i).(j) * table.(a).(b) * table.(x).(y)
  else
    zero_big_int

let ans case =
  let table = make_table case in
  let count = ref zero_big_int in
  for i = 0 to 2 do
    for j = 0 to 2 do
      for a = 0 to 2 do
	for b = 0 to 2 do
	  if place a b >= place i j && (a, b) = (i, j) then begin
	    let c = pattern_same i j table in
	    (* if c > 0 then Printf.printf "same %d %d\n" a b; *)
	    count := add_big_int c !count end
	  else if place a b > place i j then begin
	    let c = pattern_diff (i, j) (a, b) table in
	    (* if c > 0 then Printf.printf "diff (%d, %d) (%d, %d)\n" i j a b; *)
	    count := add_big_int c !count  end
	  else
	    ()
	done
      done
    done
  done;
  !count

let _ =
  for i = 1 to num_instances do
    Printf.printf "Case #%d: %s\n" i (ans (i - 1) |> string_of_big_int)
  done
