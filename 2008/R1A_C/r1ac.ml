open Util
open Read
open Big_int
let input = Sys.argv.(1)

let parse i expr =
  match expr with
    `Int j -> (j, 1)
  | _ -> failwith "parse"

let (raw1, raw2) = read parse input

let num_instances = extract_int raw1.(0)


let ( * ) x y = mult_big_int x y
and ( + ) x y = add_big_int x y

let mul (a, b, c, d) (e, f, g, h) =
  let i = a * e + b * g
  and j = a * f + b * h
  and k = c * e + d * g
  and l = c * f + d * h
  in
  (i, j, k, l)

let eye = (unit_big_int, zero_big_int, zero_big_int, unit_big_int)

let mat_exp = general_exp eye mul

let b = big_int_of_int

let add0 s =
  match String.length s with
    1 -> "00" ^ s
  | 2 -> "0" ^ s
  | 3 -> s
  | _ -> failwith "add0"

let ans n =
  let (a, _, _, _) = mat_exp (b 3, b 5, b 1, b 3) n in
  mod_big_int (b 2 * a + b 999) (b 1000) |> string_of_big_int |> add0

let output () =
  for i = 1 to num_instances do
     Printf.printf "Case #%d: %s\n" i (raw2.(0) |> (swap_arg List.nth (i - 1)) |> int_of_string |> ans)
  done

let _ = output ()

