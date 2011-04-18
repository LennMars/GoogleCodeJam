open Util
open Read
let input = Sys.argv.(1)

let parse_int i expr =
  if i = 0 then (0, 0)
  else match expr with
    `Int j -> (j, 1)
  | _ -> failwith "parse_int"

let (raw_int, raw) = Read.read parse_int input

let rec drop_while_with_size p xs =
  let rec aux n xs =
    if xs = [] then (n, [])
    else if p (List.hd xs) then aux (n + 1) (List.tl xs)
    else (n, xs)
  in
  aux 0 xs

let solve i =
  let engines = raw.(i * 2) in
  let queries = raw.(i * 2 + 1) in
  if queries = [] then 0 else
    let drop qs e = drop_while_with_size (fun q -> e <> q) qs in
    let drop_eagerly qs = List.find_max fst (List.map (drop qs) engines) |> snd in
    let rec aux n qs =
      if qs = [] then n
      else aux (n + 1) (drop_eagerly qs)
    in
    aux (-1) queries

let _ =
  for i = 0 to (fun x -> match x with `Int i -> i | _ -> failwith "a") raw_int.(0) - 1 do
    Printf.printf "Case #%d: %d\n" (i + 1) (solve i)
  done
