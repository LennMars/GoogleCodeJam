open Util
open Read

let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sample.txt"

let parse_int i expr =
  if i = 0 then
    match expr with
      `Int j -> (j, 1)
    | _ ->  failwith ""
  else failwith ""

let (raw_int, raw) = Read.read parse_int input

let num_testcases = extract_int raw_int.(0)

let solve n = ""

let _ =
  for n = 1 to num_testcases do
    Printf.printf "Case #%d: %d\n" n (solve (n - 1))
  done
