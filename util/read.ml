open Util
open Str

type expr = [`String of string | `StringList of string list | `IntList of int list | `Int of int | `Float of float | `FloatList of float list | `Empty]


let extract_string = function
    `String s -> s
  | _ -> raise (Invalid_argument "extract_string")

let extract_string_list = function
    `StringList ss -> ss
  | _ -> raise (Invalid_argument "extract_string_list")

let extract_int_list = function
    `IntList is -> is
  | _ -> raise (Invalid_argument "extract_int_list")

let extract_int = function
    `Int s -> s
  | _ -> raise (Invalid_argument "extract_int")

let extract_float_list = function
    `FloatList fs -> fs
  | _ -> raise (Invalid_argument "extract_float_list")

let extract_float = function
    `Float f -> f
  | _ -> raise (Invalid_argument "extract_float")

let match_as_expr s : expr =
  match Str.split (regexp "[ ¥t]+") s with
    [] -> `Empty
  | [single] ->
      (try `Int (int_of_string single) with Failure _ ->
	try `Float (float_of_string single) with Failure _ ->
	  `String single)
  | _ as ss ->
      (try `IntList (List.map int_of_string ss) with Failure _ ->
	try `FloatList (List.map float_of_string ss) with Failure _ ->
	  `StringList ss)

(*
let read parse_int filename =
  let line_num = ref (-1) in
  let file_in = open_in filename in
  let input_line () =
    line_num := !line_num + 1;
    input_line file_in
  in
  let rec read_block (m, n) accum =
    let rec aux m accum =
      if m <= 0 then List.rev accum
      else aux (m - 1) (input_line () :: accum)
    in
    if n <= 0 then accum
    else read_block (m, n - 1) (aux m [] :: accum)
  in
  let rec main ints strings =
    try
      let line = input_line () in
      try
	let i = int_of_string line in
	main (i :: ints) (read_block (parse_int !line_num i) [] @ strings)
      with
	Failure "int_of_string" -> main ints ([line] :: strings)
    with End_of_file ->
      close_in file_in;
      (List.rev ints |> Array.of_list, List.rev strings |> Array.of_list)
  in
  main [] []
 *)

let read parse_expr filename =
  let line_num = ref (-1) in
  let file_in = open_in filename in
  let input_line () =
    line_num := !line_num + 1;
    input_line file_in
  in
  let rec read_block (m, n) accum =
    let rec aux m accum =
      if m <= 0 then List.rev accum
      else aux (m - 1) (input_line () :: accum)
    in
    if n <= 0 then accum
    else read_block (m, n - 1) (aux m [] :: accum)
  in
  let rec main exprs strings =
    let line = input_line () in
    let expr = match_as_expr line in
    let exprs = expr :: exprs in
    let strings = read_block (parse_expr !line_num expr) [] @ strings
    in
    try main exprs strings
    with End_of_file ->
      close_in file_in;
      (List.rev exprs |> Array.of_list, List.rev strings |> Array.of_list)
  in
  main [] []

