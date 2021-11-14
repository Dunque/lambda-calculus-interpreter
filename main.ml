
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open List;;

(*Simple function that uses regex to check if a string contains any given expression.
  In our case, we will only use this to check if a string contains the stop character ;*)
let has_char str c =
  try match (Str.search_forward (Str.regexp c) str 0) with _-> true  
    with Not_found -> false
;;

(*This function reads the console input, concatenating each line (string) it reads with
  a space in between each, and it only stops when it finds a ;. The remaining input to the
  right of the ; gets ignored.*)
let read_input () =
  let rec aux acc =
    let line = read_line () in
    if (has_char line ";") then 
      let split_line = (hd (split_on_char ';' line)) in 
        cat acc (cat " " split_line)
    else 
      aux (cat acc (cat " " line))
  in aux ""
;;
  
(*We changed the read_line funtion for our own read_input one.*)
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_input ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

