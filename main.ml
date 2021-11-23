
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

(*This function reads the file that we input in the command line, and does essentialy the same as
  read_input. The only difference is that this one reads from the input channel of a file, and also
  it returns a tuple, containing both the desired input string and the number of the line where
  it started to read the sentence that contains the error.*)
let read_input_from_file in_channel numLine =
  let rec aux acc lineNum =
    let line = input_line in_channel in
    if (has_char line ";") then 
      let split_line = (hd (split_on_char ';' line)) in 
        ((cat acc (cat " " split_line)),lineNum+1)
    else 
      aux (cat acc (cat " " line)) (lineNum+1)
  in aux "" numLine
;;

(*This loop is used to read the contents of the file. Its changed so it stops when it reads a whole
  sentence (finishing with a ';') and also displays the line where the error was found*)
let file_loop () = 
  let in_channel = open_in Sys.argv.(1) in
  try
    while true do
      let rec loop ctx numLine =
        try
          let inp = read_input_from_file in_channel numLine in
          let tm = s token (from_string (fst inp)) in
          let tyTm = typeof ctx tm in
          print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
          loop ctx (snd inp)
        with
           Lexical_error ->
             print_endline ("lexical error in line: " ^ string_of_int numLine);
             exit 0;
         | Parse_error ->
             print_endline ("syntax error in line: " ^ string_of_int numLine);
             exit 0;
         | Type_error e ->
             print_endline ("type error: " ^ e ^ " in line: " ^ string_of_int numLine);
             exit 0;
      in
        loop emptyctx 1
    done
  with End_of_file ->
    close_in in_channel
  ;;
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

if (Array.length Sys.argv >= 2) then
  file_loop ()
else
  top_level_loop ()
;;

