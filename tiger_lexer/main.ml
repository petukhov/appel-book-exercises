(* let stringify_token = function
     | Parser.EOF -> "EOF"
     | Parser.OPENING_TAG s -> "OPENING_TAG " ^ s
     | Parser.CLOSING_TAG s -> "CLOSING_TAG " ^ s
     | Parser.STRING s -> "STRING " ^ s

   let loop filename () =
     let inx = Core.In_channel.create filename in
     let lexbuf = Lexing.from_channel inx in
     try
       while true do
         let token = Lexer.read lexbuf in
         let str = stringify_token token in
         Printf.printf "%s\n" str;
         if token = Parser.EOF then exit 0
       done
     with Failure msg ->
       Printf.eprintf "Lexer error: %s\n" msg;
       exit 1

   let file_to_string filename =
     let inx = Core.In_channel.create filename in
     let str = Core.In_channel.input_all inx in
     Core.In_channel.close inx;
     str

   let read_all_tokens (lexbuf : Lexing.lexbuf) : string =
     let all_tokens = ref [] in
     try
       while true do
         let token = Lexer.read lexbuf in
         let str = stringify_token token in
         all_tokens := str :: !all_tokens;
         if token = Parser.EOF then raise Exit
       done;
       ""
     with Exit -> List.rev !all_tokens |> String.concat "\n"

   let test_lexer () =
     let expected_str = file_to_string "xml_parser/lexer_expected.txt" in
     let lexbuf = Lexing.from_string (file_to_string "xml_parser/test1.xml") in
     let all_tokens_str = read_all_tokens lexbuf in
     if String.equal expected_str all_tokens_str then
       Printf.printf "Lexer test passed\n"
     else Printf.printf "Lexer test failed\n" *)

(* let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      print_endline msg;
      Tag ("SyntaxError", [])
  | Parser.Error ->
      print_endline "Syntax error";
      exit (-1)

let indentation indent =
  print_endline "";
  for _ = 0 to indent - 1 do
    Printf.printf "  "
  done

let rec print_tag (indent : int) (tag : Ast.xml) =
  match tag with
  | Ast.Tag (value, l) ->
      indentation indent;
      Printf.printf "<%s>" value;
      List.iter (fun x -> print_tag (indent + 1) x) l;
      indentation indent;
      Printf.printf "</%s>" value
  | Ast.String s ->
      indentation indent;
      Printf.printf "%s" s
  | _ -> print_endline "Error"

let parse_and_print lexbuf =
  match parse_with_error lexbuf with a -> print_tag 0 a

let loop filename () =
  print_endline "\nParsing...";
  print_endline filename;
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let () =
  Command.basic_spec ~summary:"Parse and display Xml"
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command_unix.run *)

let () =
   print_string "Hello, World!\n"
