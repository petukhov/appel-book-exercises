open Core

let rec tokens lexbuf =
  match Lexer.read lexbuf with
  | EOF -> [Syntax.EOF]
  | token -> token::tokens lexbuf

let lex_print ch =
  ch
  |> Lexing.from_channel
  |> tokens
  |> List.iter ~f:(fun token -> Format.printf "%s\n" (Syntax.show token))

let run filename () =
  In_channel.with_file filename ~f:lex_print

let () =
  Command.basic_spec
    ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: string))
    run
  |> Command_unix.run