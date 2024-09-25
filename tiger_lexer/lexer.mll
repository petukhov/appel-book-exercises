{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

(* Remove angle brackets around the tag like <, >, and </*)
let remove_angle_brackets (str: string) = 
  let len = String.length str in
  let res = String.sub str 1 (len - 2) in 
  if res.[0] = '/' then
    String.sub res 1 (len - 3)
  else
    res
}

let newline = '\r' | '\n' | "\r\n" 
let white = [' ' '\t']+
let opening_bracket = '<'
let closing_bracket = '>'
let closing_tag_opening_bracket = opening_bracket '/'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let opening_tag = opening_bracket id closing_bracket
let closing_tag = closing_tag_opening_bracket id closing_bracket
let any_string = [^ '<' '>' '\n' '\r' '\t' ' ']+

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | opening_tag { OPENING_TAG (remove_angle_brackets (Lexing.lexeme lexbuf))}
  | closing_tag { CLOSING_TAG (remove_angle_brackets (Lexing.lexeme lexbuf))}
  | any_string { STRING (Lexing.lexeme lexbuf)}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }