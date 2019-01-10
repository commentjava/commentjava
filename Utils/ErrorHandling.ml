open Lexer

let green = "\x1b[0;32m"
let red = "\x1b[0;31m"
let reset_color = "\x1b[0m"

module I =
  Parser.MenhirInterpreter

module S = 
  MenhirLib.General

let stack checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      I.stack env
  | _ ->
    assert false

let state checkpoint : int =
  match Lazy.force (stack checkpoint) with
  | S.Nil -> (* Parser in initial state *)
      0
  | S.Cons (Element (s, _, _, _), _) ->
      I.number s

let report lexbuf checkpoint =
    let startp = Lexing.lexeme_start_p lexbuf
    and endp = Lexing.lexeme_end_p lexbuf in
  Printf.fprintf stderr
      "%sLine %d: Col: %d-%d syntax error\n\n%!"
      red
      (startp.pos_lnum)
      (startp.pos_cnum)
      (endp.pos_cnum)
  ;
  Printf.fprintf stderr
      "Unexpected token '%s'\n%!"
      (Lexing.lexeme lexbuf);
  Printf.fprintf stderr
      "Parser state: %d%s\n\n%!"
      (state checkpoint)
      reset_color
  
