open Lexer

let green = "\x1b[0;32m"
let red = "\x1b[0;31m"
let reset_color = "\x1b[0m"

module I =
  Parser.MenhirInterpreter

module S = 
  MenhirLib.General

let stack checkpoint : I.element option =
  match checkpoint with
  | I.HandlingError env ->
      I.top env
  | _ ->
    assert false

let state checkpoint : int =
  match stack checkpoint with
  | Some (I.Element (s, _, _, _)) ->
      I.number s

let report lexbuf checkpoint =
    let startp = Lexing.lexeme_start_p lexbuf
    and endp = Lexing.lexeme_end_p lexbuf in
  Printf.fprintf stderr
      "%sLine %d: Col: %d-%d syntax error\n\n%!"
      red
      (startp.pos_lnum)
      (startp.pos_cnum - startp.pos_bol + 1)
      (endp.pos_cnum - endp.pos_bol + 1)
  ;
  Printf.fprintf stderr
      "Unexpected token '%s'\n%!"
      (Lexing.lexeme lexbuf);
  Printf.fprintf stderr
      "Parser state: %d%s\n\n%!"
      (state checkpoint)
      reset_color
  
