{
  open Parser

  let create_error_message (lexbuf: Lexing.lexbuf) =
    lexbuf.lex_curr_p.pos_fname ^ ": cannot find symbol"
    ^ ":line " ^ (string_of_int lexbuf.lex_curr_p.pos_lnum)
    ^ ":column " ^ (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
}

let java_letter = ['a'-'z' 'A'-'Z' '_' '$']

let non_zero_digit = [ '1' - '9' ]
let digit = '0' | non_zero_digit

(* TODO change identifier *)
let identifier = java_letter (java_letter | digit )*

(* Boolean Literals *)
let boolean_literal = "true" | "false"

(* Integer Literals *)
let decimal_numeral = '0' | non_zero_digit digit*
let integer_literal = decimal_numeral ('l' | 'L')?

(* String Literals *)
let char = [^'"']
let string_literal = '"' char* '"'

(* Float Literals *)
let float_literal = (digit+ '.'? digit* | '.' digit+) ('f' | 'F')?

(* Char Literals *)
let char_literal = '\'' char* '\''

let space = [' ' '\t']
let newline = ['\n' '\r'] | '\r' '\n'
rule nexttoken = parse

(* White Space - 3.6 *)
  | newline        { Lexing.new_line lexbuf; nexttoken lexbuf }
  | space+         { nexttoken lexbuf }
  | eof            { EOF }

(* Keywords - 3.9 *)
  | "abstract"     { ABSTRACT "abstract" }
  | "assert"       { ASSERT "assert" }
  | "boolean"      { BOOLEAN "boolean" }
  | "break"        { BREAK "break" }
  | "byte"         { BYTE "byte" }
  | "case"         { CASE "case" }
  | "catch"        { CATCH "catch" }
  | "char"         { CHAR "char" }
  | "class"        { CLASS "class" }
  | "const"        { CONST "const" }
  | "continue"     { CONTINUE "continue" }
  | "default"      { DEFAULT "default" }
  | "do"           { DO "do" }
  | "double"       { DOUBLE "double" }
  | "else"         { ELSE "else" }
  | "enum"         { ENUM "enum" }
  | "extends"      { EXTENDS "extends" }
  | "final"        { FINAL "final" }
  | "finally"      { FINALLY "finally" }
  | "float"        { FLOAT "float" }
  | "for"          { FOR "for" }
  | "goto"         { GOTO "goto" }
  | "if"           { IF "if" }
  | "implements"   { IMPLEMENTS "implements" }
  | "import"       { IMPORT "import" }
  | "instanceof"   { INSTANCEOF "instanceof" }
  | "int"          { INT "int" }
  | "interface"    { INTERFACE "interface" }
  | "long"         { LONG "long" }
  | "native"       { NATIVE "native" }
  | "new"          { NEW "new" }
  | "package"      { PACKAGE "package" }
  | "private"      { PRIVATE "private" }
  | "protected"    { PROTECTED "protected" }
  | "public"       { PUBLIC "public" }
  | "return"       { RETURN "return" }
  | "short"        { SHORT "short" }
  | "static"       { STATIC "static" }
  | "strictfp"     { STRICTFP "strictfp" }
  | "super"        { SUPER "super" }
  | "switch"       { SWITCH "switch" }
  | "synchronized" { SYNCHRONIZED "synchronized" }
  | "this"         { THIS "this" }
  | "throw"        { THROW "throw" }
  | "throws"       { THROWS "throws" }
  | "transient"    { TRANSIENT "transient" }
  | "try"          { TRY "try" }
  | "void"         { VOID "void" }
  | "volatile"     { VOLATILE "volatile" }
  | "while"        { WHILE "while" }

(* Identifiers - 3.8 *)
  | identifier as ident { IDENTIFIER ident }

(* Literals - 3.10 *)
  | integer_literal as i { INTEGER_LITERAL i }
  | string_literal as s { STRING_LITERAL s }
  | float_literal as f { FLOAT_LITERAL f }
  | boolean_literal as b { BOOLEAN_LITERAL b }
  | char_literal as c { CHAR_LITERAL c }

(* Separators - 3.11 *)
  | "("   { L_PAR "l_par" }
  | ")"   { R_PAR "r_par" }
  | "{"   { L_BRACE "l_brace" }
  | "}"   { R_BRACE "r_brace" }
  | "["   { L_BRACKET "l_bracket" }
  | "]"   { R_BRACKET "r_bracket" }
  | ";"   { SEMICOLON "semicolon" }
  | ","   { COMMA "comma" }
  | "."   { PERIOD "period" }

(* Operators - 3.12 *)
  | "="    { ASSIGN "assign" }
  | "=="   { EQUAL "equal" }
  | "!="   { NOT_EQUAL "not_equal" }
  | "+"    { PLUS "plus" }
  | "-"    { MINUS "minus" }
  | "*"    { MULTIPLY "multiply" }
  | "/"    { DIVIDE "divide" }
  | "%"    { MODULO "modulo" }
  | "?"    { TERNARY_THEN "ternary_then" }
  | ":"    { TERNARY_ELSE "ternary_else" }
  | "!"    { NOT_LOGICAL "not_logical" }

(* Error *)
  | _      { raise (Failure (create_error_message lexbuf)) }