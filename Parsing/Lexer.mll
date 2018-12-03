{
  open Parser
}

let java_letter = ['a'-'z' 'A'-'Z' '_' '$']

let non_zero_digit = [ '1' - '9' ]
let digit = '0' | non_zero_digit

(* TODO change identifier *)
let identifier = java_letter (java_letter | digit )*

(* Boolean Literals *)
let boolean = "true"|"false"
(* Integer Literals *)
let decimal_numeral = '0' | non_zero_digit digit*
let integer = decimal_numeral* ('l' | 'L')?

(* TODO ad other literals *)

let space = [' ' '\t' '\n' ]
rule nexttoken = parse

(* White Space - 3.6 *)
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

(* Literals - 3.10 *)
  | boolean as b { BOOLEAN_LITERAL b }
  | integer as i { INTEGER_LITERAL i }

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