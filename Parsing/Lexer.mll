{
  open Tokens
}

let java_letter = ['a'-'z' 'A'-'Z' '_' '$']
let java_digit = ['0'-'9']
let identifier = java_letter (java_letter | java_digit )*
let space = [' ' '\t' '\n' ]

rule nexttoken = parse

(* White Space - 3.6 *)
  | space+         { nexttoken lexbuf }
  | eof            { EOF }

(* Keywords - 3.9 *)
  | "abstract"     { ABSTRACT }
  | "assert"       { ASSERT }
  | "boolean"      { BOOLEAN }
  | "break"        { BREAK }
  | "byte"         { BYTE }
  | "case"         { CASE }
  | "catch"        { CATCH }
  | "char"         { CHAR } 
  | "class"        { CLASS }
  | "const"        { CONST }
  | "continue"     { CONTINUE }
  | "default"      { DEFAULT }
  | "do"           { DO }
  | "double"       { DOUBLE }
  | "else"         { ELSE }
  | "enum"         { ENUM }
  | "extends"      { EXTENDS }
  | "final"        { FINAL }
  | "finally"      { FINALLY }
  | "float"        { FLOAT }
  | "for"          { FOR }
  | "goto"         { GOTO }
  | "if"           { IF }
  | "implements"   { IMPLEMENTS }
  | "import"       { IMPORT }
  | "instanceof"   { INSTANCEOF }
  | "int"          { INT }
  | "interface"    { INTERFACE }
  | "long"         { LONG }
  | "native"       { NATIVE }
  | "new"          { NEW }
  | "package"      { PACKAGE }
  | "private"      { PRIVATE }
  | "protected"    { PROTECTED }
  | "public"       { PUBLIC }
  | "return"       { RETURN }
  | "short"        { SHORT }
  | "static"       { STATIC }
  | "strictfp"     { STRICTFP }
  | "super"        { SUPER }
  | "switch"       { SWITCH }
  | "synchronized" { SYNCHRONIZED }
  | "this"         { THIS }
  | "throw"        { THROW }
  | "throws"       { THROWS }
  | "transient"    { TRANSIENT }
  | "try"          { TRY }
  | "void"         { VOID }
  | "volatile"     { VOLATILE }
  | "while"        { WHILE }

(* Identifiers - 3.8 *)
  | identifier as ident { IDENTIFIER ident }

(* Separators - 3.11 *)
  | "("   { L_PAR }
  | ")"   { R_PAR }
  | "{"   { L_BRACE }
  | "}"   { R_BRACE }
  | "["   { L_BRACKET }
  | "]"   { R_BRACKET }
  | ";"   { SEMICOLON }
  | ","   { COMMA }
  | "."   { PERIOD }

