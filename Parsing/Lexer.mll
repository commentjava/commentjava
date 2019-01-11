{
  open Parser

  let create_error_message (lexbuf: Lexing.lexbuf) =
    lexbuf.lex_curr_p.pos_fname ^ ": unknown symbol "
    ^ "'" ^ Lexing.lexeme lexbuf ^ "' "
    ^ "line " ^ (string_of_int lexbuf.lex_curr_p.pos_lnum)
    ^ " column " ^ (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
}

let java_letter = ['a'-'z' 'A'-'Z' '_' '$']

let non_zero_digit = [ '1' - '9' ]
let digit = '0' | non_zero_digit
let octal_digit = ['0'-'7']
let hex_digit = ['0'-'9' 'a'-'z' 'A'-'Z']

(* TODO change identifier *)
let identifier = java_letter (java_letter | digit )*

(* Boolean Literals *)
let boolean_literal = "true" | "false"

(* Boolean Literals *)
let null_literal = "null"

(* Integer Literals *)
let hex_numeral = '0' ('x' | 'X') hex_digit+
let octal_numeral = '0' octal_digit+
let decimal_numeral = '0' | non_zero_digit digit*
let integer_literal = (decimal_numeral | hex_numeral | octal_numeral) ('l' | 'L')?

(* String Literals *)
let char = [^'"']
let string_literal = '"' char* '"'

(* Float Literals *)
let float_literal = (digit+ '.'? digit* | '.' digit+) ('f' | 'F')?

(* Char Literals *)
let char_literal = '\'' char* '\''

(* space / horizontal tab / form feed *)
let space = [' ' '\t' '\x0C']

(* newline / return / newline then return *)
let newline = ['\n' '\r'] | '\r' '\n'

let eol_comment = "//" [^ '\n' '\r']* newline
let traditional_comment = "/*" ([^'*'] | '*' [^'/'])* "*/"

rule nexttoken = parse

(* Comments *)
  | eol_comment    { Lexing.new_line lexbuf; nexttoken lexbuf }
  | traditional_comment { nexttoken lexbuf }

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

(* Literals - 3.10 *)
(* Has to be before identifier in order to parse booleans and null and not identifiers instead *)
  | boolean_literal as b { BOOLEAN_LITERAL b }
  | char_literal as c { CHAR_LITERAL c }
  | float_literal as f { FLOAT_LITERAL f }
  | integer_literal as i { INTEGER_LITERAL i }
  | null_literal { NULL_LITERAL }
  | string_literal as s { STRING_LITERAL s }

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
  | "!="   { NOT_EQUAL "not_equal"}
  | "<"    { LOWER "lower" }
  | ">"    { GREATER "greater" }
  | "<="   { LOWER_OR_EQUAL "lower_or_equal" }
  | ">="   { GREATER_OR_EQUAL "greater_or_equal" }
  | "?"    { QUESTION_MARK "question_mark" }
  | ":"    { COLON "colon" }
  | "&&"   { AND_LOGICAL "and_logical" }
  | "||"   { OR_LOGICAL "or_logical" }
  | "!"    { NOT_LOGICAL "not_logical" }
  | "++"   { INCREMENT "increment" }
  | "--"   { DECREMENT "decrement" }
  | "+"    { PLUS "plus" }
  | "-"    { MINUS "minus" }
  | "*"    { MULTIPLY "multiply" }
  | "/"    { DIVIDE "divide" }
  | "%"    { MODULO "modulo" }
  | "~"    { COMPLEMENT_BITWISE "complement_bitwise" }
  | "&"    { AND_BITWISE "and_bitwise" }
  | "|"    { OR_BITWISE "or_bitwise" }
  | "^"    { XOR_BITWISE "xor_bitwise" }
  | "<<"   { LEFT_SHIFT "left_shift" }
  | ">>"   { RIGHT_SHIFT "right_shift" }
  | ">>>"  { RIGHT_SHIFT_UNSIGNED "right_shift_unsigned" }
  | "+="   { PLUS_ASSIGN "plus_assign" }
  | "-="   { MINUS_ASSIGN "minus_assign" }
  | "*="   { MULTIPLY_ASSIGN "multiply_assign" }
  | "/="   { DIVIDE_ASSIGN "divide_assign" }
  | "%="   { MODULUS_ASSIGN "modulus_assign" }
  | "&="   { AND_BITWISE_ASSIGN "and_bitwise_assign" }
  | "|="   { OR_BITWISE_ASSIGN "or_bitwise_assign" }
  | "^="   { XOR_ASSIGN "xor_assign" }
  | "<<="  { LEFT_SHIFT_ASSIGN "left_shift_assign" }
  | ">>="  { RIGHT_SHIFT_ASSIGN "right_shift_assign" }
  | ">>>=" { RIGHT_SHIFT_UNSIGNED_ASSIGN "right_shift_unsigned_assign" }
  | "@"    { AT "at" }


(* Identifiers - 3.8 *)
  | identifier as ident { IDENTIFIER ident }

(* Error *)
  | _      { raise (Failure (create_error_message lexbuf)) }
