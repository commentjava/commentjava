%token EOF

(* Identifiers - 3.8*)
%token < string > IDENTIFIER

(* Keywords - 3.9 *)
%token <string> ABSTRACT ASSERT
%token <string> BOOLEAN BREAK BYTE
%token <string> CASE CATCH CHAR CLASS CONST CONTINUE
%token <string> DEFAULT DO DOUBLE
%token <string> ELSE ENUM EXTENDS
%token <string> FINAL FINALLY FLOAT FOR
%token <string> GOTO
%token <string> IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE
%token <string> LONG
%token <string> NATIVE NEW
%token <string> PACKAGE PRIVATE PROTECTED PUBLIC
%token <string> RETURN
%token <string> SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
%token <string> THIS THROW THROWS TRANSIENT TRY
%token <string> VOID VOLATILE
%token <string> WHILE

(* Literals - 3.10 *)
/* %token < int > INTEGER_LITERAL
%token < float > FLOAT_LITERAL
%token < float > DOUBLE_LITERAL
%token < bool > BOOLEAN_LITERAL
%token < char > CHAR_LITERAL
%token < string > STRING_LITERAL
%token NULL_LITERAL */

%token <string> INTEGER_LITERAL
%token <string> FLOAT_LITERAL
%token <string> DOUBLE_LITERAL
%token <string> BOOLEAN_LITERAL
%token <string> CHAR_LITERAL
%token <string> STRING_LITERAL
%token NULL_LITERAL

(* Separators - 3.11 *)
%token <string> L_PAR R_PAR
%token <string> L_BRACE R_BRACE
%token <string> L_BRACKET R_BRACKET
%token <string> SEMICOLON COMMA PERIOD

(* Operators - 3.12 *)
%token <string> ASSIGN
%token <string> EQUAL NOT_EQUAL LOWER GREATER LOWER_OR_EQUAL GREATER_OR_EQUAL
%token <string> QUESTION_MARK COLON
%token <string> AND_LOGICAL OR_LOGICAL NOT_LOGICAL
%token <string> INCREMENT DECREMENT
%token <string> PLUS MINUS MULTIPLY DIVIDE MODULO
%token <string> COMPLEMENT_BITWISE AND_BITWISE OR_BITWISE XOR_BITWISE LEFT_SHIFT RIGHT_SHIFT RIGHT_SHIFT_UNSIGNED

%token <string> PLUS_ASSIGN MINUS_ASSIGN MULTIPLY_ASSIGN DIVIDE_ASSIGN MODULUS_ASSIGN
%token <string> AND_BITWISE_ASSIGN OR_BITWISE_ASSIGN XOR_ASSIGN LEFT_SHIFT_ASSIGN RIGHT_SHIFT_ASSIGN RIGHT_SHIFT_UNSIGNED_ASSIGN

(* Other *)
%token <string> AT
%token <string> ELLIPSIS

%%
