%token EOF

(* Identifiers - 3.8*)
%token < string > IDENTIFIER

(* Keywords - 3.9 *)
%token ABSTRACT ASSERT
%token BOOLEAN BREAK BYTE
%token CASE CATCH CHAR CLASS CONST CONTINUE 
%token DEFAULT DO DOUBLE
%token ELSE ENUM EXTENDS
%token FINAL FINALLY FLOAT FOR
%token GOTO
%token IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE
%token LONG
%token NATIVE NEW
%token PACKAGE PRIVATE PROTECTED PUBLIC
%token RETURN
%token SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
%token THIS THROW THROWS TRANSIENT TRY
%token VOID VOLATILE
%token WHILE

(* Literals - 3.10 *)
%token < int > INTEGER_LITERAL
%token < int > LONG_LITERAL
%token < float > FLOAT_LITERAL
%token < boolean > BOOLEAN_LITERAL
%token < char > CHAR_LITERAL
%token < string > STRING_LITERAL
%token NULL_LITERAL

(* Separators - 3.11 *)
%token L_PAR R_PAR
%token L_BRACE R_BRACE
%token L_BRACKET R_BRACKET
%token SEMICOLON COMMA PERIOD

(* Operators - 3.12 *)
%token ASSIGN
%token EQUAL NOT_EQUAL LOWER GREATER LOWER_OR_EQUAL GREATER_OR_EQUAL 
%token TERNARY_THEN TERNARY_ELSE
%token AND_LOGICAL OR_LOGICAL COMPLEMENT_LOGICAL
%token INCREMENT DECREMENT
%token PLUS MINUS MULTIPLY DIVIDE MODULO
%token COMPLEMENT_BITWISE AND_BITWISE OR_BITWISE XOR LEFT_SHIFT RIGHT_SHIFT RIGHT_SHIFT_UNSIGNED

%token PLUS_ASSIGN MINUS_ASSIGN MULTIPLY_ASSIGN DIVIDE_ASSIGN MODULUS_ASSIGN
%token AND_BITWISE_ASSIGN OR_BITWISE_ASSIGN XOR_ASSIGN LEFT_SHIFT_ASSIGN RIGHT_SHIFT_ASSIGN RIGHT_SHIFT_UNSIGNED_ASSIGN

%%
