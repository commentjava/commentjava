%{
  open Ast
%}

%start block
%type < string > block

%%

(* Section 14 Blocks and Statement *)

(* 14.2 *)
block:
  | L_BRACE R_BRACE { "{}" }
  | L_BRACE block_statements R_BRACE { "{" ^ $2 ^ "}" }

block_statements:
  | block_statement { $1 }
  | block_statements block_statement { $1 ^ " " ^ $2 }

block_statement:
  (*| local_variable_declaration_statement { "" }*)
  (* | class_declaration { "" } *)
  | statement { $1 }

(* 14.4 *)
(*local_variable_declaration_statement:
  | local_variable_declaration SEMICOLON { $1 ^ ";" }

local_variable_declaration:
  | variable_modifiers type_ variable_declarators { "" } *)

(* 14.5 *)
statement:
  | statement_without_trailing_substatement { $1 }
  | labeled_statement { $1 }
  | if_then_statement { $1 }
  | if_then_else_statement { $1 }
  | while_statement { $1 }
  (*| for_statement { $1 }*)

statement_without_trailing_substatement:
  | block { $1 }
  | empty_statement { $1 }
  (* | expression_statement { $1 }
  | assert_statement { $1 }
  | switch_statement { $1 }
  | do_statement { $1 }
  | break_statement { $1 }
  | continue_statement { $1 }
  | return_statement { $1 }
  | synchronized_statement { $1 }
  | throw_statement { $1 }
  | try_statement { $1 } *)

statement_no_short_if:
  | statement_without_trailing_substatement { $1 }
  | labeled_statement_no_short_if { $1 }
  | if_then_else_statement_no_short_if { $1 }
  | while_statement_no_short_if { $1 }
  (*| for_statement_no_short_if { $1 }*)

(* 14.6 *)
empty_statement:
  | SEMICOLON { ";" } 

(* 14.7 *)

labeled_statement:
  | identifier COLON statement { "labeled_statement(" ^ $1 ^ " : " ^ $3 ^ ")" }

labeled_statement_no_short_if:
  | identifier COLON statement_no_short_if { "labeled_statement(" ^ $1 ^ " : " ^ $3 ^ ")" }

(* 14.8 *)

expression_statement:
  | statement_expression SEMICOLON {""}

statement_expression:
  | assignment {"" }
  (*| pre_increment_expression
  | pre_decrement_expression
  | post_increment_expression
  | post_decrement_rexpression
  | method_invocation
  | class_instance_creation_expression {}*)


(* 14.9 *)

if_then_statement:
  | IF L_PAR expression R_PAR statement { "if(" ^ $3 ^ ")" ^ $5 }

if_then_else_statement:
  | IF L_PAR expression R_PAR statement_no_short_if ELSE statement { "if(" ^ $3 ^ ")" ^ $5 ^ "else" ^ $7 }

if_then_else_statement_no_short_if:
  | IF L_PAR expression R_PAR statement_no_short_if ELSE statement_no_short_if { "if(" ^ $3 ^ ")" ^ $5 ^ "else" ^ $7 }

(* 14.10 *)
(* assert_statement:
  | ASSERT expression SEMICOLON {}
  | ASSERT expression COLON expression SEMICOLON {} *)

(* 14.11 *)
(*
switch_statement:
  | SWITCH L_PAR expression R_PAR switch_block { "" }

switch_block:
  | L_BRACE R_BRACE { "" }
  | L_BRACE switch_block_statement_groups R_BRACE { "" }
  | L_BRACE switch_labels R_BRACE { "" }
  | L_BRACE switch_block_statement_groups switch_labels R_BRACE { "" }

switch_block_statement_groups:
  | switch_block_statement_group { $1 }
  | switch_block_statement_groups switch_block_statement_group { $1 ^ $2 }

switch_block_statement_group:
  | switch_labels block_statements {}

switch_labels:
  | switch_label
  | switch_labels switch_label {}

switch_label:
  | CASE constant_expression COLON
  | CASE enum_constant_name COLON
  | DEFAULT COLON {}

enum_constant_name:
  | identifier {} *)

(* 14.12 *)
while_statement:
  | WHILE L_PAR expression R_PAR statement { "while(" ^ $3 ^ ")" ^ $5 }

while_statement_no_short_if:
  | WHILE L_PAR expression R_PAR statement_no_short_if { "while(" ^ $3 ^ ")" ^ $5 }

(* 14.13 *)
(*
do_statement:
  | DO statement WHILE L_PAR expression R_PAR SEMICOLON {}
*)

(* 14.14 *)

for_statement:
  | basic_for_statement { $1 }
  (*| enhanced_for_statement { $1 }*) 

basic_for_statement:
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement { "for(;;)" ^ s }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement { "for(" ^ i ^ ";;)" ^ s }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement { "for(" ^ ";" ^ e ^ ";)" ^ s }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement { "for(" ^ ";" ^ ";" ^ u ^ ")" ^ s }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement { "for(" ^ i ^ ";" ^ e ^ ";" ^ ")" ^ s }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement { "for(" ^ i ^ ";" ^ ";" ^ u ^ ")" ^ s }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { "for(" ^ ";" ^ e ^ ";" ^ u ^ ")" ^ s }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { "for(" ^ ";" ^ e ^ ";" ^ u ^ ")" ^ s }

for_statement_no_short_if:
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { "for(;;)" ^ s }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { "for(" ^ i ^ ";;)" ^ s }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { "for(" ^ ";" ^ e ^ ";)" ^ s }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { "for(" ^ ";" ^ ";" ^ u ^ ")" ^ s }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { "for(" ^ i ^ ";" ^ e ^ ";" ^ ")" ^ s }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { "for(" ^ i ^ ";" ^ ";" ^ u ^ ")" ^ s }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { "for(" ^ ";" ^ e ^ ";" ^ u ^ ")" ^ s }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { "for(" ^ ";" ^ e ^ ";" ^ u ^ ")" ^ s }

for_init:
  | s=statement_expression_list { s }
  (*| local_variable_declaration {}*)

for_update:
  | s=statement_expression_list { s }

statement_expression_list:
  | s=statement_expression { s }
  | sl=statement_expression_list COMMA s=statement_expression { sl ^ "," ^ s }

(*
enhanced_for_statement:
  | FOR L_PAR type_ identifier COLON expression R_PAR statement {}
  | FOR L_PAR variable_modifiers type_ identifier COLON expression R_PAR statement {}
*)

(* 14.15 *)
(*break_statement:
  | BREAK SEMICOLON {}
  | BREAK identifier SEMICOLON {}
  *)

(* 14.16 *)
(*
continue_statement:
  | CONTINUE SEMICOLON {}
  | CONTINUE identifier SEMICOLON {}*)

(* 14.17 *)
(*return_statement:
  | RETURN SEMICOLON {}
  | RETURN expression SEMICOLON {} *)

(* 14.18 *)
(*throw_statement:
  | THROW expression SEMICOLON {} *)

(* 14.19 *)
(*
synchronized_statement:
  | SYNCHRONIZED L_PAR expression R_PAR block {}
*)

(* 14.20 *)
(* 
try_statement:
  | TRY block catches {}
  | TRY block catches finally {}
  | TRY block finally {}
*)

(*
catches:
  | catch_clause
  | catches catch_clause {}

catch_clause:
  | CATCH L_PAR formal_parameter R_PAR block {} 

finally:
  | FINALLY block {}
*)
