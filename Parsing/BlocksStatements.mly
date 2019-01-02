%{
  open Ast
%}

%start block
%type < Ast.ast > block

%%

(* Section 14 Blocks and Statement *)

(* 14.2 *)
block:
  | L_BRACE R_BRACE { Leaf("block") }
  | L_BRACE bs=block_statements R_BRACE { Tree("block", [bs]) }

block_statements:
  | b=block_statement {  Tree("block_statements", [b]) }
  | bs=block_statements b=block_statement { Tree("block_statements", [bs; b]) }

block_statement:
  (*| local_variable_declaration_statement { "" }*)
  (* | class_declaration { "" } *)
  | s=statement { Tree("block_statement", [s]) }

(* 14.4 *)
(*local_variable_declaration_statement:
  | local_variable_declaration SEMICOLON { $1 ^ ";" }
*)
(*
local_variable_declaration:
  | variable_modifiers type_ variable_declarators { "" } *)

(* 14.5 *)
statement:
  | s=statement_without_trailing_substatement { Tree("statement", [s]) }
  | s=labeled_statement { Tree("statement", [s]) }
  | s=if_then_statement { Tree("statement", [s]) }
  | s=if_then_else_statement { Tree("statement", [s]) }
  | s=while_statement { Tree("statement", [s]) }
  | s=for_statement { Tree("statement", [s]) }

statement_without_trailing_substatement:
  | b=block { Tree("statement_without_trailing_substatement", [b]) }
  | s=empty_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=expression_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=assert_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=switch_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=do_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=break_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=continue_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=return_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=synchronized_statement { Tree("statement_without_trailing_substatement", [s]) }
  | s=throw_statement { Tree("statement_without_trailing_substatement", [s]) }
  (*| try_statement { $1 } require formal_parameter *)

statement_no_short_if:
  | s=statement_without_trailing_substatement { Tree("statement_no_short_if", [s]) }
  | s=labeled_statement_no_short_if { Tree("statement_no_short_if", [s]) }
  | s=if_then_else_statement_no_short_if { Tree("statement_no_short_if", [s]) }
  | s=while_statement_no_short_if { Tree("statement_no_short_if", [s]) }
  (*| for_statement_no_short_if { $1 }*)

(* 14.6 *)
empty_statement:
  | SEMICOLON { Leaf("empty_statement") }

(* 14.7 *)

labeled_statement:
  | i=identifier COLON s=statement { Tree("labeled_statement", [Leaf(i); s]) }

labeled_statement_no_short_if:
  | i=identifier COLON s=statement_no_short_if { Tree("labeled_statement_no_short_if", [Leaf(i); s]) }

(* 14.8 *)

expression_statement:
  | e=statement_expression SEMICOLON { Tree("expression_statement", [e]) }

statement_expression:
  | a=assignment { Tree("statement_expression", [Expression(a)]) }
  (*| pre_increment_expression
  | pre_decrement_expression
  | post_increment_expression
  | post_decrement_rexpression
  | method_invocation
  | class_instance_creation_expression {}*)


(* 14.9 *)

if_then_statement:
  | IF L_PAR e=expression R_PAR s=statement { Tree("if_then_statement", [e; s]) }

if_then_else_statement:
  | IF L_PAR e=expression R_PAR s1=statement_no_short_if ELSE s2=statement { Tree("if_then_else_statement", [e; s1; s2]) }

if_then_else_statement_no_short_if:
  | IF L_PAR e=expression R_PAR s1=statement_no_short_if ELSE s2=statement_no_short_if { Tree("if_then_else_statement_no_short_if", [e; s1; s2]) }

(* 14.10 *)
assert_statement:
  | ASSERT e=expression SEMICOLON { Tree("assert_statement", [e]) }
  | ASSERT e1=expression COLON e2=expression SEMICOLON { Tree("assert_statement", [e1; e2]) }

(* 14.11 *)
switch_statement:
  | SWITCH L_PAR e=expression R_PAR b=switch_block { Tree("switch_statement", [e; b]) }

switch_block:
  | L_BRACE R_BRACE { Leaf("switch_block") }
  | L_BRACE b=switch_block_statement_groups R_BRACE { Tree("switch_block", [b]) }
  | L_BRACE l=switch_labels R_BRACE { Tree("switch_block", [l]) }
  | L_BRACE b=switch_block_statement_groups l=switch_labels R_BRACE { Tree("switch_block", [b; l])  }

switch_block_statement_groups:
  | b=switch_block_statement_group { Tree("switch_block_statement_groups", [b]) }
  | bs=switch_block_statement_groups b=switch_block_statement_group { Tree("switch_block_statement_groups", [bs; b]) }

switch_block_statement_group:
  | l=switch_labels b=block_statements { Tree("switch_block_statement_group", [l; b]) }

switch_labels:
  | l=switch_label { Tree("switch_labels", [l]) }
  | ls=switch_labels l=switch_label { Tree("switch_labels", [ls; l]) }

switch_label:
  | CASE e=constant_expression COLON { Tree("switch_label", [e]) }
  (*| CASE enum_constant_name COLON { "caseTEST(" ^ $2 ^ "): "} NEVER MATCH SINCE constant_expression can also be indentifier? *)
  | DEFAULT COLON { Leaf("switch_label") }

enum_constant_name:
  | i=identifier { Tree("enum_constant_name", [Leaf(i)]) }

(* 14.12 *)
while_statement:
  | WHILE L_PAR e=expression R_PAR s=statement { Tree("while_statement", [e; s]) }

while_statement_no_short_if:
  | WHILE L_PAR e=expression R_PAR s=statement_no_short_if { Tree("while_statement", [e; s]) }

(* 14.13 *)
do_statement:
  | DO s=statement WHILE L_PAR e=expression R_PAR SEMICOLON { Tree("do_statement", [s; e]) }

(* 14.14 *)
for_statement:
  | s=basic_for_statement { Tree("for_statement", [s]) }
  (*| enhanced_for_statement { $1 } *)

basic_for_statement:
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement { Tree("basic_for_statement", [s]) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement { Tree("basic_for_statement", [i; s]) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement { Tree("basic_for_statement", [e; s]) }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement { Tree("basic_for_statement", [u; s]) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement { Tree("basic_for_statement", [i; e; s]) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement { Tree("basic_for_statement", [i; u; s]) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { Tree("basic_for_statement", [e; u; s]) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { Tree("basic_for_statement", [i; e; u; s]) }

for_statement_no_short_if:
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [s]) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [i; s]) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [e; s]) }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [u; s]) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [i; e; s]) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [i; u; s]) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [e; u; s]) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { Tree("for_statement_no_short_if", [i; e; u; s]) }

for_init:
  | s=statement_expression_list { Tree("for_init", [s]) }
  (*| local_variable_declaration {}*)

for_update:
  | s=statement_expression_list { Tree("for_update", [s]) }

statement_expression_list:
  | s=statement_expression { Tree("statement_expression_list", [s]) }
  | sl=statement_expression_list COMMA s=statement_expression { Tree("statement_expression_list", [sl; s]) }

(*enhanced_for_statement:
  | FOR L_PAR type_ identifier COLON expression R_PAR statement { "for(typeast " ^ $4 ^ " : " ^ $6 ^ ")" ^ $8 }*)
    (* reduce/reduce conflict ambiguous_name -> IDENTIFIER / type_name -> IDENTIFIER *)
  (*| FOR L_PAR variable_modifiers type_ identifier COLON expression R_PAR statement {}*)

(* 14.15 *)
break_statement:
  | BREAK SEMICOLON { Leaf("break_statement") }
  | BREAK i=identifier SEMICOLON { Tree("break_statement", [Leaf(i)]) }

(* 14.16 *)
continue_statement:
  | CONTINUE SEMICOLON { Leaf("continue_statement") }
  | CONTINUE i=identifier SEMICOLON { Tree("continue_statement", [Leaf(i)]) }

(* 14.17 *)
return_statement:
  | RETURN SEMICOLON { Leaf("return_statement") }
  | RETURN e=expression SEMICOLON { Tree("return_statement", [e]) }

(* 14.18 *)
throw_statement:
  | THROW e=expression SEMICOLON { Tree("throw_statement", [e]) }

(* 14.19 *)
synchronized_statement:
  | SYNCHRONIZED L_PAR e=expression R_PAR b=block { Tree("synchronized_statement", [e; b]) }

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
