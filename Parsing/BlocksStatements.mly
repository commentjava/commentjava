%{
  open Ast
%}

%start block_main
%type < Ast.ast > block_main

%%

%public block_main:
  | b=block { Statement(b) }
(* Section 14 Blocks and Statement *)

(* 14.2 *)
block:
  | L_BRACE R_BRACE { Block([]) }
  | L_BRACE bs=block_statements R_BRACE { Block(bs) }

block_statements:
  | b=block_statement { [b] }
  | bs=block_statements b=block_statement { bs @ [b] }

block_statement:
  | s=local_variable_declaration_statement { s }
  /* | c=class_declaration { c } */
  | s=statement { s }

(* 14.4 *)
local_variable_declaration_statement:
  | d=local_variable_declaration SEMICOLON { d }

local_variable_declaration:
  | m=variable_modifiers t=type_ d=variable_declarators { VariableDeclarationStatement(m, t, d) }

(* 14.5 *)
statement:
  | s=statement_without_trailing_substatement { s }
  | s=labeled_statement { s }
  | s=if_then_statement { s }
  | s=if_then_else_statement { s }
  | s=while_statement { s }
  | s=for_statement { s }

statement_without_trailing_substatement:
  | b=block { b }
  | s=empty_statement { s }
  | s=expression_statement { s }
  | s=assert_statement { s }
  | s=switch_statement { s }
  | s=do_statement { s }
  | s=break_statement { s }
  | s=continue_statement { s }
  | s=return_statement { s }
  | s=synchronized_statement { s }
  | s=throw_statement { s }
  (*| try_statement { $1 } require formal_parameter *)

statement_no_short_if:
  | s=statement_without_trailing_substatement { s }
  | s=labeled_statement_no_short_if { s }
  | s=if_then_else_statement_no_short_if { s }
  | s=while_statement_no_short_if { s }
  (*| for_statement_no_short_if { $1 }*)

(* 14.6 *)
empty_statement:
  | SEMICOLON { EmptyStatement }

(* 14.7 *)

labeled_statement:
  | i=identifier COLON s=statement { LabeledStatement(i, s) }

labeled_statement_no_short_if:
  | i=identifier COLON s=statement_no_short_if { LabeledStatement(i, s) }

(* 14.8 *)

expression_statement:
  | e=statement_expression SEMICOLON { ExpressionStatement(e) }

statement_expression:
  | a=assignment { a }
  | e=pre_increment_expression { e }
  | e=pre_decrement_expression { e }
  | e=post_increment_expression { e }
  | e=post_decrement_expression { e }
  | e=method_invocation { e }
  /* | class_instance_creation_expression {} */


(* 14.9 *)

if_then_statement:
  | IF L_PAR e=expression R_PAR s=statement { IfStatement(e, s, None) }

if_then_else_statement:
  | IF L_PAR e=expression R_PAR s1=statement_no_short_if ELSE s2=statement { IfStatement(e, s1, Some s2) }

if_then_else_statement_no_short_if:
  | IF L_PAR e=expression R_PAR s1=statement_no_short_if ELSE s2=statement_no_short_if { IfStatement(e, s1, Some s2) }

(* 14.10 *)
assert_statement:
  | ASSERT e=expression SEMICOLON { AssertStatement([e]) }
  | ASSERT e1=expression COLON e2=expression SEMICOLON { AssertStatement([e1; e2]) }

(* 14.11 *)
switch_statement:
  | SWITCH L_PAR e=expression R_PAR b=switch_block { SwitchStatement(e, b) }

switch_block:
  | L_BRACE R_BRACE { [] }
  | L_BRACE b=switch_block_statement_groups R_BRACE { b }
  | L_BRACE l=switch_labels R_BRACE { l }
  | L_BRACE b=switch_block_statement_groups l=switch_labels R_BRACE { b @ l }

switch_block_statement_groups:
  | b=switch_block_statement_group { b }
  | bs=switch_block_statement_groups b=switch_block_statement_group { bs @ b }

switch_block_statement_group:
  | l=switch_labels b=block_statements { l @ b }

switch_labels:
  | l=switch_label { [l] }
  | ls=switch_labels l=switch_label { ls @ [l] }

switch_label:
  | CASE e=constant_expression COLON { SwitchCase(Some e) }
  (*| CASE enum_constant_name COLON { "caseTEST(" ^ $2 ^ "): "} NEVER MATCH SINCE constant_expression can also be indentifier? *)
  | DEFAULT COLON { SwitchCase(None) }

enum_constant_name:
  | i=identifier { Tree("enum_constant_name", [Leaf(i)]) }

(* 14.12 *)
while_statement:
  | WHILE L_PAR e=expression R_PAR s=statement { WhileStatement(e, s) }

while_statement_no_short_if:
  | WHILE L_PAR e=expression R_PAR s=statement_no_short_if { WhileStatement(e, s) }

(* 14.13 *)
do_statement:
  | DO s=statement WHILE L_PAR e=expression R_PAR SEMICOLON { DoStatement(s, e) }

(* 14.14 *)
for_statement:
  | s=basic_for_statement { s }
  (*| enhanced_for_statement { $1 } *)

basic_for_statement:
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement { ForStatement(None, None, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement { ForStatement(Some i, None, None, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement { ForStatement(None, Some e, None, s) }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement { ForStatement(None, None, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement { ForStatement(Some i, Some e, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement { ForStatement(Some i, None, Some u, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { ForStatement(None, Some e, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { ForStatement(Some i, Some e, Some u, s) }

for_statement_no_short_if:
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { ForStatement(None, None, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { ForStatement(Some i, None, None, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { ForStatement(None, Some e, None, s) }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(None, None, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { ForStatement(Some i, Some e, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(Some i, None, Some u, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(None, Some e, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(Some i, Some e, Some u, s) }

for_init:
  | s=statement_expression_list { s }
  (*| local_variable_declaration {}*)

for_update:
  | s=statement_expression_list { s }

statement_expression_list:
  | s=statement_expression { [s] }
  | sl=statement_expression_list COMMA s=statement_expression { sl @ [s] }

(*enhanced_for_statement:
  | FOR L_PAR type_ identifier COLON expression R_PAR statement { "for(typeast " ^ $4 ^ " : " ^ $6 ^ ")" ^ $8 }*)
    (* reduce/reduce conflict ambiguous_name -> IDENTIFIER / type_name -> IDENTIFIER *)
  (*| FOR L_PAR variable_modifiers type_ identifier COLON expression R_PAR statement {}*)

(* 14.15 *)
break_statement:
  | BREAK SEMICOLON { BreakStatement(None) }
  | BREAK i=identifier SEMICOLON { BreakStatement(Some i) }

(* 14.16 *)
continue_statement:
  | CONTINUE SEMICOLON { ContinueStatement(None) }
  | CONTINUE i=identifier SEMICOLON { ContinueStatement(Some i) }

(* 14.17 *)
return_statement:
  | RETURN SEMICOLON { ReturnStatement(None) }
  | RETURN e=expression SEMICOLON { ReturnStatement(Some e) }

(* 14.18 *)
throw_statement:
  | THROW e=expression SEMICOLON { ThrowStatement(e) }

(* 14.19 *)
synchronized_statement:
  | SYNCHRONIZED L_PAR e=expression R_PAR b=block { SynchronizedStatement(e, b) }

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
