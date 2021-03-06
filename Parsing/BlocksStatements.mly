%{
  open Ast
%}

%start block_main
%type < Ast.ast > block_main

%%

%public block_main: (* ast TODO : change to statement *)
  | b=block { Statement(b) }
(* Section 14 Blocks and Statement *)

(* 14.2 *)
%public block: (* statement *)
  | L_BRACE R_BRACE { Block([]) }
  | L_BRACE bs=block_statements R_BRACE { Block(bs) }

%public block_statements: (* statement list *)
  | b=block_statement { [b] }
  | bs=block_statements b=block_statement { bs @ [b] }

%public block_statement: (* statement *)
  | s=local_variable_declaration_statement { s }
  | c=class_declaration { LocalClassDeclarationStatement(c) }
  | s=statement { s }

(* 14.4 *)
local_variable_declaration_statement: (* statement *)
  | d=local_variable_declaration SEMICOLON { d }

local_variable_declaration: (* statement *)
  | t=type_ d=variable_declarators { VariableDeclarationStatement([], t, d) }
  | t=array_type d=variable_declarators { VariableDeclarationStatement([], t, d) }
  | m=variable_modifiers t=type_ d=variable_declarators { VariableDeclarationStatement(m, t, d) }
  | m=variable_modifiers t=array_type d=variable_declarators { VariableDeclarationStatement(m, t, d) }

(* 14.5 *)
statement: (* statement *)
  | s=statement_without_trailing_substatement { s }
  | s=labeled_statement { s }
  | s=if_then_statement { s }
  | s=if_then_else_statement { s }
  | s=while_statement { s }
  | s=for_statement { s }

statement_without_trailing_substatement: (* statement *)
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
  | s=try_statement { s }

statement_no_short_if: (* statement *)
  | s=statement_without_trailing_substatement { s }
  | s=labeled_statement_no_short_if { s }
  | s=if_then_else_statement_no_short_if { s }
  | s=while_statement_no_short_if { s }
  | s=for_statement_no_short_if { s }

(* 14.6 *)
empty_statement: (* statement *)
  | SEMICOLON { EmptyStatement }

(* 14.7 *)

labeled_statement: (* statement *)
  | i=identifier COLON s=statement { LabeledStatement(i, s) }

labeled_statement_no_short_if: (* statement *)
  | i=identifier COLON s=statement_no_short_if { LabeledStatement(i, s) }

(* 14.8 *)

expression_statement: (* statement *)
  | e=statement_expression SEMICOLON { e }

statement_expression: (* statement *)
  | a=assignment { ExpressionStatement(a) }
  | e=pre_increment_expression { ExpressionStatement(e) }
  | e=pre_decrement_expression { ExpressionStatement(e) }
  | e=post_increment_expression { ExpressionStatement(e) }
  | e=post_decrement_expression { ExpressionStatement(e) }
  | e=method_invocation { ExpressionStatement(e) }
  | e=class_instance_creation_expression { ExpressionStatement(e) }


(* 14.9 *)

if_then_statement: (* statement *)
  | IF L_PAR e=expression R_PAR s=statement { IfStatement(e, s, None) }

if_then_else_statement: (* statement *)
  | IF L_PAR e=expression R_PAR s1=statement_no_short_if ELSE s2=statement { IfStatement(e, s1, Some s2) }

if_then_else_statement_no_short_if: (* statement *)
  | IF L_PAR e=expression R_PAR s1=statement_no_short_if ELSE s2=statement_no_short_if { IfStatement(e, s1, Some s2) }

(* 14.10 *)
assert_statement: (* statement *)
  | ASSERT e=expression SEMICOLON { AssertStatement([e]) }
  | ASSERT e1=expression COLON e2=expression SEMICOLON { AssertStatement([e1; e2]) }

(* 14.11 *)
switch_statement: (* statement *)
  | SWITCH L_PAR e=expression R_PAR b=switch_block { SwitchStatement(e, b) }

switch_block: (* statement list *)
  | L_BRACE R_BRACE { [] }
  | L_BRACE b=switch_block_statement_groups R_BRACE { b }
  | L_BRACE l=switch_labels R_BRACE { l }
  | L_BRACE b=switch_block_statement_groups l=switch_labels R_BRACE { b @ l }

switch_block_statement_groups: (* statement list *)
  | b=switch_block_statement_group { b }
  | bs=switch_block_statement_groups b=switch_block_statement_group { bs @ b }

switch_block_statement_group: (* statement list *)
  | l=switch_labels b=block_statements { l @ b }

switch_labels: (* statement list *)
  | l=switch_label { [l] }
  | ls=switch_labels l=switch_label { ls @ [l] }

switch_label: (* statement *)
  | CASE e=constant_expression COLON { SwitchCase(Some e) }
  (*| CASE enum_constant_name COLON { "caseTEST(" ^ $2 ^ "): "} NEVER MATCH SINCE constant_expression can also be indentifier? *)
  | DEFAULT COLON { SwitchCase(None) }

enum_constant_name: (* ast TODO : change *)
  | i=identifier { Tree("enum_constant_name", [Leaf(i)]) }

(* 14.12 *)
while_statement: (* statement *)
  | WHILE L_PAR e=expression R_PAR s=statement { WhileStatement(e, s) }

while_statement_no_short_if: (* statement *)
  | WHILE L_PAR e=expression R_PAR s=statement_no_short_if { WhileStatement(e, s) }

(* 14.13 *)
do_statement: (* statement *)
  | DO s=statement WHILE L_PAR e=expression R_PAR SEMICOLON { DoStatement(s, e) }

(* 14.14 *)
for_statement: (* statement *)
  | s=basic_for_statement { s }
  | s=enhanced_for_statement { s }

basic_for_statement: (* statement *)
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement { ForStatement(None, None, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement { ForStatement(Some i, None, None, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement { ForStatement(None, Some e, None, s) }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement { ForStatement(None, None, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement { ForStatement(Some i, Some e, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement { ForStatement(Some i, None, Some u, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { ForStatement(None, Some e, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement { ForStatement(Some i, Some e, Some u, s) }

for_statement_no_short_if: (* statement *)
  | FOR L_PAR SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { ForStatement(None, None, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON R_PAR s=statement_no_short_if { ForStatement(Some i, None, None, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { ForStatement(None, Some e, None, s) }
  | FOR L_PAR SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(None, None, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON R_PAR s=statement_no_short_if { ForStatement(Some i, Some e, None, s) }
  | FOR L_PAR i=for_init SEMICOLON SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(Some i, None, Some u, s) }
  | FOR L_PAR SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(None, Some e, Some u, s) }
  | FOR L_PAR i=for_init SEMICOLON e=expression SEMICOLON u=for_update R_PAR s=statement_no_short_if { ForStatement(Some i, Some e, Some u, s) }

for_init: (* statement list *)
  | s=statement_expression_list { s }
  | s=local_variable_declaration { [s] }

for_update: (* statement list *)
  | s=statement_expression_list { s }

statement_expression_list: (* statement list *)
  | s=statement_expression { [s] }
  | sl=statement_expression_list COMMA s=statement_expression { sl @ [s] }

enhanced_for_statement: (* statement *)
  | FOR L_PAR t=type_ i=identifier COLON e=expression R_PAR s=statement { EnhancedForStatement(SingleVariableDeclaration(None, t, None, false, i, 0, None), e, s) }
  | FOR L_PAR t=array_type i=identifier COLON e=expression R_PAR s=statement { EnhancedForStatement(SingleVariableDeclaration(None, t, None, false, i, 0, None), e, s) }
  | FOR L_PAR vm=variable_modifiers t=type_ i=identifier COLON e=expression R_PAR s=statement { EnhancedForStatement(SingleVariableDeclaration(Some vm, t, None, false, i, 0, None), e, s)}
  | FOR L_PAR vm=variable_modifiers t=array_type i=identifier COLON e=expression R_PAR s=statement { EnhancedForStatement(SingleVariableDeclaration(Some vm, t, None, false, i, 0, None), e, s)}

(* 14.15 *)
break_statement: (* statement *)
  | BREAK SEMICOLON { BreakStatement(None) }
  | BREAK i=identifier SEMICOLON { BreakStatement(Some i) }

(* 14.16 *)
continue_statement: (* statement *)
  | CONTINUE SEMICOLON { ContinueStatement(None) }
  | CONTINUE i=identifier SEMICOLON { ContinueStatement(Some i) }

(* 14.17 *)
return_statement: (* statement *)
  | RETURN SEMICOLON { ReturnStatement(None) }
  | RETURN e=expression SEMICOLON { ReturnStatement(Some e) }

(* 14.18 *)
throw_statement: (* statement *)
  | THROW e=expression SEMICOLON { ThrowStatement(e) }

(* 14.19 *)
synchronized_statement: (* statement *)
  | SYNCHRONIZED L_PAR e=expression R_PAR b=block { SynchronizedStatement(e, b) }

(* 14.20 *)
try_statement: (* statement *)
  | TRY b=block cl=catches { TryStatement(b, Some cl, None) }
  | TRY b=block cl=catches bf=finally { TryStatement(b, Some cl, Some bf)}
  | TRY b=block bf=finally { TryStatement(b, None, Some bf) }

catches: (* catch_clause list *)
  | c=catch_clause { [c] }
  | cl=catches c=catch_clause { cl @ [c] }

catch_clause: (* catch_clause *)
  | CATCH L_PAR p=formal_parameter R_PAR b=block { CatchClause(p, b) }

finally: (* statement *)
  | FINALLY b=block { b }

