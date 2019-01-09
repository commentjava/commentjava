%{
    open Ast
%}

%start expression
%type < Ast.expression > expression

%%

(* Section 15 Expressions *)

(* 15.8 *)
primary:
  | p=primary_no_new_array { p }
  (*| e=array_creation_expression { e }*)

primary_no_new_array:
  | l=literal { l }
  (*| t=type_ PERIOD CLASS { TypeLiteral(t) }*)
  (*| t=VOID PERIOD CLASS { TypeLiteral(t) }*)
  (* | THIS { ThisExpression(None) }
  | n=class_name PERIOD THIS { ThisExpression(Some n) } *)
  | L_PAR e=expression R_PAR { ParenthesizedExpression(e) }
  (*| class_instance_creation_expression { "" } *)
  | a=field_access { a }
  /* | method_invocation {}*) */
  | a=array_access { a }

literal:
  | l=INTEGER_LITERAL { NumberLiteral(l) }
  | l=FLOAT_LITERAL { NumberLiteral(l) }
  | l=BOOLEAN_LITERAL { BooleanLiteral(l) }
  | l=CHAR_LITERAL { CharacterLiteral(l) }
  | l=STRING_LITERAL { StringLiteral(l) }
  | l=NULL_LITERAL { NullLiteral }

(* 15.9 *)
(*class_instance_creation_expression:
  | NEW class_or_interface_type L_PAR R_PAR {}
  | NEW type_arguments class_or_interface_type L_PAR R_PAR {}
  | NEW class_or_interface_type L_PAR argument_list R_PAR {}
  | NEW class_or_interface_type L_PAR R_PAR class_body {}
  | NEW type_arguments class_or_interface_type L_PAR argument_list R_PAR {}
  | NEW type_arguments class_or_interface_type L_PAR R_PAR class_body {}
  | NEW class_or_interface_type L_PAR argument_list R_PAR class_body {}
  | NEW type_arguments class_or_interface_type L_PAR argument_list R_PAR class_body {} *)
  (*| primary PERIOD NEW identifier L_PAR R_PAR {}
  | primary PERIOD NEW type_arguments identifier L_PAR R_PAR {}
  | primary PERIOD NEW identifier type_arguments L_PAR R_PAR {}
  | primary PERIOD NEW identifier L_PAR argument_list R_PAR {}
  | primary PERIOD NEW identifier L_PAR R_PAR class_body {}
  | primary PERIOD NEW type_arguments identifier type_arguments L_PAR R_PAR {}
  | primary PERIOD NEW type_arguments identifier L_PAR argument_list R_PAR {}
  | primary PERIOD NEW type_arguments identifier L_PAR R_PAR class_body {}
  | primary PERIOD NEW identifier type_arguments L_PAR argument_list R_PAR {}
  | primary PERIOD NEW identifier type_arguments L_PAR R_PAR class_body {}
  | primary PERIOD NEW identifier L_PAR argument_list R_PAR class_body {}
  | primary PERIOD NEW type_arguments identifier type_arguments L_PAR argument_list R_PAR {}
  | primary PERIOD NEW type_arguments identifier type_arguments L_PAR R_PAR class_body {}
  | primary PERIOD NEW type_arguments identifier L_PAR argument_list R_PAR class_body {}
  | primary PERIOD NEW identifier type_arguments L_PAR argument_list R_PAR class_body {}
  | primary PERIOD NEW type_arguments identifier type_arguments L_PAR argument_list R_PAR class_body {}*)

(*
argument_list:
  | e=expression { e }
  | es=argument_list COMMA e=expression { es ^ e }
*)

(* 15.10 *)
(*
array_creation_expression:
  | NEW primitive_type dim_exprs {}
  | NEW primitive_type dim_exprs dims {}
  | NEW class_or_interface_type dim_exprs {}
  | NEW class_or_interface_type dim_exprs dims {}
  | NEW primitive_type dims array_initializer {}
  | NEW class_or_interface_type dims array_initializer {}

dim_exprs:
  | e=dim_expr { e }
  | es=dim_expr e=dim_expr { es ^ e }

dim_expr:
  | L_BRACKET e=expression R_BRACKET { e }

dims:
  | L_BRACKET R_BRACKET { "[]" }
  | d=dims L_BRACKET R_BRACKET { d ^ "[]" }
*)

(* 15.11 *)
field_access:
  | p=primary PERIOD i=identifier { FieldAccess(p, ExpressionName(SimpleName(i))) }
  | SUPER PERIOD i=identifier { SuperFieldAccess(None, ExpressionName(SimpleName(i))) }
  | n=expression_name PERIOD SUPER PERIOD i=identifier { SuperFieldAccess(Some n, ExpressionName(SimpleName(i))) }

(* 15.12 *)
(*
method_invocation:
  | method_name L_PAR R_PAR {}
  | method_name L_PAR argument_list R_PAR {}
  | primary PERIOD identifier L_PAR R_PAR {}
  | primary PERIOD non_wild_type_arguments identifier L_PAR R_PAR {}
  | primary PERIOD identifier L_PAR argument_list R_PAR {}
  | primary PERIOD non_wild_type_arguments identifier L_PAR argument_list R_PAR {}
  | SUPER PERIOD identifier L_PAR R_PAR {}
  | SUPER PERIOD non_wild_type_arguments identifier L_PAR R_PAR {}
  | SUPER PERIOD identifier L_PAR argument_list R_PAR {}
  | SUPER PERIOD non_wild_type_arguments identifier L_PAR argument_list R_PAR {}
  | class_name PERIOD SUPER PERIOD identifier L_PAR R_PAR {}
  | class_name PERIOD SUPER PERIOD non_wild_type_arguments identifier L_PAR R_PAR {}
  | class_name PERIOD SUPER PERIOD identifier L_PAR argument_list R_PAR {}
  | class_name PERIOD SUPER PERIOD non_wild_type_arguments identifier L_PAR argument_list R_PAR {}
  | type_name PERIOD non_wild_type_arguments identifier L_PAR R_PAR {}
  | type_name PERIOD non_wild_type_arguments identifier L_PAR argument_list R_PAR {}
*)

(* 15.13 *)
array_access:
  | en=expression_name L_BRACKET e=expression R_BRACKET { ArrayAccess(en, e) }
  | p=primary_no_new_array L_BRACKET e=expression R_BRACKET { ArrayAccess(p, e) }

(* 15.14 *)
postfix_expression:
  | p=primary { p }
  | en=expression_name { en }
  | e=post_increment_expression { e }
  | e=post_decrement_expression { e }

%public post_increment_expression:
  | e=postfix_expression INCREMENT { PostfixExpression(e, INCREMENT) }

%public post_decrement_expression:
  | e=postfix_expression DECREMENT { PostfixExpression(e, DECREMENT) }

(* 15.15 *)
unary_expression:
  | e=pre_increment_expression { e }
  | e=pre_decrement_expression { e }
  | PLUS e=unary_expression { PrefixExpression(e, PLUS) }
  | MINUS e=unary_expression { PrefixExpression(e, MINUS) }
  | e=unary_expression_not_plus_minus { e }

%public pre_increment_expression:
  | INCREMENT e=unary_expression { PrefixExpression(e, INCREMENT) }

%public pre_decrement_expression:
  | DECREMENT e=unary_expression { PrefixExpression(e, DECREMENT) }

unary_expression_not_plus_minus:
  | e=postfix_expression { e }
  | COMPLEMENT_BITWISE e=unary_expression { PrefixExpression(e, COMPLEMENT) }
  | NOT_LOGICAL e=unary_expression { PrefixExpression(e, NOT) }
  /* | e=cast_expresion { e } */

(* 15.16 *)
(*
cast_expresion:
  | L_PAR t=primitive_type R_PAR e=unary_expression { "(" ^ t ^ ")" ^ e}
  | L_PAR t=primitive_type d=dims R_PAR e=unary_expression { "(" ^ t ^ d ^ ")" ^ e}
  | L_PAR t=reference_type R_PAR e=unary_expression_not_plus_minus { "(" ^ t ^ ")" ^ e}
*)

(* 15.17 *)
multiplicative_expression:
  | e=unary_expression { e }
  | me=multiplicative_expression MULTIPLY e=unary_expression { InfixExpression(me, MULTIPLY, e) }
  | me=multiplicative_expression DIVIDE e=unary_expression { InfixExpression(me, DIVIDE, e) }
  | me=multiplicative_expression MODULO e=unary_expression { InfixExpression(me, MODULO, e) }

(* 15.18 *)
additive_expression:
  | e=multiplicative_expression { e }
  | ae=additive_expression PLUS e=multiplicative_expression { InfixExpression(ae, PLUS, e) }
  | ae=additive_expression MINUS e=multiplicative_expression { InfixExpression(ae, MINUS, e) }

(* 15.19 *)
shift_expression:
  | e=additive_expression { e }
  | se=shift_expression LEFT_SHIFT e=additive_expression { InfixExpression(se,LEFT_SHIFT, e) }
  | se=shift_expression RIGHT_SHIFT e=additive_expression { InfixExpression(se,RIGHT_SHIFT, e) }
  | se=shift_expression RIGHT_SHIFT_UNSIGNED e=additive_expression { InfixExpression(se,RIGHT_SHIFT_UNSIGNED, e) }

(* 15.20 *)
relational_expression:
  | e=shift_expression { e }
  | re=relational_expression LOWER e=shift_expression { InfixExpression(re, LOWER, e) }
  | re=relational_expression GREATER e=shift_expression { InfixExpression(re, GREATER, e) }
  | re=relational_expression LOWER_OR_EQUAL e=shift_expression { InfixExpression(re, LOWER_OR_EQUAL, e) }
  | re=relational_expression GREATER_OR_EQUAL e=shift_expression { InfixExpression(re, GREATER_OR_EQUAL, e) }
  /* | re=relational_expression INSTANCEOF t=reference_type { InstanceofExpression(re, t) } */

(* 15.21 *)
equality_expression:
  | e=relational_expression { e }
  | ee=equality_expression EQUAL e=relational_expression { InfixExpression(ee, EQUAL, e) }
  | ee=equality_expression NOT_EQUAL e=relational_expression { InfixExpression(ee, NOT_EQUAL, e) }

(* 15.22 *)
and_expression:
  | e=equality_expression { e }
  | ae=and_expression AND_BITWISE e=equality_expression { InfixExpression(ae, AND_BITWISE, e) }

exclusive_or_expression:
  | e=and_expression { e }
  | ee=exclusive_or_expression XOR_BITWISE e=and_expression { InfixExpression(ee, XOR_BITWISE, e) }

inclusive_or_expression:
  | e=exclusive_or_expression { e }
  | ie=inclusive_or_expression OR_BITWISE e=exclusive_or_expression { InfixExpression(ie, OR_BITWISE, e) }

(* 15.23 *)
conditional_and_expression:
  | e=inclusive_or_expression { e }
  | ce=conditional_and_expression AND_LOGICAL e=inclusive_or_expression { InfixExpression(ce, AND_LOGICAL, e) }

(* 15.24 *)
conditional_or_expression:
  | e=conditional_and_expression { e }
  | ce=conditional_or_expression OR_LOGICAL e=conditional_and_expression { InfixExpression(ce, OR_LOGICAL, e) }

(* 15.25 *)
%public conditional_expression:
  | e=conditional_or_expression { e }
  | coe=conditional_or_expression QUESTION_MARK e=expression COLON ce=conditional_expression { ConditionalExpression(coe, e, ce) }

(* 15.26 *)
assignment_expression:
  | e=conditional_expression { e }
  | a=assignment { a }

%public assignment:
  | lhs=left_hand_side o=assignment_operator e=assignment_expression { Assignment(lhs, o, e) }

left_hand_side:
  | n=expression_name { n }
  | a=field_access { a }
  | a=array_access { a }

assignment_operator:
  | ASSIGN { ASSIGN }
  | PLUS_ASSIGN { PLUS_ASSIGN }
  | MINUS_ASSIGN { MINUS_ASSIGN }
  | MULTIPLY_ASSIGN { MULTIPLY_ASSIGN }
  | DIVIDE_ASSIGN { DIVIDE_ASSIGN }
  | MODULUS_ASSIGN { MODULUS_ASSIGN }
  | AND_BITWISE_ASSIGN { AND_BITWISE_ASSIGN }
  | OR_BITWISE_ASSIGN { OR_BITWISE_ASSIGN }
  | XOR_ASSIGN { XOR_ASSIGN }
  | LEFT_SHIFT_ASSIGN { LEFT_SHIFT_ASSIGN }
  | RIGHT_SHIFT_ASSIGN { RIGHT_SHIFT_ASSIGN }
  | RIGHT_SHIFT_UNSIGNED_ASSIGN { RIGHT_SHIFT_UNSIGNED_ASSIGN }

(* 15.27 *)
%public expression:
  | e=assignment_expression { e }

(* 15.28 *)
%public constant_expression:
  | e=expression { e }
