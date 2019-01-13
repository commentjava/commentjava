%{
    open Ast
%}

%type < Ast.expression > expression

%%

(* Section 15 Expressions *)

(* 15.8 *)
%public %inline primary:
  | p=primary_no_new_array { p }
  | e=array_creation_expression { e }

%inline primary_no_new_array:
  | l=literal { l }
  | t=name PERIOD CLASS {  TypeLiteral(Some (SimpleType(t))) }
  /* | t=type_ PERIOD CLASS {  TypeLiteral(Some t) } */
  | VOID PERIOD CLASS { TypeLiteral(None) }
  | THIS { ThisExpression(None) }
  | n=name PERIOD THIS { ThisExpression(Some n) }
  | L_PAR e=expression R_PAR { ParenthesizedExpression(e) }
  | c=class_instance_creation_expression { c }
  | a=field_access { a }
  | mi=method_invocation { mi }
  | a=array_access { a }

literal:
  | l=INTEGER_LITERAL { NumberLiteral(l) }
  | l=FLOAT_LITERAL { NumberLiteral(l) }
  | l=BOOLEAN_LITERAL { BooleanLiteral(l) }
  | l=CHAR_LITERAL { CharacterLiteral(l) }
  | l=STRING_LITERAL { StringLiteral(l) }
  | l=NULL_LITERAL { NullLiteral }

(* 15.9 *)
class_instance_creation_expression:
  | NEW t=class_or_interface_type L_PAR R_PAR { ClassInstanceCreation(None, None, t, None, None) }
  | NEW a=type_arguments t=class_or_interface_type L_PAR R_PAR { ClassInstanceCreation(None, Some a, t, None, None) }
  | NEW t=class_or_interface_type L_PAR al=argument_list R_PAR { ClassInstanceCreation(None, None, t, Some al, None) }
  | NEW t=class_or_interface_type L_PAR R_PAR cb=class_body { ClassInstanceCreation(None, None, t, None, cb) }
  | NEW a=type_arguments t=class_or_interface_type L_PAR al=argument_list R_PAR { ClassInstanceCreation(None, Some a, t, Some al, None) }
  | NEW a=type_arguments t=class_or_interface_type L_PAR R_PAR cb=class_body { ClassInstanceCreation(None, Some a, t, None, cb) }
  | NEW t=class_or_interface_type L_PAR al=argument_list R_PAR cb=class_body { ClassInstanceCreation(None, None, t, Some al, cb) }
  | NEW a=type_arguments t=class_or_interface_type L_PAR al=argument_list R_PAR cb=class_body { ClassInstanceCreation(None, Some a, t, Some al, cb) }
  | p=primary PERIOD NEW i=identifier L_PAR R_PAR { ClassInstanceCreation(Some [p], None, SimpleType(ExpressionName(SimpleName(i))), None, None) }
  | p=primary PERIOD NEW a=type_arguments i=identifier L_PAR R_PAR { ClassInstanceCreation(Some [p], Some a, SimpleType(ExpressionName(SimpleName(i))), None, None) }
  /* | p=primary PERIOD NEW i=identifier a2=type_arguments L_PAR R_PAR { ClassInstanceCreation(Some [p], Some a, SimpleType(ExpressionName(SimpleName(i))), None, None) } */
  | p=primary PERIOD NEW i=identifier L_PAR al=argument_list R_PAR { ClassInstanceCreation(Some [p], None, SimpleType(ExpressionName(SimpleName(i))), Some al, None) }
  | p=primary PERIOD NEW i=identifier L_PAR R_PAR cb=class_body { ClassInstanceCreation(Some [p], None, SimpleType(ExpressionName(SimpleName(i))), None, cb) }
  /* | p=primary PERIOD NEW a=type_arguments i=identifier a2=type_arguments L_PAR R_PAR {} */
  | p=primary PERIOD NEW a=type_arguments i=identifier L_PAR al=argument_list R_PAR { ClassInstanceCreation(Some [p], Some a, SimpleType(ExpressionName(SimpleName(i))), Some al, None) }
  | p=primary PERIOD NEW a=type_arguments i=identifier L_PAR R_PAR cb=class_body { ClassInstanceCreation(Some [p], Some a, SimpleType(ExpressionName(SimpleName(i))), None, cb) }
  /* | p=primary PERIOD NEW i=identifier a2=type_arguments L_PAR al=argument_list R_PAR { ClassInstanceCreation(Some [p], Some a, SimpleType(ExpressionName(SimpleName(i))), None, cb) } */
  /* | p=primary PERIOD NEW i=identifier a2=type_arguments L_PAR R_PAR cb=class_body { ClassInstanceCreation(Some [p], Some a, SimpleType(ExpressionName(SimpleName(i))), None, cb) } */
  | p=primary PERIOD NEW i=identifier L_PAR al=argument_list R_PAR cb=class_body { ClassInstanceCreation(Some [p], None, SimpleType(ExpressionName(SimpleName(i))), Some al, cb) }
  /* | p=primary PERIOD NEW a=type_arguments i=identifier a2=type_arguments L_PAR al=argument_list R_PAR {} */
  /* | p=primary PERIOD NEW a=type_arguments i=identifier a2=type_arguments L_PAR R_PAR cb=class_body {} */
  | p=primary PERIOD NEW a=type_arguments i=identifier L_PAR al=argument_list R_PAR cb=class_body { ClassInstanceCreation(Some [p], Some a, SimpleType(ExpressionName(SimpleName(i))), Some al, cb) }
  /* | p=primary PERIOD NEW i=identifier a2=type_arguments L_PAR al=argument_list R_PAR cb=class_body {} */
  /* | p=primary PERIOD NEW a=type_arguments i=identifier a2=type_arguments L_PAR al=argument_list R_PAR cb=class_body {} */

%public argument_list: (* expression list *)
  | e=expression { [e] }
  | al=argument_list COMMA e=expression { al @ [e] }

(* 15.10 *)
array_creation_expression:
  | NEW t=primitive_type de=dim_exprs { ArrayCreation(t, Some de, 0, None) }
  | NEW t=primitive_type de=dim_exprs d=dims { ArrayCreation(t, Some de, d, None) }
  | NEW t=class_or_interface_type de=dim_exprs { ArrayCreation(t, Some de, 0, None) }
  | NEW t=class_or_interface_type de=dim_exprs d=dims { ArrayCreation(t, Some de, d, None) }
  | NEW t=primitive_type d=dims ai=array_initializer { ArrayCreation(t, None, d, Some ai) }
  | NEW t=class_or_interface_type d=dims ai=array_initializer { ArrayCreation(t, None, d, Some ai) }

dim_exprs:
  | e=dim_expr { [e] }
  | es=dim_expr e=dim_expr { [es; e] }

dim_expr:
  | L_BRACKET e=expression R_BRACKET { e }

%public dims:
  | L_BRACKET R_BRACKET { 1 }
  | d=dims L_BRACKET R_BRACKET { d+1 }

(* 15.11 *)
field_access:
  | p=primary PERIOD i=identifier { FieldAccess(p, ExpressionName(SimpleName(i))) }
  | SUPER PERIOD i=identifier { SuperFieldAccess(None, ExpressionName(SimpleName(i))) }
  | n=name PERIOD SUPER PERIOD i=identifier { SuperFieldAccess(Some n, ExpressionName(SimpleName(i))) }

(* 15.12 *)
%public method_invocation:
  | n=name L_PAR R_PAR { MethodInvocation(None, None, n, None) }
  | n=name L_PAR al=argument_list R_PAR { MethodInvocation(None, None, n, Some al) }
  | p=primary PERIOD i=identifier L_PAR R_PAR { MethodInvocation(Some [p], None, ExpressionName(SimpleName(i)), None) }
  | p=primary PERIOD ta=non_wild_type_arguments i=identifier L_PAR R_PAR { MethodInvocation(Some [p], Some ta, ExpressionName(SimpleName(i)), None) }
  | p=primary PERIOD i=identifier L_PAR al=argument_list R_PAR { MethodInvocation(Some [p], None, ExpressionName(SimpleName(i)), Some al) }
  | p=primary PERIOD ta=non_wild_type_arguments i=identifier L_PAR al=argument_list R_PAR { MethodInvocation(Some [p], Some ta, ExpressionName(SimpleName(i)), Some al) }
  | SUPER PERIOD i=identifier L_PAR R_PAR { SuperMethodInvocation(None, None, ExpressionName(SimpleName(i)), None) }
  | SUPER PERIOD ta=non_wild_type_arguments i=identifier L_PAR R_PAR { SuperMethodInvocation(None, Some ta, ExpressionName(SimpleName(i)), None) }
  | SUPER PERIOD i=identifier L_PAR al=argument_list R_PAR { SuperMethodInvocation(None, None, ExpressionName(SimpleName(i)), Some al) }
  | SUPER PERIOD ta=non_wild_type_arguments i=identifier L_PAR al=argument_list R_PAR { SuperMethodInvocation(None, Some ta, ExpressionName(SimpleName(i)), Some al) }
  | n=name PERIOD SUPER PERIOD i=identifier L_PAR R_PAR { SuperMethodInvocation(Some [n], None, ExpressionName(SimpleName(i)), None) }
  | n=name PERIOD SUPER PERIOD ta=non_wild_type_arguments i=identifier L_PAR R_PAR { SuperMethodInvocation(Some [n], Some ta, ExpressionName(SimpleName(i)), None) }
  | n=name PERIOD SUPER PERIOD i=identifier L_PAR al=argument_list R_PAR { SuperMethodInvocation(Some [n], None, ExpressionName(SimpleName(i)), Some al) }
  | n=name PERIOD SUPER PERIOD ta=non_wild_type_arguments i=identifier L_PAR al=argument_list R_PAR { SuperMethodInvocation(Some [n], Some ta, ExpressionName(SimpleName(i)), Some al) }
  | n=name PERIOD ta=non_wild_type_arguments i=identifier L_PAR R_PAR { MethodInvocation(Some [n], Some ta, ExpressionName(SimpleName(i)), None) }
  | n=name PERIOD ta=non_wild_type_arguments i=identifier L_PAR al=argument_list R_PAR { MethodInvocation(Some [n], Some ta, ExpressionName(SimpleName(i)), Some al) }

(* 15.13 *)
array_access:
  | en=name L_BRACKET e=expression R_BRACKET { ArrayAccess(en, e) }
  | p=primary_no_new_array L_BRACKET e=expression R_BRACKET { ArrayAccess(p, e) }

(* 15.14 *)
postfix_expression:
  | p=primary { p }
  | en=name { en }
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
  | L_PAR re=name LOWER e=shift_expression R_PAR { ParenthesizedExpression(InfixExpression(re, LOWER, e)) } (* Force to match (a < b) before (s<T>) *)
  | e=postfix_expression { e }
  | COMPLEMENT_BITWISE e=unary_expression { PrefixExpression(e, COMPLEMENT) }
  | NOT_LOGICAL e=unary_expression { PrefixExpression(e, NOT) }
  | e=cast_expresion { e }

(* 15.16 *)
cast_expresion:
  | L_PAR t=primitive_type R_PAR e=unary_expression { CastExpression(t, e) }
  | L_PAR t=primitive_type d=dims R_PAR e=unary_expression { CastExpression(ArrayType(t, d), e) }
  | L_PAR t=reference_type R_PAR e=unary_expression_not_plus_minus { CastExpression(t, e) }
  | L_PAR t=array_type R_PAR e=unary_expression_not_plus_minus { CastExpression(t, e) }

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
  (* TODO: check this again because it may be broken but it is maybe not important ¯\_(ツ)_/¯*)
  | re=relational_expression INSTANCEOF t=reference_type { InstanceofExpression(re, t) }
  | re=relational_expression INSTANCEOF t=array_type { InstanceofExpression(re, t) }

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
%public conditional_expression: (* expression *)
  | e=conditional_or_expression { e }
  | coe=conditional_or_expression QUESTION_MARK e=expression COLON ce=conditional_expression { ConditionalExpression(coe, e, ce) }

(* 15.26 *)
assignment_expression: (* expression *)
  | e=conditional_expression { e }
  | a=assignment { a }

%public assignment:
  | lhs=left_hand_side o=assignment_operator e=assignment_expression { Assignment(lhs, o, e) }

left_hand_side:
  | n=name { n }
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
%public expression: (* expression *)
  | e=assignment_expression { e }

(* 15.28 *)
%public constant_expression:
  | e=expression { e }
