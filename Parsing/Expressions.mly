%start expression
%type < string > expression

%%

(* Section 15 Expressions *)

(* 15.8 *)
primary:
  | p=primary_no_new_array { p }
  (*| e=array_creation_expression { e }*)

primary_no_new_array:
  | l=literal { l }
  (*| TYPE PERIOD CLASS {}*)
  (*| VOID PERIOD CLASS { $1 }*)
  (*| THIS { "this" }
  | class_name PERIOD THIS { "" }
  | L_PAR e=expression R_PAR { e }
  | class_instance_creation_expression { "" }
  | field_access {}
  | method_invocation {}
  | array_access {}*)

literal:
  | l=INTEGER_LITERAL { l }
  | l=FLOAT_LITERAL { l }
  | l=BOOLEAN_LITERAL { l }
  | l=CHAR_LITERAL { l }
  | l=STRING_LITERAL { l }
  | l=NULL_LITERAL { l }

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
(*
field_access:
  | primary PERIOD identifier {}
  | SUPER PERIOD identifier {}
  | class_name PERIOD SUPER PERIOD identifier {}
*)

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
(*
array_access:
  | en=expression_name L_BRACKET e=expression R_BRACKET { en ^ "[" ^ e ^ "]" }
  | p=primary_no_new_array L_BRACKET e=expression R_BRACKET { p ^ "[" ^ e ^ "]" }
*)

(* 15.14 *)
postfix_expression:
  | p=primary { p }
  | en=expression_name { en }
  (*| e=post_increment_expression { e }
  | e=post_decrement_expression { e }*)

post_increment_expression:
  | e=postfix_expression INCREMENT { e ^ "++" }

post_decrement_expression:
  | e=postfix_expression DECREMENT { e ^ "--" }

(* 15.15 *)
unary_expression:
  (*| e=pre_increment_expression { e }
  | e=pre_decrement_expression { e } *)
  (*| PLUS e=unary_expression { "+" ^ e }
  | MINUS e=unary_expression { "-" ^ e }*)
  | e=unary_expression_not_plus_minus { e }

(*
pre_increment_expression:
  | INCREMENT e=unary_expression { "++" ^ e }

pre_decrement_expression:
  | DECREMENT e=unary_expression { "--" ^ e }
*)

unary_expression_not_plus_minus:
  | e=postfix_expression { e }
  (*| COMPLEMENT_BITWISE e=unary_expression { "~" ^ e }
  | NOT_LOGICAL e=unary_expression { "!" ^ e }*)
  (*| e=cast_expresion { e }*)

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
  (*
  | me=multiplicative_expression MULTIPLY e=unary_expression { me ^ "*" e }
  | me=multiplicative_expression DIVIDE e=unary_expression { me ^ "/" e }
  | me=multiplicative_expression MODULO e=unary_expression { me ^ "%" e }
*)

(* 15.18 *)
additive_expression:
  | e=multiplicative_expression { e }
  (*| ae=additive_expression PLUS e=multiplicative_expression { ae ^ "+" ^ e }
  | ae=additive_expression MINUS e=multiplicative_expression { ae ^ "-" ^ e }
  *)

(* 15.19 *)
shift_expression:
  | e=additive_expression { e }
(*
  | se=shift_expression LEFT_SHIFT e=addition_expression { se ^ "<<" ^ e }
  | se=shift_expression RIGHT_SHIFT e=addition_expression { se ^ "<<" ^ e }
  | se=shift_expression RIGHT_SHIFT_UNSIGNED e=addition_expression { se ^ "<<<" ^ e }
*)

(* 15.20 *)
relational_expression:
  | e=shift_expression { e }
  (*
  | re=relational_expression LOWER e=shift_expression { re ^ "<" ^ e }
  | re=relational_expression GREATER e=shift_expression { re ^ ">" ^ e }
  | re=relational_expression LOWER_OR_EQUAL e=shift_expression { re ^ "<=" ^ e }
  | re=relational_expression GREATER_OR_EQUAL e=shift_expression { re ^ ">=" ^ e }
  | re=relational_expression INSTANCEOF t=reference_type { re ^ " instanceof " ^ t }
  *)

(* 15.21 *)
equality_expression:
  | e=relational_expression { e }
  (*| ee=equality_expression EQUAL e=relational_expression { ee ^ "==" ^ e }*)
  (*| ee=equality_expression NOT_EQUAL e=relational_expression { ee ^ "!=" ^ e }*)

(* 15.22 *)
and_expression:
  | e=equality_expression { e }
  (*| ae=and_expression AND_BITWISE e=equality_expression { ae ^ "&" ^ }*)

exclusive_or_expression:
  | e=and_expression { e }
  (*| ee=exclusive_or_expression XOR_BITWISE e=and_expression { ee ^ "^" ^ e }*)

inclusive_or_expression:
  | e=exclusive_or_expression { e }
  (*| ie=inclusive_or_expression OR_BITWISE e=exclusive_or_expression { ie ^ "|" ^ e }*)

(* 15.23 *)
conditional_and_expression:
  | e=inclusive_or_expression { e }
 (* | ce=conditional_and_expression AND_LOGICAL e=inclusive_or_expression { ce ^ "&&" ^ e }*)

(* 15.24 *)
conditional_or_expression:
  | e=conditional_and_expression { e }
  (*| ce=conditional_or_expression OR_LOGICAL e=conditional_and_expression { ce ^ "||" ^ e }*)

(* 15.25 *)
%public conditional_expression:
  | e=conditional_or_expression { e }
  (*| coe=conditional_or_expression QUESTION_MARK e=expression COLON ce=conditional_expression { coe ^ "?" ^ e ^ ":" ^ ce }*)

(* 15.26 *)
assignment_expression:
  | a=conditional_expression { a }
  | a=assignment { a }

%public assignment:
  | ls=left_hand_side op=assignment_operator e=assignment_expression { ls ^ op ^ e }

left_hand_side:
  | n=expression_name { n }
  (* 
  | a=field_access { a }
  | a=array_access { a }
  *)

assignment_operator:
  | ASSIGN { "=" }
  | PLUS_ASSIGN { "+=" }
  | MINUS_ASSIGN { "-=" }
  | MULTIPLY_ASSIGN { "*=" }
  | DIVIDE_ASSIGN { "/=" }
  | MODULUS_ASSIGN { "%=" }
  | AND_BITWISE_ASSIGN { "&=" }
  | OR_BITWISE_ASSIGN { "|=" }
  | XOR_ASSIGN { "^=" }
  | LEFT_SHIFT_ASSIGN { "<<=" }
  | RIGHT_SHIFT_ASSIGN { ">>=" }
  | RIGHT_SHIFT_UNSIGNED_ASSIGN { ">>>=" }

(* 15.27 *)
%public expression:
  | e=assignment_expression { e }

(* 15.28 *)
%public constant_expression:
  | e=expression { e }
