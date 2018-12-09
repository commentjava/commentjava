%start expression
%type < string > expression

%%

(* Section 15 *)
%public expression:
  | assignment_expression { $1 }

%public assignment:
  | left_hand_side assignment_operator assignment_expression { $1 ^ " " ^ $2 ^ " " ^ $3 }
  (* TODO: other assignment *)

left_hand_side:
  | expression_name { $1 }
  (* TODO: other left_hand_side *)

expression_name:
  | IDENTIFIER { $1 }
  | ambiguous_name PERIOD IDENTIFIER { $1 }

ambiguous_name:
  | IDENTIFIER { $1 }
  | ambiguous_name PERIOD IDENTIFIER { $1 }

assignment_operator:
  | ASSIGN { $1 }
  (* TODO: other assignment_operators 15.26.1 *)

assignment_expression:
  | assignment { $1 }
  | conditional_assignment { $1 }

conditional_assignment:
  | conditional_or_expression { $1 }
  (*| conditional_or_expression QUESTION_MARK expression COLON conditional_expression { $1 }*)

%public conditional_expression:
  | conditional_or_expression { $1 }
  (*| conditional_or_expression QUESTION_MARK expression COLON conditional_expression { $1 }*)

conditional_or_expression:
  | conditional_and_expression { $1 }
  (*| conditional_or_expression OR_LOGICAL conditional_and_expression { $1 }*)

conditional_and_expression:
  | inclusive_or_expression { $1 }
 (* | conditional_and_expression AND_LOGICAL inclusive_or_expression { $1 }*)

inclusive_or_expression:
  | exclusive_or_expression { $1 }
  (*| inclusive_or_expression OR_BITWISE exclusive_or_expression { $1 }*)

exclusive_or_expression:
  | and_expression { $1 }
  (*| exclusive_or_expression AND_BITWISE and_expression { $1 }*)

and_expression:
  | equality_expression { $1 }
  (*| and_expression XOR_BITWISE equality_expression { $1 }*)

equality_expression:
  | relational_expression { $1 }
  (*| equality_expression EQUAL relational_expression { $1 }*)
  (*| equality_expression NOT_EQUAL relational_expression { $1 }*)

relational_expression:
  | shift_expression { $1 }
  (* TODO: other relational_expressions 15.20 *)

shift_expression:
  | additive_expression { $1 }
  (* TODO: other shift_expression 15.29 *)

additive_expression:
  | multiplicative_expression { $1 }
  (* TODO: other shift_expression 15.18 *)

multiplicative_expression:
  | unary_expression { $1 }
  (* TODO: other multiplicative_expression 15.17 *)

unary_expression:
  (*| PLUS unary_expression { $1 }
  | MINUS unary_expression { $1 }*)
  | unary_expression_not_plus_minus { $1 }
  (* TODO: other unary_expression 15.15 *)

unary_expression_not_plus_minus:
  | postfix_expression { $1 }
  (*| MINUS unary_expression { $1 }
  | NOT_LOGICAL unary_expression { $1 }*)
  (* TODO: other unary_expression_not_plus_minus 15.15 *)

postfix_expression:
  | primary { $1 }
  | expression_name { $1 }
  (*| post_increment_expression { $1 }
  | post_decrement_expression { $1 }*)

post_increment_expression:
  | postfix_expression INCREMENT { $1 }

post_decrement_expression:
  | postfix_expression DECREMENT { $1 }

primary:
  | primary_no_new_array { $1 }
  (* TODO: other primary 15.8 *)

primary_no_new_array:
  | literal { $1 }
  | VOID PERIOD CLASS { $1 }
  | THIS { $1 }
(* TODO: other primary_no_new_array 15.8 *)

literal:
  | INTEGER_LITERAL { $1 }
  | FLOAT_LITERAL { $1 }
  | BOOLEAN_LITERAL { $1 }
  | CHAR_LITERAL { $1 }
  | STRING_LITERAL { $1 }
  | NULL_LITERAL { $1 }
