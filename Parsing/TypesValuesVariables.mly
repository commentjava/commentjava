%type < string > identifier
%%

(* Section 4 Types, Values and Variables *)

(* 3.1 *)
%public identifier: (* string TODO : change $1 *)
  | IDENTIFIER { $1 }

(* 4.1 *)
%public type_: (* type_ *)
  | t=primitive_type { t }
  | t=reference_type { t }


%public array_type: (* type_ *)
  | t=primitive_type d=dims { ArrayType(t, d) }
  | t=reference_type d=dims { ArrayType(t, d) }

(* 4.2 *)
%public primitive_type: (* type_ *)
  | t=numeric_type { t }
  | BOOLEAN { Boolean }

numeric_type: (* type_ *)
  | t=integral_type { t }
  | t=floating_point_type { t }

integral_type: (* type_ *)
  | BYTE  { Byte }
  | SHORT { Short  }
  | INT   { Int }
  | LONG  { Long }
  | CHAR  { Char }

floating_point_type: (* type_ *)
  | FLOAT  { Float }
  | DOUBLE { Double }

(* 4.3 *)
%inline %public reference_type: (* type_ *)
  | t=class_or_interface_type { t }
  (* | t=type_variable { t } *)
  (* | t=array_type { t } *)

%inline %public reference_type_one_greater_more: (* type_ *)
  | t=class_or_interface_type_one_greater_more { t }
  (* | t=type_variable { t } *)
  (* | t=array_type { t } *)

%inline %public reference_type_two_greater_more: (* type_ *)
  | t=class_or_interface_type_two_greater_more { t }
  (* | t=type_variable { t } *)
  (* | t=array_type { t } *)

%inline %public class_or_interface_type: (* type_ *)
  | n=name { SimpleType(n) }
  | n=name a=type_arguments { ParameterizedType(SimpleType(n), Some a) }
  (* | t=class_or_interface_type PERIOD i=identifier a=type_arguments { ParameterizedType(QualifiedType(t, ExpressionName(SimpleName(i))), Some a) } *)
  (* | t=class_or_interface_type PERIOD i=identifier { QualifiedType(t, ExpressionName(SimpleName(i))) } *)

%inline %public class_or_interface_type_one_greater_more: (* type_ *)
  (*| n=name { SimpleType(n) }*)
  | n=name a=type_arguments_one_greater_more { ParameterizedType(SimpleType(n), Some a) }
  (* | t=class_or_interface_type PERIOD i=identifier a=type_arguments { ParameterizedType(QualifiedType(t, ExpressionName(SimpleName(i))), Some a) } *)
  (* | t=class_or_interface_type PERIOD i=identifier { QualifiedType(t, ExpressionName(SimpleName(i))) } *)

%inline %public class_or_interface_type_two_greater_more: (* type_ *)
  (*| n=name { SimpleType(n) }*)
  | n=name a=type_arguments_two_greater_more { ParameterizedType(SimpleType(n), Some a) }
  (* | t=class_or_interface_type PERIOD i=identifier a=type_arguments { ParameterizedType(QualifiedType(t, ExpressionName(SimpleName(i))), Some a) } *)
  (* | t=class_or_interface_type PERIOD i=identifier { QualifiedType(t, ExpressionName(SimpleName(i))) } *)


type_variable: (* type_ *)
  | i=identifier { SimpleType(ExpressionName(SimpleName(i))) }

/* array_type: (* type_ *)
  | t=type_ L_BRACKET R_BRACKET { ArrayType(t, 0) } */

(* 4.4 *)
%public type_parameter: (* type_parameter *)
  | tv=type_variable tb=type_bound? { TypeParameter(tv, tb) }

%public type_parameter_one_greater_more: (* type_parameter *)
  | tv=type_variable tb=type_bound_one_greater_more? { TypeParameter(tv, tb) }

type_bound: (* type_ list *)
  | EXTENDS cit=class_or_interface_type (* no additional_bound_list *) { [cit] }
  | EXTENDS cit=class_or_interface_type abl=additional_bound_list { cit::abl }

type_bound_one_greater_more: (* type_ list *)
  | EXTENDS cit=class_or_interface_type_one_greater_more (* no additional_bound_list *) { [cit] }
  | EXTENDS cit=class_or_interface_type abl=additional_bound_list_one_greater_more { cit::abl }

additional_bound_list: (* type_ list *)
  | abl=additional_bound_list ab=additional_bound { abl @ [ab] }
  | ab=additional_bound { [ab] }

additional_bound_list_one_greater_more: (* type_ list *)
  | abl=additional_bound_list ab=additional_bound_one_greater_more { abl @ [ab] }
  | ab=additional_bound_one_greater_more { [ab] }

additional_bound: (* type_ *)
  | AND_BITWISE cit=class_or_interface_type { cit }

additional_bound_one_greater_more: (* type_ *)
  | AND_BITWISE cit=class_or_interface_type_one_greater_more { cit }

(* 4.5 *)
%public type_arguments: (* type_ list *)
  | LOWER al=actual_type_argument_list GREATER { al }
  | LOWER al=actual_type_argument_list_one_greater_more { al }

%public type_arguments_one_greater_more: (* type_ list *)
  | LOWER al=actual_type_argument_list RIGHT_SHIFT { al }
  | LOWER al=actual_type_argument_list_two_greater_more { al }

%public type_arguments_two_greater_more: (* type_ list *)
  | LOWER al=actual_type_argument_list RIGHT_SHIFT_UNSIGNED { al }

actual_type_argument_list: (* type_ list *)
  | a=actual_type_argument { [a] }
  | al=actual_type_argument_list COMMA a=actual_type_argument { al @ [a] }

actual_type_argument_list_one_greater_more: (* type_ list *)
  | a=actual_type_argument_one_greater_more { [a] }
  | al=actual_type_argument_list COMMA a=actual_type_argument_one_greater_more { al @ [a] }

actual_type_argument_list_two_greater_more: (* type_ list *)
  | a=actual_type_argument_two_greater_more { [a] }
  | al=actual_type_argument_list COMMA a=actual_type_argument_two_greater_more { al @ [a] }

actual_type_argument: (* type_ *)
  | t=reference_type { t }
  | t=array_type { t }
  | w=wildcard { w }

actual_type_argument_one_greater_more: (* type_ *)
  | t=reference_type_one_greater_more { t }
  (*| t=array_type { t }*)
  | w=wildcard_one_greater_more { w }

actual_type_argument_two_greater_more: (* type_ *)
  | t=reference_type_two_greater_more { t }
  (*| t=array_type { t }*)
  | w=wildcard_two_greater_more { w }

wildcard: (* type_ *)
  | QUESTION_MARK { WildcardType(None) }
  | QUESTION_MARK b=wildcard_bounds { WildcardType(Some b) }

wildcard_one_greater_more: (* type_ *)
  (*| QUESTION_MARK { WildcardType(None) }*)
  | QUESTION_MARK b=wildcard_bounds_one_greater_more { WildcardType(Some b) }

wildcard_two_greater_more: (* type_ *)
  (*| QUESTION_MARK { WildcardType(None) }*)
  | QUESTION_MARK b=wildcard_bounds_two_greater_more { WildcardType(Some b) }

wildcard_bounds: (* type_ *)
  | EXTENDS t=reference_type { t }
  | EXTENDS t=array_type { t }
  | SUPER t=reference_type { t }
  | SUPER t=array_type { t }

wildcard_bounds_one_greater_more: (* type_ *)
  | EXTENDS t=reference_type_one_greater_more { t }
  (*| EXTENDS t=array_type { t }*)
  | SUPER t=reference_type_one_greater_more { t }
  (*| SUPER t=array_type { t }*)

wildcard_bounds_two_greater_more: (* type_ *)
  | EXTENDS t=reference_type_two_greater_more { t }
  (*| EXTENDS t=array_type { t }*)
  | SUPER t=reference_type_two_greater_more { t }
  (*| SUPER t=array_type { t }*)
