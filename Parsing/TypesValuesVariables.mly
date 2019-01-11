%type < string > identifier
%%

(* Section 4 Types, Values and Variables *)

(* 3.1 *)
%public identifier:
  | IDENTIFIER { $1 }

(* 4.1 *)
%public type_:
  | t=primitive_type { t }
  | t=reference_type { t }

(* 4.2 *)
primitive_type:
  | t=numeric_type { t }
  | BOOLEAN { Boolean }

numeric_type:
  | t=integral_type { t }
  | t=floating_point_type { t }

integral_type:
  | BYTE  { Byte }
  | SHORT { Short  }
  | INT   { Int }
  | LONG  { Long }
  | CHAR  { Char }

floating_point_type:
  | FLOAT  { Float }
  | DOUBLE { Double }

(* 4.3 *)
%public reference_type:
  | t=class_or_interface_type { t }
  /* | t=type_variable { t } */
  | t=array_type { t }

%public class_or_interface_type:
  | n=name { SimpleType(n) }
  | n=name a=type_arguments { ParameterizedType(SimpleType(n), Some a) }
  /* | t=class_or_interface_type PERIOD i=identifier a=type_arguments { ParameterizedType(QualifiedType(t, ExpressionName(SimpleName(i))), Some a) } */
  /* | t=class_or_interface_type PERIOD i=identifier { QualifiedType(t, ExpressionName(SimpleName(i))) } */


type_variable:
  | i=identifier { SimpleType(ExpressionName(SimpleName(i))) }

array_type:
  | t=type_ L_BRACKET R_BRACKET { ArrayType(t) }

(* 4.4 *)
%public type_parameter:
  | type_variable type_bound? { Treeopt("type_parameter", [(Some (Type($1))); $2])  }

type_bound:
  | EXTENDS class_or_interface_type additional_bound_list? { Treeopt("type_bound", [Some(Type($2)); $3])  }

additional_bound_list:
  | additional_bound additional_bound_list { Tree("additional_bound_list", [$1; $2])  } (* TODO : optimize? *)
  | additional_bound { Tree("additional_bound_list", [$1])  }

additional_bound:
  | AND_BITWISE class_or_interface_type { Tree("additional_bound", [Type($2)])  }

(* 4.5 *)
type_arguments:
  | LOWER al=actual_type_argument_list GREATER { al }

actual_type_argument_list:
  | a=actual_type_argument { [a] }
  | al=actual_type_argument_list COMMA a=actual_type_argument { al @ [a] }

actual_type_argument:
  | t=reference_type { t }
  | w=wildcard { w }

wildcard:
  | QUESTION_MARK { WildcardType(None) }
  | QUESTION_MARK b=wildcard_bounds { WildcardType(Some b) }

wildcard_bounds:
  | EXTENDS t=reference_type { t }
  | SUPER t=reference_type { t }
