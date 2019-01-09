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
reference_type:
  | t=class_or_interface_type { Byte }
  (*| type_variable { "" } *)
  (*| array_type { "" } *)

class_or_interface_type:
  | t=class_type { t }
  | t=interface_type { t }

%public class_type:
  | ds=type_decl_specifier a=type_arguments? { ParameterizedType(ds, a) }

%public interface_type:
  | ds=type_decl_specifier a=type_arguments? { ParameterizedType(ds, a) }

type_decl_specifier:
  | n=name { n }
  /* | class_or_interface_type PERIOD i=identifier { ExpressionName(SimpleName(i)) } */

(* name is also defined in 6.5 but with context distinction *)
(*
%public name:
  | identifier { Tree("name", [Leaf($1)])  }
  | name PERIOD identifier { Tree("name", [$1; Leaf($3)])  }
*)

type_variable:
  | identifier { Tree("type_variable", [Leaf($1)])  }

(*
array_type:
  | type_ L_BRACKET R_BRACKET { "" }
*)

(* 4.4 *)
%public type_parameter:
  | type_variable type_bound? { Treeopt("type_parameter", [(Some $1); $2])  }

type_bound:
  | EXTENDS class_or_interface_type additional_bound_list? { Treeopt("type_bound", [Some(Type($2)); $3])  }

additional_bound_list:
  | additional_bound additional_bound_list { Tree("additional_bound_list", [$1; $2])  } (* TODO : optimize? *)
  | additional_bound { Tree("additional_bound_list", [$1])  }

additional_bound:
  | AND_BITWISE interface_type { Tree("additional_bound", [Type($2)])  }

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
