%%

(* Section 4 Types, Values and Variables *)

(* 3.1 *)
%public identifier:
  | IDENTIFIER { "ident(" ^ $1 ^ ")" }

(* 4.1 *)
%public type_:
  | primitive_type { Tree("type_", [$1])  }
  | reference_type { Tree("type_", [$1])  }

(* 4.2 *)
primitive_type:
  | numeric_type { Tree("primitive_type", [$1])  }
  | BOOLEAN { Tree("primitive_type", [Leaf($1)])  }

numeric_type:
  | integral_type { Tree("numeric_type", [$1])  }
  | floating_point_type { Tree("numeric_type", [$1])  }

integral_type:
  | BYTE  { Tree("integral_type", [Leaf($1)])  }
  | SHORT { Tree("integral_type", [Leaf($1)])  }
  | INT   { Tree("integral_type", [Leaf($1)])  }
  | LONG  { Tree("integral_type", [Leaf($1)])  }
  | CHAR  { Tree("integral_type", [Leaf($1)])  }

floating_point_type:
  | FLOAT  { Tree("floating_point_type", [Leaf($1)])  }
  | DOUBLE { Tree("floating_point_type", [Leaf($1)])  }

(* 4.3 *)
reference_type:
  | class_or_interface_type { Tree("reference_type", [$1])  }
  (*| type_variable { "" } *)
  (*| array_type { "" } *)

class_or_interface_type:
  | class_type { Tree("class_or_interface_type", [$1])  }
  | interface_type { Tree("class_or_interface_type", [$1])  }

%public class_type:
  | type_decl_specifier type_arguments? { Treeopt("class_type", [(Some $1); $2])  }

%public interface_type:
  | type_decl_specifier type_arguments? { Treeopt("interface_type", [(Some $1); $2])  }
(*TODO not sure if type_argument is opt section 4.3*)

type_decl_specifier:
  | type_name { Tree("type_decl_specifier", [$1])  }
  (*| class_or_interface_type PERIOD identifer { "" }*)

(* type_name is also defined in 6.5 but with context distinction *)
(*
%public type_name: 
  | identifier { Tree("type_name", [Leaf($1)])  }
  | type_name PERIOD identifier { Tree("type_name", [$1; Leaf($3)])  } 
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
  | EXTENDS class_or_interface_type additional_bound_list? { Treeopt("type_bound", [Some($2); $3])  }

additional_bound_list:
  | additional_bound additional_bound_list { Tree("additional_bound_list", [$1; $2])  } (* TODO : optimize? *)
  | additional_bound { Tree("additional_bound_list", [$1])  }

additional_bound:
  | AND_BITWISE interface_type { Tree("additional_bound", [$2])  }

(* 4.5 *)
type_arguments:
  | LOWER actual_type_argument_list GREATER { Tree("type_arguments", [$2])  }

actual_type_argument_list:
  | actual_type_argument { Treeopt("actual_type_argument_list", [None; (Some $1)])  }
  | actual_type_argument_list COMMA actual_type_argument { Treeopt("actual_type_argument_list", [(Some $1); (Some $3)])  }

actual_type_argument:
  | reference_type { Tree("actual_type_argument", [$1])  }
  | wildcard { Tree("actual_type_argument", [$1])  }

wildcard:
  | QUESTION_MARK wildcard_bounds? { Treeopt("wildcard", [$2])  }

wildcard_bounds:
  | EXTENDS reference_type { Tree("wildcard_bounds", [$2])  }
  | SUPER reference_type { Tree("wildcard_bounds", [$2])  } 