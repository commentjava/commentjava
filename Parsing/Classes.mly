%{
    open Ast
%}


%start class_declaration
%type <Ast.ast> class_declaration

%%

class_declaration :
  | normal_class_declaration { Tree("class_declaration", [$1])  }
 (*TODO: section 8.1 *)

normal_class_declaration:
  | class_modifiers? CLASS IDENTIFIER type_parameters? super? interfaces? class_body { Treeopt("normal_class_declaration", [$1; (Some (Leaf($3))); $4; $5; $6; (Some $7)]) }

class_modifiers:
  | class_modifier { Tree("class_modifiers", [$1]) }
  | class_modifiers class_modifier { Tree("class_modifiers", [$1; $2]) }

class_modifier:
  | PUBLIC { Tree("class_modifier", [Leaf($1)]) }
  | PROTECTED { Tree("class_modifier", [Leaf($1)]) }
  | PRIVATE { Tree("class_modifier", [Leaf($1)]) }
  | ABSTRACT { Tree("class_modifier", [Leaf($1)]) }
  | STATIC { Tree("class_modifier", [Leaf($1)]) }
  | FINAL { Tree("class_modifier", [Leaf($1)]) }
  | STRICTFP { Tree("class_modifier", [Leaf($1)]) }
  | annotation { Tree("class_modifier", [$1]) } (* TODO : isn't there a problem here? annotation -> annotations *)

(* section 9.7 Annotations *)
annotations:
  | annotations? annotation { Treeopt("annotations", [$1; (Some $2)])  }

annotation:
  | normal_annotation { Tree("annotation", [$1])  }
  | marker_annotation { Tree("annotation", [$1])  }
  | single_element_annotation { Tree("annotation", [$1])  }

normal_annotation:
  | AT type_name L_PAR element_value_pairs? R_PAR { Treeopt("normal_annotation", [(Some $2); $4])  }

element_value_pairs:
  | element_value_pair { Treeopt("element_value_pairs", [None; (Some $1)])  }
  | element_value_pairs COMMA element_value_pair { Treeopt("element_value_pairs", [(Some $1); (Some $3)])  }

element_value_pair:
  | IDENTIFIER ASSIGN element_value { Tree("element_value_pair", [Leaf($1); $3])  }

element_value:
  | conditional_expression { Tree("element_value", [Leaf($1)])  }
  | annotation { Tree("element_value", [$1])  }
  | element_value_array_initializer { Tree("element_value", [$1])  } 

element_value_array_initializer:
  | L_BRACE element_values? COMMA? { Treeopt("element_value_array_initializer", [$2])  } (* TODO : are we sure of this one? *)

element_values:
  | element_value { Treeopt("element_values", [None; (Some $1)])  }
  | element_values COMMA element_value { Treeopt("element_values", [(Some $1); (Some $3)])  }

single_element_annotation:
  | AT type_name L_PAR element_value R_PAR { Tree("single_element_annotation", [$2; $4])  }

marker_annotation:
  | AT type_name { Tree("marker_annotation", [$2])  }

type_parameters:
  | LOWER type_parameter_list GREATER { Tree("type_parameters", [$2]) }

type_parameter_list:
  | type_parameter_list PERIOD type_parameter { Tree("type_parameter_list", [$1; $3])  }
  | type_parameter { Tree("type_parameter_list", [$1])  }

type_parameter:
  | type_variable type_bound? { Treeopt("type_parameter", [(Some $1); $2])  }

type_bound:
  | EXTENDS class_or_interface_type additional_bound_list? { Treeopt("type_bound", [Some($2); $3])  }

additional_bound_list:
  | additional_bound additional_bound_list { Tree("additional_bound_list", [$1; $2])  } (* TODO : optimize? *)
  | additional_bound { Tree("additional_bound_list", [$1])  }

additional_bound:
  | AND_BITWISE interface_type { Tree("additional_bound", [$2])  }


type_name:
  | IDENTIFIER { Tree("type_name", [Leaf($1)])  }
  | type_name PERIOD IDENTIFIER { Tree("type_name", [$1; Leaf($3)])  } 

(* SECTION 4.5.1 *)
type_argument:
  | LOWER actual_type_argument_list GREATER { Tree("type_argument", [$2])  }

actual_type_argument_list:
  | actual_type_argument { Treeopt("actual_type_argument_list", [None; (Some $1)])  }
  | actual_type_argument_list COMMA actual_type_argument { Treeopt("actual_type_argument_list", [(Some $1); (Some $3)])  }

actual_type_argument:
  | reference_type { Tree("actual_type_argument", [$1])  }
  | wildcard { Tree("actual_type_argument", [$1])  }

wildcard:
  | TERNARY_THEN wildcard_bounds? { Treeopt("wildcard", [$2])  }

wildcard_bounds:
  | EXTENDS reference_type { Tree("wildcard_bounds", [$2])  }
  | SUPER reference_type { Tree("wildcard_bounds", [$2])  } 

(* SECTION 4.3 *)

reference_type:
  | class_or_interface_type { Tree("reference_type", [$1])  }
(* TODO section 4.3 *)

class_or_interface_type:
  | class_type { Tree("class_or_interface_type", [$1])  }
  | interface_type { Tree("class_or_interface_type", [$1])  }

class_type:
  | type_decl_specifier type_argument? { Treeopt("class_type", [(Some $1); $2])  }

interface_type:
  | type_decl_specifier type_argument? { Treeopt("interface_type", [(Some $1); $2])  }
(*TODO not sure if type_argument is opt section 4.3*)

type_decl_specifier:
  | type_name { Tree("type_decl_specifier", [$1])  }
 (*TODO : section 4.3*)

type_variable:
  | IDENTIFIER { Tree("type_variable", [Leaf($1)])  }

(* SECTION 8.1.4 *)

super:
  | EXTENDS class_type { Tree("super", [$2])  }

(* SECTION 8.1.5 *)

interfaces:
  | IMPLEMENTS interface_type_list { Tree("interfaces", [$2])  }

interface_type_list:
  | interface_type { Treeopt("interface_type_list", [None; (Some $1)])  }
  | interface_type_list COMMA interface_type { Treeopt("interface_type_list", [(Some $1); (Some $3)])  }

(* SECTION 8.1.6 *)

class_body:
  | L_BRACE class_body_declarations? R_BRACE { Treeopt("class_body", [$2])  }

class_body_declarations:
  | class_body_declaration { Tree("class_body_declarations", [$1])  }
  | class_body_declarations class_body_declaration { Tree("class_body_declarations", [$1; $2])  }

class_body_declaration:
  | class_member_declaration { Tree("class_body_declaration", [$1])  }
(* TODO : section 8.1.6 *)

class_member_declaration:
  | field_declaration { Tree("class_member_declaration", [$1])  }
(* TODO : section 8.1.6 *)

(* SECTION 8.3*)
field_declaration:
  | field_modifiers? type_ variable_declarators SEMICOLON { Treeopt("field_declaration", [$1; (Some $2); (Some $3)])  }

field_modifiers:
  | field_modifier { Treeopt("field_modifiers", [None; (Some $1)])  } (* TODO : good optimization? *)
  | field_modifiers field_modifier { Treeopt("field_modifiers", [(Some $1); (Some $2)])  }

field_modifier:
  | PUBLIC { Tree("field_modifier", [Leaf($1)])  }
  | PROTECTED { Tree("field_modifier", [Leaf($1)])  }
  | PRIVATE { Tree("field_modifier", [Leaf($1)])  }
  | STATIC { Tree("field_modifier", [Leaf($1)])  }
  | FINAL { Tree("field_modifier", [Leaf($1)])  }
  | TRANSIENT { Tree("field_modifier", [Leaf($1)])  }
  | VOLATILE { Tree("field_modifier", [Leaf($1)])  }
(*TODO : section 8.3.1*)

variable_declarators:
  | variable_declarator { Tree("variable_declarators", [$1])  }
  | variable_declarators COMMA variable_declarator { Tree("variable_declarators", [$1; $3])  }

variable_declarator:
  | variable_declarator_id { Tree("variable_declarator", [$1])  }
  | variable_declarator_id ASSIGN variable_initializer { Tree("variable_declarator", [$1; $3])  }

variable_declarator_id:
  | IDENTIFIER { Tree("variable_declarator_id", [Leaf($1)])  }
(*TODO: section 8.3 *)

variable_initializer:
  | expression { Tree("variable_initializer", [Leaf($1)])  }
(*TODO: section 8.3 *)

(*SECTION 4.1 *)
type_:
  | primitive_type { Tree("type_", [$1])  }
  | reference_type { Tree("type_", [$1])  }

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

