%start class_declaration
%type <Ast.ast> class_declaration

%%

class_declaration :
  | normal_class_declaration { Tree("normal_class_declaration", [Leaf($1)])  }
 (*TODO: section 8.1 *)

normal_class_declaration:
  | class_modifiers? CLASS IDENTIFIER type_parameters? super? interfaces? class_body { $3 }

class_modifiers:
  | class_modifier { $1 }
  | class_modifiers class_modifier { $1 }

class_modifier:
  | PUBLIC { $1 }
  | PROTECTED { $1 }
  | PRIVATE { $1 }
  | ABSTRACT { $1 }
  | STATIC { $1 }
  | FINAL { $1 }
  | STRICTFP { $1 }
  | annotation { $1 }

(* section 9.7 Annotations *)
annotations:
  | annotation { $1 }
  | annotations annotation { $1 }

annotation:
  | normal_annotation { $1 }
  | marker_annotation { $1 }
  | single_element_annotation { $1 }

normal_annotation:
  | AT type_name L_PAR element_value_pairs? R_PAR { $1 }

element_value_pairs:
  | element_value_pair { $1 }
  | element_value_pairs COMMA element_value_pair { $1 }

element_value_pair:
  | IDENTIFIER ASSIGN element_value { $1 }

element_value:
  | conditional_expression { $1 }
  | annotation { $1 }
  | element_value_array_initializer { $1 } 

element_value_array_initializer:
  | L_BRACE element_values? COMMA? { $1 }

element_values:
  | element_value { $1 }
  | element_values COMMA element_value { $1 }

single_element_annotation:
  | AT type_name L_PAR element_value R_PAR { $1 }

marker_annotation:
  | AT type_name { $1 }

type_parameters:
  | LOWER type_parameter_list GREATER { $1 }

type_parameter_list:
  | type_parameter_list PERIOD type_parameter { $1 }
  | type_parameter { $1 }

type_parameter:
  | type_variable type_bound? { $1 }

type_bound:
  | EXTENDS class_or_interface_type additional_bound_list? { $1 }

additional_bound_list:
  | additional_bound additional_bound_list { $1 }
  | additional_bound { $1 }

additional_bound:
  | AND_BITWISE interface_type { $1 }


type_name:
  | IDENTIFIER { $1 }
  | type_name PERIOD IDENTIFIER { $1 } 

(* SECTION 4.5.1 *)
type_argument:
  | LOWER actual_type_argument_list GREATER { $1 }

actual_type_argument_list:
  | actual_type_argument { $1 }
  | actual_type_argument_list COMMA actual_type_argument { $1 }

actual_type_argument:
  | reference_type { $1 }
  | wildcard { $1 }

wildcard:
  | TERNARY_THEN wildcard_bounds? { $1 }

wildcard_bounds:
  | EXTENDS reference_type { $1 }
  | SUPER reference_type { $1 } 

(* SECTION 4.3 *)

reference_type:
  | class_or_interface_type { $1 }
(* TODO section 4.3 *)

class_or_interface_type:
  | class_type { $1 }
  | interface_type { $1 }

class_type:
  | type_decl_specifier type_argument? { $1 }

interface_type:
  | type_decl_specifier type_argument? { $1 }
(*TODO not sure if type_argument is opt section 4.3*)

type_decl_specifier:
  | type_name { $1 }
 (*TODO : section 4.3*)

type_variable:
  | IDENTIFIER { $1 }

(* SECTION 8.1.4 *)

super:
  | EXTENDS class_type { $1 }

(* SECTION 8.1.5 *)

interfaces:
  | IMPLEMENTS interface_type_list { $1 }

interface_type_list:
  | interface_type { $1 }
  | interface_type_list COMMA interface_type { $1 }

(* SECTION 8.1.6 *)

class_body:
  | L_BRACE class_body_declarations? R_BRACE { $1 }

class_body_declarations:
  | class_body_declaration { $1 }
  | class_body_declarations class_body_declaration { $1 }

class_body_declaration:
  | class_member_declaration { $1 }
(* TODO : section 8.1.6 *)

class_member_declaration:
  | field_declaration { $1 }
(* TODO : section 8.1.6 *)

(* SECTION 8.3*)
field_declaration:
  | field_modifiers? type_ variable_declarators SEMICOLON { $1 }

field_modifiers:
  | field_modifier { $1 }
  | field_modifiers field_modifier { $1 }

field_modifier:
  | PUBLIC { $1 }
  | PROTECTED { $1 }
  | PRIVATE { $1 }
  | STATIC { $1 }
  | FINAL { $1 }
  | TRANSIENT { $1 }
  | VOLATILE { $1 }
(*TODO : section 8.3.1*)

variable_declarators:
  | variable_declarator { $1 }
  | variable_declarators COMMA variable_declarator { $1 }

variable_declarator:
  | variable_declarator_id { $1 }
  | variable_declarator_id ASSIGN variable_initializer { $1 }

variable_declarator_id:
  | IDENTIFIER { $1 }
(*TODO: section 8.3 *)

variable_initializer:
  | expression { $1 }
(*TODO: section 8.3 *)

(*SECTION 4.1 *)
type_:
  | primitive_type { $1 }
  | reference_type { $1 }

primitive_type:
  | numeric_type { $1 }
  | BOOLEAN { $1 }

numeric_type:
  | integral_type { $1 }
  | floating_point_type { $1 }

integral_type:
  | BYTE  { $1 }
  | SHORT { $1 }
  | INT { $1 }
  | LONG { $1 }
  | CHAR { $1 }

floating_point_type:
  | FLOAT { $1 }
  | DOUBLE { $1 }


