%{
    open Ast
%}


%start compilation_unit
%type <Ast.ast> compilation_unit

%%

(* section 7.3 *)
compilation_unit:
  | p=package_declaration? i=import_declarations? t=type_declarations? EOF { CompilationUnit(CompilationUnit_(None, None, t)) }

type_declarations:
  | t=type_declaration { [t] }
  | ts=type_declarations t=type_declaration { ts @ [t] }

import_declarations:
  | is=import_declarations i=import_declaration { is @ [i] }
  | i=import_declaration { [i] }

(* section 7.4 *)

package_declaration:
  | a=annotations? PACKAGE p=package_name SEMICOLON { PackageDeclaration_("a", "p") }

(* section 7.5 *)

import_declaration:
  | IMPORT STATIC n=package_or_type_name PERIOD MULTIPLY SEMICOLON { ImportDeclaration_(true, "n", true) }
  | IMPORT n=package_or_type_name PERIOD MULTIPLY SEMICOLON { ImportDeclaration_(false, "n", true) }
  | IMPORT STATIC n=package_or_type_name SEMICOLON { ImportDeclaration_(true, "n", false) }
  | IMPORT n=package_or_type_name SEMICOLON { ImportDeclaration_(false, "n", false) }

(* section 7.6 *)

type_declaration:
  | c=class_declaration { c }
(* TODO interface *)
(* TODO enum declaration *)
(* TODO | SEMICOLON { }*)

(* section 8.1 *)

class_declaration:
  | cm=class_modifiers? CLASS i=identifier tp=type_parameters? s=super? it=interfaces? cb=class_body { ClassDeclaration(Some "cm", i, Some "tp", Some "s", Some "it", "cb") (*Treeopt("normal_class_declaration", [$1; (Some (Leaf($3))); $4; $5; $6; (Some $7)])*) }

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
  | annotation { Treeopt("annotations", [(Some $1); None])  }
  | annotations annotation { Treeopt("annotations", [(Some $1); (Some $2)])  }

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
  | identifier ASSIGN element_value { Tree("element_value_pair", [Leaf($1); $3])  }

element_value:
  | conditional_expression { Tree("element_value", [Expression($1)])  }
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

%public variable_declarators:
  | variable_declarator { Tree("variable_declarators", [$1])  }
  | variable_declarators COMMA variable_declarator { Tree("variable_declarators", [$1; $3])  }

variable_declarator:
  | variable_declarator_id { Tree("variable_declarator", [$1])  }
  | variable_declarator_id ASSIGN variable_initializer { Tree("variable_declarator", [$1; Expression($3)])  }

variable_declarator_id:
  | identifier { Tree("variable_declarator_id", [Leaf($1)])  }
(*TODO: section 8.3 *)

%public variable_initializer:
  | e=expression { e }
  | ai=array_initializer { ai }

(*TODO: section 8.3 *)
