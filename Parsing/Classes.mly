%{
    open Ast
%}


%start compilation_unit
%type <Ast.compilationUnit> compilation_unit

%%

(* section 7.3 *)
compilation_unit: (* compilationUnit *)
  | t=type_declarations? EOF { CompilationUnit_(None, None, t) }
  | i=import_declarations t=type_declarations? EOF { CompilationUnit_(None, (Some i), t) }
  | p=package_declaration i=import_declarations? t=type_declarations? EOF { CompilationUnit_((Some p), i, t) } 

type_declarations: (* bodyDeclaration list *)
  | t=type_declaration { [t] }
  | ts=type_declarations t=type_declaration { ts @ [t] }

import_declarations: (* importDeclaration list *)
  | is=import_declarations i=import_declaration { is @ [i] }
  | i=import_declaration { [i] }

(* section 7.4 *)

package_declaration: (* packageDeclaration *)
  | a=annotations? PACKAGE p=name SEMICOLON { PackageDeclaration_(a, p) }

(* section 7.5 *)

import_declaration: (* importDeclaration *)
  | IMPORT STATIC n=name PERIOD MULTIPLY SEMICOLON { ImportDeclaration_(true, n, true) }
  | IMPORT n=name PERIOD MULTIPLY SEMICOLON { ImportDeclaration_(false, n, true) }
  | IMPORT STATIC n=name SEMICOLON { ImportDeclaration_(true, n, false) }
  | IMPORT n=name SEMICOLON { ImportDeclaration_(false, n, false) }

(* section 7.6 *)

type_declaration: (* bodyDeclaration *)
  | c=class_declaration { c }
(* TODO interface *)
(* TODO enum declaration *)
(* TODO | SEMICOLON { }*)

(* section 8.1 *)

class_declaration: (* bodyDeclaration *)
  | cm=class_modifiers? CLASS i=identifier tp=type_parameters? s=super? it=interfaces? cb=class_body { ClassDeclaration(cm, i, Some "tp", Some "s", Some "it", "cb") (*Treeopt("normal_class_declaration", [$1; (Some (Leaf($3))); $4; $5; $6; (Some $7)])*) }

class_modifiers: (* expression list *)
  | cm=class_modifier { [cm] }
  | cm=class_modifier cms=class_modifiers { cm::cms }

class_modifier: (* expression *)
  | PUBLIC { Modifier("Public") }
  | PROTECTED { Modifier("Protected") }
  | PRIVATE { Modifier("Private") }
  | ABSTRACT { Modifier("Abstract") }
  | STATIC { Modifier("Static") }
  | FINAL { Modifier("Final") }
  | STRICTFP { Modifier("Strictfp") }
  | a=annotation { a } 

(* section 9.7 Annotations *)
annotations: (* expression list *)
  | an=annotation { [an] }
  | an=annotation ans=annotations { an::ans }

annotation: (* expression *)
  | na=normal_annotation { na }
  | ma=marker_annotation { ma }
  | sea=single_element_annotation { sea }

normal_annotation: (* expression *)
  | AT tn=name L_PAR evpL=element_value_pairs? R_PAR { NormalAnnotation(tn, evpL) }

element_value_pairs: (* memberValuePair list *)
  | evp=element_value_pair { [evp] }
  | evp=element_value_pair COMMA evpL=element_value_pairs { evp::evpL }

element_value_pair: (* memberValuePair *)
  | i=identifier ASSIGN ev=element_value { MemberValuePair(i, ev) }

element_value: (* expression *)
  | ce=conditional_expression { ce }
  | an=annotation { an }
  | evai=element_value_array_initializer { evai }

element_value_array_initializer: (*expression *)
  | L_BRACE ev=element_values? COMMA? R_BRACE { ArrayInitializer(ev) }

element_values: (* expression list *)
  | ev=element_value { [ev] }
  | ev=element_value COMMA evs=element_values { ev::evs }

single_element_annotation: (* expression *)
  | AT n=name L_PAR ev=element_value R_PAR { SingleMemberAnnotation(n, ev)  }

marker_annotation: (* expression *)
  | AT n=name { MarkerAnnotation(n) }

type_parameters: (* typeParameter list *) (* TODO *)
  | LOWER tpl=type_parameter_list GREATER { tpl }

type_parameter_list: (* typeParameter list *) (* TODO *)
  | type_parameter PERIOD type_parameter_list { Tree("type_parameter_list", [$1; $3])  }
  | type_parameter { Tree("type_parameter_list", [$1])  }

(* SECTION 8.1.4 *)

super:
  | EXTENDS class_type { Tree("super", [Type($2)])  }

(* SECTION 8.1.5 *)

interfaces:
  | IMPLEMENTS interface_type_list { Tree("interfaces", [$2])  }

interface_type_list:
  | interface_type { Treeopt("interface_type_list", [None; (Some (Type($1)))])  }
  | interface_type_list COMMA interface_type { Treeopt("interface_type_list", [(Some $1); (Some (Type($3)))])  }

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
  | field_modifiers? type_ variable_declarators SEMICOLON { Treeopt("field_declaration", [$1; (Some (Type($2))); (Some $3)])  }

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
