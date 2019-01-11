%{
    open Ast
%}


%start compilation_unit
%type <Ast.compilationUnit> compilation_unit

%%

(* SECTION 7.3 *)
compilation_unit: (* compilationUnit *)
  | t=type_declarations? EOF { CompilationUnit_(None, None, t) }
  | i=import_declarations t=type_declarations? EOF { CompilationUnit_(None, (Some i), t) }
  | p=package_declaration i=import_declarations? t=type_declarations? EOF { CompilationUnit_((Some p), i, t) } 

import_declarations: (* importDeclaration list *)
  | is=import_declarations i=import_declaration { is @ [i] }
  | i=import_declaration { [i] }

type_declarations: (* bodyDeclaration list *)
  | t=type_declaration { [t] }
  | ts=type_declarations t=type_declaration { ts @ [t] }

(* SECTION 7.4 *)

package_declaration: (* packageDeclaration *)
  | a=annotations? PACKAGE p=name SEMICOLON { PackageDeclaration_(a, p) }

(* SECTION 7.5 *)

import_declaration: (* importDeclaration *)
  | IMPORT STATIC n=name PERIOD MULTIPLY SEMICOLON { ImportDeclaration_(true, n, true) }
  | IMPORT n=name PERIOD MULTIPLY SEMICOLON { ImportDeclaration_(false, n, true) }
  | IMPORT STATIC n=name SEMICOLON { ImportDeclaration_(true, n, false) }
  | IMPORT n=name SEMICOLON { ImportDeclaration_(false, n, false) }

(* SECTION 7.6 *)

type_declaration: (* bodyDeclaration *)
  | c=class_declaration { c }
  | id=interface_declaration { id }
(* TODO enum declaration *)
(* TODO | SEMICOLON { }*)

(* SECTION 8.1 *)
%inline class_declaration: (* bodyDeclaration *)
  | em=extended_modifiers? CLASS i=identifier tp=type_parameters? s=super? it=interfaces? cb=class_body { ClassDeclaration(em, i, Some "tp", Some "s", Some "it", cb) }

(* SECTION 8.1.1 *)
(* WARNING : Eclipse spec used -> class_modifiers replaced by extended_modifiers *)
extended_modifiers: (* expression list *)
  | em=extended_modifier { [em] }
  | em=extended_modifier ems=extended_modifiers { em::ems }

extended_modifier: (* expression *)
  | PUBLIC { Modifier("Public") }
  | PROTECTED { Modifier("Protected") }
  | PRIVATE { Modifier("Private") }
  | ABSTRACT { Modifier("Abstract") }
  | STATIC { Modifier("Static") }
  | FINAL { Modifier("Final") }
  | STRICTFP { Modifier("Strictfp") }
  | a=annotation { a } 

(* SECTION 8.1.2 *)
type_parameters: (* typeParameter list *) (* TODO *)
  | LOWER tpl=type_parameter_list GREATER { tpl }

type_parameter_list: (* typeParameter list *) (* TODO *)
  | type_parameter PERIOD type_parameter_list { Tree("type_parameter_list", [$1; $3]) } (* TODO : isn't it COMMA ? *)
  | type_parameter { Tree("type_parameter_list", [$1])  }

(* SECTION 8.1.4 *)
super:
  | EXTENDS class_or_interface_type { Tree("super", [Type($2)])  }

(* SECTION 8.1.5 *)
interfaces:
  | IMPLEMENTS interface_type_list { Tree("interfaces", [$2])  }

interface_type_list:
  | class_or_interface_type { Treeopt("interface_type_list", [None; (Some (Type($1)))])  }
  | interface_type_list COMMA class_or_interface_type { Treeopt("interface_type_list", [(Some $1); (Some (Type($3)))])  }

(* SECTION 8.1.6 *)
class_body: (* bodyDeclaration list option *)
  | L_BRACE cbd=class_body_declarations? R_BRACE { cbd }

class_body_declarations: (* bodyDeclaration list *)
  | cbd=class_body_declaration { [cbd] }
  | cbd=class_body_declaration cbds=class_body_declarations { cbd::cbds }

class_body_declaration: (* bodyDeclaration *)
  | cmd=class_member_declaration { cmd (*Tree("class_body_declaration", [$1])*)  }
  (* | instance_initializer TODO *)
  (* | static_initializer TODO *)
  (* | constructor_declaration TODO *)

class_member_declaration: (* bodyDeclaration *)
  | fd=field_declaration { fd }
  (* | method_declaration TODO *)
  | cd=class_declaration { cd }
  | id=interface_declaration { id }
  (* | SEMICOLON !!! TODO *)

(* SECTION 8.3 *)
field_declaration: (* bodyDeclaration *)
  | fm=field_modifiers? t=type_ vds=variable_declarators SEMICOLON { FieldDeclaration(fm, "t", vds) }

%public variable_declarators: (* variableDeclaration list *)
  | vd=variable_declarator { [vd] }
  | vd=variable_declarator COMMA vds=variable_declarators { vd::vds }

variable_declarator: (* variableDeclaration *)
  | vdi=variable_declarator_id { match vdi with (i, d) -> VariableDeclarationFragment(i, d, None) }
  | vdi=variable_declarator_id ASSIGN vi=variable_initializer { match vdi with (i, d) -> VariableDeclarationFragment(i, d, (Some vi)) }

variable_declarator_id: (* sting * int *)
  | i=identifier { (i, 0) }
  | vdi=variable_declarator_id L_BRACKET R_BRACKET { match vdi with (i, d) -> (i, d + 1) }

%public variable_initializer: (* expression *)
  | e=expression { e }
  | ai=array_initializer { ai }

(* SECTION 8.3.1 *)
field_modifiers: (*expression list *)
  | fm=field_modifier { [fm] }
  | fm=field_modifier fms=field_modifiers { fm::fms }

field_modifier: (* expression *)
  | PUBLIC { Modifier("Public") }
  | PROTECTED { Modifier("Protected") }
  | PRIVATE { Modifier("Private") }
  | STATIC { Modifier("Static") }
  | FINAL { Modifier("Final") }
  | TRANSIENT { Modifier("Transient") }
  | VOLATILE { Modifier("Volatile") }
  | a=annotation { a }

(* SECTION 9.1 *)
%inline interface_declaration: (* bodyDeclaration *)
  | em=extended_modifiers? INTERFACE i=identifier tp=type_parameters? ei=extends_interfaces? ib=interface_body { InterfaceDeclaration(em, i, "tp", ei, ib) }

(* SECTION 9.1.1 *)
(* WARNING : Eclipse spec used -> interface_modifiers replaced by extended_modifiers *)

(* SECTION 9.1.3 *)
extends_interfaces: (* type_ list *)
  | EXTENDS it=interface_type  { [it] }
  | eis=extends_interfaces it=interface_type { eis @ [it] }

(* SECTION 9.1.4 *)
interface_body: (* bodyDeclaration list option *)
  | L_BRACE imds=interface_member_declarations? R_BRACE { imds }

interface_member_declarations: (* bodyDeclaration list *)
  | imd=interface_member_declaration { [imd] }
  | imd=interface_member_declaration imds=interface_member_declarations { imd::imds }

interface_member_declaration: (* bodyDeclaration *)
  (* | constant_declaration {  } *)
  (* | abstract_method_declaration {  } *)
  | cd=class_declaration { cd }
  | id=interface_declaration { id }
  (* | SEMICOLON {  } *)

(* SECTION 9.7 Annotations *)
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

marker_annotation: (* expression *)
  | AT n=name { MarkerAnnotation(n) }

single_element_annotation: (* expression *)
  | AT n=name L_PAR ev=element_value R_PAR { SingleMemberAnnotation(n, ev)  }

(* SECTION 8.7 *)

/* ExplicitConstructorInvocation:
  NonWildTypeArgumentsopt this ( ArgumentListopt ) ;
  NonWildTypeArgumentsopt super ( ArgumentListopt ) ;
  Primary. NonWildTypeArgumentsopt super ( ArgumentListopt ) ; */

%public non_wild_type_arguments:
  | LOWER tl=reference_type_list GREATER { tl }

reference_type_list:
  | t=reference_type { [t] }
  | tl=reference_type_list COMMA t=reference_type { tl @ [t] }
