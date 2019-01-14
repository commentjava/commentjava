%{
    open Ast
    let create_error_message msg (position : Lexing.position) =
      position.pos_fname ^ ": " ^ msg ^ "\n"
      ^ "line " ^ (string_of_int position.pos_lnum)
      ^ " column " ^ (string_of_int (position.pos_cnum - position.pos_bol))
    let error msg position =
      raise (Failure (create_error_message msg position))
%}


%start compilation_unit
%type <Ast.compilationUnit> compilation_unit

%%

(* SECTION 7.3 *)
compilation_unit: (* compilationUnit *)
  | t=type_declarations? EOF { CompilationUnit(None, None, t) }
  | i=import_declarations t=type_declarations? EOF { CompilationUnit(None, (Some i), t) }
  | p=package_declaration i=import_declarations? t=type_declarations? EOF { CompilationUnit(Some p, i, t) }

import_declarations: (* importDeclaration list *)
  | is=nonempty_list(import_declaration) { is }

type_declarations: (* bodyDeclaration list *)
  | ts=nonempty_list(type_declaration) { ts }

(* SECTION 7.4 *)
package_declaration: (* packageDeclaration *)
  | a=extended_modifiers PACKAGE p=name SEMICOLON {
    match check_modifiers check_package_modifier a with
    | true -> PackageDeclaration(a, p)
    | false -> error ("Invalid annotation for package declaration") $startpos
  }

(* SECTION 7.5 *)
import_declaration: (* importDeclaration *)
  | IMPORT s=boption(STATIC) n=name PERIOD MULTIPLY SEMICOLON { ImportDeclaration_(s, n, true) }
  | IMPORT s=boption(STATIC) n=name SEMICOLON { ImportDeclaration_(s, n, false) }

(* SECTION 7.6 *)
type_declaration: (* bodyDeclaration *)
  | c=class_declaration { c }
  | id=interface_declaration { id }
  | e=enum_declaration { e } (* section 8.1 *)
  | SEMICOLON { EmptyBodyDeclaration }

(* SECTION 8.1 *)
%public class_declaration: (* bodyDeclaration *)
  | em=extended_modifiers CLASS i=identifier tp=type_parameters? s=super? it=interfaces? cb=class_body {
          match check_modifiers check_class_modifier em with
          | true -> ClassDeclaration(em, i, tp, s, it, cb)
          | false -> error ("Invalid modifier for class " ^ i) $startpos
}

(* SECTION 8.1.1 *)
(* XXX: Eclipse spec used -> class_modifiers replaced by extended_modifiers
  * Extended modifiers used to parse:
  * - class_modifiers
  * - constant_modifiers
  * - enum_modifiers
  * - interface_modifiers
  *)
extended_modifiers: (* expression list *)
  | ems=list(extended_modifier) { ems }

extended_modifier: (* expression *)
  | PUBLIC { Modifier(PUBLIC) }
  | PROTECTED { Modifier(PROTECTED) }
  | PRIVATE { Modifier(PRIVATE) }
  | ABSTRACT { Modifier(ABSTRACT) }
  | STATIC { Modifier(STATIC) }
  | FINAL { Modifier(FINAL) }
  | STRICTFP { Modifier(STRICTFP) }
  | TRANSIENT { Modifier(TRANSIENT) }
  | VOLATILE { Modifier(VOLATILE) }
  | SYNCHRONIZED  { Modifier(SYNCHRONIZED) }
  | NATIVE  { Modifier(NATIVE) }
  | a=annotation { a }

(* SECTION 8.1.2 *)
type_parameters: (* typeParameter list *)
  | LOWER tpl=type_parameter_list GREATER { tpl }
  | LOWER tpl=type_parameter_list_one_greater_more { tpl }

type_parameter_list: (* typeParameter list *)
  | tpl=type_parameter_list COMMA tp=type_parameter { tpl @ [tp] }
  | tp=type_parameter { [tp] }

type_parameter_list_one_greater_more: (* typeParameter list *)
  | tpl=type_parameter_list COMMA tp=type_parameter_one_greater_more { tpl @ [tp] }
  | tp=type_parameter_one_greater_more { [tp] }

(* SECTION 8.1.4 *)
super: (* type_ *)
  | EXTENDS cit=class_or_interface_type { cit }

(* SECTION 8.1.5 *)
interfaces: (* type_ list *)
  | IMPLEMENTS itl=interface_type_list { itl }

interface_type_list: (* type_ list *)
  | cit=class_or_interface_type { [cit] }
  | itl=interface_type_list COMMA cit=class_or_interface_type { itl @ [cit] }

(* SECTION 8.1.6 *)
%public class_body: (* bodyDeclaration list option *)
  | L_BRACE cbd=class_body_declarations? R_BRACE { cbd }

class_body_declarations: (* bodyDeclaration list *)
  | cbds=nonempty_list(class_body_declaration) { cbds }

class_body_declaration: (* bodyDeclaration *)
  | cd=constructor_declaration { cd }
  | cmd=class_member_declaration { cmd }
  | ii=instance_initializer { InstanceInitializer(ii) }
  | STATIC ii=instance_initializer { StaticInstanceInitializer(ii) }

class_member_declaration: (* bodyDeclaration *)
  | fd=field_declaration { fd }
  | md=method_declaration { md }
  | cd=class_declaration { cd }
  | id=interface_declaration { id }
  | SEMICOLON { EmptyBodyDeclaration }

(* SECTION 8.3 *)
field_declaration: (* bodyDeclaration *)
  | fm=extended_modifiers t=type_ vds=variable_declarators SEMICOLON { FieldDeclaration(fm, t, vds) }
  | fm=extended_modifiers t=array_type vds=variable_declarators SEMICOLON { FieldDeclaration(fm, t, vds) }

%public variable_declarators: (* variableDeclaration list *)
  | vd=variable_declarator { [vd] }
  | vd=variable_declarator COMMA vds=variable_declarators { vd::vds }

%inline variable_declarator: (* variableDeclaration *)
  | vdi=variable_declarator_id { match vdi with (i, d) -> VariableDeclarationFragment(i, d, None) }
  | vdi=variable_declarator_id ASSIGN vi=variable_initializer { match vdi with (i, d) -> VariableDeclarationFragment(i, d, Some vi) }

%inline variable_declarator_id: (* string * int *)
  | i=identifier { (i, 0) }
  | i=identifier d=dims { (i, d) }

%public variable_initializer: (* expression *)
  | e=expression { e }
  | ai=array_initializer { ai }

(* SECTION 8.3.1 *)

(* XXX: Fields modifiers unused, we use extended_modifiers instead
 * and to a final check when building the tree  with check_*_modifier *)

(* SECTION 8.4 *)
method_declaration: (* bodyDeclaration *)
  | em=extended_modifiers rt=result_type i=identifier L_PAR fpl=ioption(formal_parameters) R_PAR t=throws? mb=method_body {
          match check_modifiers check_method_modifier em with
          | true -> MethodDeclaration(em, None, rt, i, fpl, t, Some mb)
          | false -> error ("Invalid modifier for method " ^ i) $startpos
  }
  | em=extended_modifiers tp=type_parameters rt=result_type i=identifier L_PAR fpl=ioption(formal_parameters) R_PAR t=throws? mb=method_body {
          match check_modifiers check_method_modifier em with
          | true -> MethodDeclaration(em, Some tp, rt, i, fpl, t, Some mb)
          | false -> error ("Invalid modifier for method " ^ i) $startpos
  }
  (* XXX not: `anonymous()` considered as a deprecated synonym of `endrule()`
   * But because we want to keep compatibility with older versions of Menhir
   * (e.g. the ones packaged with Debian and Ubuntu) we kept this syntax.
   * You might want change that in the future.
   *)
  | amd=anonymous(abstract_method_declaration) { amd }

%inline result_type: (* type_ *)
  | t=type_ { t }
  | t=array_type { t }
  | VOID { Void }

(* SECTION 8.4.1 *)
formal_parameters: (* variableDeclaration list *)
  | fps=separated_list(COMMA, formal_parameter) {
    match check_formal_parameters fps with
    | true -> fps
    | false -> error ("Invalid ... in parameter list") $startpos
  }

%public %inline formal_parameter: (* variableDeclaration *)
  | vm=variable_modifiers? t=type_ e=boption(ELLIPSIS) vd=variable_declarator_id {
    match vd with
    | i, n -> SingleVariableDeclaration(vm, t, None, e, i, n, None) }
  | vm=variable_modifiers? t=array_type e=boption(ELLIPSIS) vd=variable_declarator_id {
    match vd with
    | i, n -> SingleVariableDeclaration(vm, t, None, e, i, n, None)
  }

%public variable_modifiers: (* expression list *)
  | m=nonempty_list(variable_modifier) { m }

variable_modifier: (* expression *)
  | FINAL { Modifier(FINAL) }
  | a=annotation { a }

(* SECTION 8.4.3 *)
(* XXX: method_modifiers replaced by extended_modifiers *)

(* SECTION 8.4.6 *)
throws: (* type_ list *)
  | THROWS tl=exception_type_list { tl }

exception_type_list: (* type_ list *)
  | et=class_or_interface_type { [et] }
  | et=class_or_interface_type COMMA ets=exception_type_list { et::ets }

(* SECTION 8.4.7 *)
method_body: (* statement *)
   | b=block { b }
(*   | SEMICOLON { EmptyStatement } *)

(* SECTION 8.6 *)
%inline instance_initializer: (* statement *)
  | b=block { b }

(* SECTION 8.7 *)
(* XXX : static_initializer included in class_body_declaration *)

(* SECTION 8.8 *)
constructor_declaration: (* bodyDeclaration *)
  | cm=extended_modifiers id=identifier cd=constructor_declarator th=throws? cb=constructor_body {
          match check_modifiers check_contructor_modifer cm with
          | true -> ConstructorDeclaration(cm, None, id, cd, th, cb)
          | false -> error ("Invalid modifier for constructor " ^ id) $startpos
  }
  | cm=extended_modifiers tp=type_parameters id=identifier cd=constructor_declarator th=throws? cb=constructor_body {
          match check_modifiers check_contructor_modifer cm with
          | true -> ConstructorDeclaration(cm, Some tp, id, cd, th, cb)
          | false -> error ("Invalid modifier for constructor " ^ id) $startpos
  }

constructor_declarator: (* contructorDeclarator *)
  | fp=delimited(L_PAR, formal_parameters, R_PAR) { fp }

(* SECTION 8.8.7 *)
constructor_body: (* bodyDeclaration *)
  | L_BRACE (* no explicit_constructor_invocation *) bs=block_statements? R_BRACE { ConstructorBody(None, bs) }
  | L_BRACE eci=explicit_constructor_invocation bs=block_statements? R_BRACE { ConstructorBody((Some eci), bs) }

(* SECTION 8.8.7.1 *)
explicit_constructor_invocation: (* statement *)
  | (* no non_wild_type_arguments *) THIS L_PAR arg=argument_list? R_PAR SEMICOLON { ConstructorInvocation(None, arg) }
  | (* no non_wild_type_arguments *) SUPER L_PAR arg=argument_list? R_PAR SEMICOLON { SuperConstructorInvocation(None, None, arg) }
  | nwta=non_wild_type_arguments THIS L_PAR arg=argument_list? R_PAR SEMICOLON { ConstructorInvocation((Some nwta), arg) }
  | nwta=non_wild_type_arguments SUPER L_PAR arg=argument_list? R_PAR SEMICOLON { SuperConstructorInvocation(None, (Some nwta), arg) }
  | p=primary PERIOD (* no non_wild_type_arguments *) SUPER L_PAR arg=argument_list? R_PAR SEMICOLON (* p : expression *) { SuperConstructorInvocation((Some p), None, arg) }
  | p=primary PERIOD nwta=non_wild_type_arguments SUPER L_PAR arg=argument_list? R_PAR SEMICOLON (* p : expression *) { SuperConstructorInvocation((Some p), (Some nwta), arg) }

%public %inline non_wild_type_arguments: (* type_ list *)
  | LOWER tl=reference_type_list GREATER { tl }
  | LOWER tl=reference_type_list_one_greater_more { tl }

reference_type_list: (* type_ list *)
  | t=reference_type { [t] }
  | t=array_type { [t] }
  | tl=reference_type_list COMMA t=reference_type { tl @ [t] }
  | tl=reference_type_list COMMA t=array_type { tl @ [t] }

reference_type_list_one_greater_more: (* type_ list *)
  | t=reference_type_one_greater_more { [t] }
  (*| t=array_type { [t] }*)
  | tl=reference_type_list COMMA t=reference_type_one_greater_more { tl @ [t] }
  (*| tl=reference_type_list COMMA t=array_type { tl @ [t] }*)

(* SECTION 8.9 *)
enum_declaration: (* bodyDeclaration *)
  | em=extended_modifiers ENUM i=identifier it=interfaces? eb=enum_body {
          match eb with
          | EnumBody(ec, cb) ->
                match check_modifiers check_class_modifier em with
                | true -> EnumDeclaration(em, i, it, ec, cb)
          | false -> error ("Invalid modifier for enum " ^ i) $startpos
  }

enum_body: (* bodyDeclaration *)
  | L_BRACE ec=enum_constants? R_BRACE { EnumBody(ec, None) }
  | L_BRACE ec=enum_constants? SEMICOLON cb=class_body_declarations? R_BRACE { EnumBody(ec, cb) }

enum_constants: (* bodyDeclaration list *)
  | c=enum_constant { [c] }
  | c=enum_constant COMMA cs=enum_constants { c::cs }
  | c=enum_constant COMMA { [c] }

enum_constant: (* bodyDeclaration *)
  | a=annotations i=identifier args=delimited(L_PAR, argument_list, R_PAR)? cb=class_body? {
          match cb with
          | Some c -> EnumConstantDeclaration(a, i, args, c)
          | None -> EnumConstantDeclaration(a, i, args, None) }

(* SECTION 9.1 *)
interface_declaration: (* bodyDeclaration *)
  | em=extended_modifiers INTERFACE i=identifier tp=type_parameters? ei=extends_interfaces? ib=interface_body {
          match check_modifiers check_interface_modifier em with
          | true -> InterfaceDeclaration(em, i, tp, ei, ib)
          | false -> error ("Invalid modifier for interface " ^ i) $startpos
  }

(* SECTION 9.1.1 *)
(* XXX: Eclipse spec used -> interface_modifiers replaced by extended_modifiers *)

(* SECTION 9.1.3 *)
extends_interfaces: (* type_ list *)
  | EXTENDS it=class_or_interface_type { [it] }
  | eis=extends_interfaces COMMA it=class_or_interface_type { eis @ [it] }

(* SECTION 9.1.4 *)
interface_body: (* bodyDeclaration list option *)
  | L_BRACE imds=nonempty_list(interface_member_declaration)? R_BRACE { imds }

interface_member_declaration: (* bodyDeclaration *)
  | amd=abstract_method_declaration { amd }
  | cd=constant_declaration { cd }
  | cd=class_declaration { cd }
  | id=interface_declaration { id }
  | atd = annotation_type_declaration { atd }
  | SEMICOLON { EmptyBodyDeclaration }

(* SECTION 9.3 *)
constant_declaration: (* bodyDeclaration *)
  | imms=extended_modifiers t=type_ vds=variable_declarators SEMICOLON {
    match check_modifiers check_constant_modifier imms with
    | true -> FieldDeclaration(imms, t, vds)
    | false -> error ("Invalid modifier for constant ") $startpos
  }
  | imms=extended_modifiers t=array_type vds=variable_declarators SEMICOLON {
    match check_modifiers check_constant_modifier imms with
    | true -> FieldDeclaration(imms, t, vds)
    | false -> error ("Invalid modifier for constant ") $startpos
  }

(* XXX: constant_modifiers replaced by extended_modifiers *)

(* SECTION 9.4 *)
abstract_method_declaration: (* bodyDeclaration *)
  | imms=extended_modifiers (* no type_parameters *) rt=result_type i=identifier L_PAR lpl=ioption(formal_parameters) R_PAR t=throws? SEMICOLON {
          match check_modifiers check_abstract_method_modifier imms with
          | true -> MethodDeclaration(imms, None, rt, i, lpl, t, None)
          | false -> error ("Invalid modifier for abstract method " ^ i) $startpos
  }
  | imms=extended_modifiers tps=type_parameters rt=result_type i=identifier L_PAR lpl=ioption(formal_parameters) R_PAR t=throws? SEMICOLON {
          match check_modifiers check_abstract_method_modifier imms with
          | true -> MethodDeclaration(imms, Some tps, rt, i, lpl, t, None)
          | false -> error ("Invalid modifier for abstract method " ^ i) $startpos
  }

(* XXX: abstract_method_modifiers replaced by extended_modifiers *)

(* SECTION 9.6 *)
(* TODO: modifiers for annotation was a bit hard, so we disabled it
 * Don't forget to do the post-process if you want to activate it *)
annotation_type_declaration: (* bodyDeclaration *)
  | (* no extended_modifiers *) AT INTERFACE i=identifier atb=annotation_type_body { AnnotationTypeDeclaration([], i, atb) }
  (*| ems=extended_modifiers AT INTERFACE i=identifier atb=annotation_type_body { AnnotationTypeDeclaration(ems, i, atb) } *)

annotation_type_body: (* bodyDeclaration list *)
  | L_BRACE ateds=annotation_type_element_declarations R_BRACE { ateds }

annotation_type_element_declarations: (* bodyDeclaration list *)
  | ateds=list(annotation_type_element_declaration) { ateds }

annotation_type_element_declaration: (* bodyDeclaration *)
  | (* no extended_modifiers *) t=type_ i=identifier L_PAR R_PAR dv=default_value? { AnnotationTypeMemberDeclaration([], t, i, dv) }
  | ems=extended_modifiers t=type_ i=identifier L_PAR R_PAR dv=default_value? { AnnotationTypeMemberDeclaration(ems, t, i, dv) }
  | (* no extended_modifiers *) t=array_type i=identifier L_PAR R_PAR dv=default_value? { AnnotationTypeMemberDeclaration([], t, i, dv) }
  | ems=extended_modifiers t=array_type i=identifier L_PAR R_PAR dv=default_value? { AnnotationTypeMemberDeclaration(ems, t, i, dv) }
  | cd=constant_declaration { cd }
  | cd=class_declaration { cd }
  | id=interface_declaration { id }
  | ed=enum_declaration { ed }
  | atd=annotation_type_declaration { atd }
  | SEMICOLON { EmptyBodyDeclaration }

default_value: (* expression *)
  | DEFAULT ev=element_value { ev }

(* SECTION 9.7 Annotations *)
annotations: (* expression list *)
  | ans=list(annotation) { ans }

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

