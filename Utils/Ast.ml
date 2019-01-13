type name =
    SimpleName of string
  | QualifiedName of name * name

type modifier =
  | PUBLIC
  | PROTECTED
  | PRIVATE
  | STATIC
  | ABSTRACT
  | FINAL
  | NATIVE
  | SYNCHRONIZED
  | TRANSIENT
  | VOLATILE
  | STRICTFP
  | DEFAULT

type operator =
    ASSIGN
  | PLUS_ASSIGN
  | MINUS_ASSIGN
  | MULTIPLY_ASSIGN
  | DIVIDE_ASSIGN
  | MODULUS_ASSIGN
  | AND_BITWISE_ASSIGN
  | OR_BITWISE_ASSIGN
  | XOR_ASSIGN
  | LEFT_SHIFT_ASSIGN
  | RIGHT_SHIFT_ASSIGN
  | RIGHT_SHIFT_UNSIGNED_ASSIGN
  (* post & prefix *)
  | INCREMENT
  | DECREMENT
  (* prefix *)
  | COMPLEMENT
  | NOT
  (* prefix & infix *)
  | PLUS
  | MINUS
  (* infix *)
  | MULTIPLY
  | DIVIDE
  | MODULO
  | LEFT_SHIFT
  | RIGHT_SHIFT
  | RIGHT_SHIFT_UNSIGNED
  | LOWER
  | GREATER
  | LOWER_OR_EQUAL
  | GREATER_OR_EQUAL
  | EQUAL
  | NOT_EQUAL
  | XOR_BITWISE
  | AND_BITWISE
  | OR_BITWISE
  | AND_LOGICAL
  | OR_LOGICAL

type variableDeclaration =
  | SingleVariableDeclaration of expression list option (* ExtendedModifier *) * type_ (*Type*) * expression list option (* Annotation *) * bool (* ... or not *) * string (* identifer *) * int (* Dimension *) * expression option (* = Expression *)
  | VariableDeclarationFragment of string (*identifier*) * int (*dimensions*) * expression option (*expression*)
and typeParameter = (*needs types *)
  | TypeParameter of type_ (*identifier*) * type_ list option (*extends: Type*)
and memberValuePair =
  | MemberValuePair of string (* name *) * expression
and type_ =
  | Byte
  | Short
  | Char
  | Int
  | Long
  | Float
  | Double
  | Boolean
  | Void
  | QualifiedType of type_ * expression
  | SimpleType of expression
  | WildcardType of type_ option
  | ArrayType of type_ * int
  | ParameterizedType of type_ * type_ list option
and expression =
    NormalAnnotation of expression (* type name *) * memberValuePair list option (* element value pairs *)
  | MarkerAnnotation of expression (*type name*)
  | SingleMemberAnnotation of expression (*type name*) * expression
  | Modifier (* because of ExtendedModifier *) of modifier
  | ArrayAccess of expression * expression
  | ArrayCreation of type_ * expression list option * int * expression option
  | ArrayInitializer of expression list option
  | Assignment of expression * operator * expression
  | BooleanLiteral of string
  | CastExpression of type_ * expression
  | CharacterLiteral of string
  | ClassInstanceCreation of expression list option * type_ list option * type_ * expression list option * bodyDeclaration list option
  | ConditionalExpression of expression * expression * expression
  (* Different from doc where InfixExpression is: Expression InfixOperator Expression { InfixOperator Expression } *)
  | InfixExpression of expression * operator * expression
  | FieldAccess of expression * expression
  | InstanceofExpression of expression * type_
  | MethodInvocation of expression list option * type_ list option * expression *  expression list option
  | ExpressionName of name
  | NullLiteral
  | NumberLiteral of string
  | ParenthesizedExpression of expression
  | PostfixExpression of expression * operator
  | PrefixExpression of expression * operator
  | StringLiteral of string
  | SuperFieldAccess of expression option * expression
  | SuperMethodInvocation of expression list option * type_ list option * expression *  expression list option
  | ThisExpression of expression option
  | TypeLiteral of type_ option
  (* | VariableDeclarationExpression *)

and bodyDeclaration =
  | AnnotationTypeDeclaration of expression list (* interfaceModifier *) * string (* identifier *) * bodyDeclaration list (* annotationTypeBody *) (* TODO : verify *)
  | EnumDeclaration of expression list (* modifiers *) * string (* identifier *) * type_ list option (* interfaces *) * bodyDeclaration list option (* enum_constants *) * bodyDeclaration list option (* enum_body *)
  | ClassDeclaration of expression list (* Extended modifier list *) * string (* Identifier *) * typeParameter list option (* TypeParameter list *) * type_ option (* Type option *) * type_ list option (* Type list *) * bodyDeclaration list option (* ClassBodyDeclaration list *)
  | InterfaceDeclaration of expression list (*interface_modifiers*) * string (*identifier*) * typeParameter list option (*type_parameters*) * type_ list option (*extends_interface*) * bodyDeclaration list option (*interface_body*)
  | AnnotationTypeMemberDeclaration of expression list (* interfaceMemberModifiers *) * type_ (* type *) * string (* identifier *) * expression option (* defaultValue *)(* TODO : verify *)
  | EnumConstantDeclaration of expression list (* annotations *) * string (* identifier *) * expression list option (* arguments *) * bodyDeclaration list option (* class_body *)
  | EnumBody of bodyDeclaration list option (* enum_constants *) * bodyDeclaration list option (* enum_body_declaration *)
  | FieldDeclaration of expression list (*field modifiers*) * type_ (*type*) * variableDeclaration list (*VariableDeclarationFragments*)
  | InstanceInitializer of statement
  | StaticInstanceInitializer of statement
  | ConstructorBody of statement option (* ExplicitConstructorInvocation *) * statement list option (* block_statements *)
  | ConstructorDeclaration of expression list (* contructor_modifiers *) * typeParameter list option (* type *) * string (* identifier *) * variableDeclaration list (* parameters  *) * type_ list option (* throws *) * bodyDeclaration (* constructor_body *)
  | MethodDeclaration of expression list (* extendedMofifiers *) * typeParameter list option (* type parameters *) * type_ (* resultType *) * string (* identifier *) * variableDeclaration list option (* formal parameters *) * type_ list option (* throws *) * statement option (* body *)
  | EmptyBodyDeclaration

and statement =
    AssertStatement of expression list
  | Block of statement list
  | BreakStatement of string option
  | ConstructorInvocation of type_ list option (* non wild type arguments *) * expression list option (* expressions *)
  | ContinueStatement of string option
  | DoStatement of statement * expression
  | EmptyStatement
  | EnhancedForStatement of variableDeclaration * expression * statement
  | ExpressionStatement of expression
  | ForStatement of statement list option * expression option * statement list option * statement
  | IfStatement of expression * statement * statement option
  | LabeledStatement of string * statement
  | ReturnStatement of expression option
  | SuperConstructorInvocation of expression option (* primary *) * type_ list option (* non wild type arguments *) * expression list option (* expressions *)
  | SwitchCase of expression option
  | SwitchStatement of expression * statement list
  | SynchronizedStatement of expression * statement  (*TODOMAYBE PROBLEM HERE *)
  | ThrowStatement of expression
  | TryStatement of statement * catch_clause list option * statement option
  (* | TypeDeclarationStatement *)
  | VariableDeclarationStatement of expression list * type_ * variableDeclaration list
  | WhileStatement of expression * statement
  | LocalClassDeclarationStatement of bodyDeclaration
and catch_clause =
  | CatchClause of variableDeclaration * statement
and importDeclaration =
    ImportDeclaration_ of bool (* static *) * expression (* name *) * bool (* .* : import all *)

and packageDeclaration =
    PackageDeclaration of expression list (* annotations *) * expression (* name *)

and compilationUnit =
    CompilationUnit of packageDeclaration option (* PackageDeclaration *) * importDeclaration list option (* ImportDeclaration *) * bodyDeclaration list option

and ast =
    | Tree of string * (ast list)
    | Treeopt of string * (ast option list)
    | Expression of expression
    | Statement of statement
    | Type of type_
    | CompilationUnit of compilationUnit
    | Leaf of string

(*                    *)
(*    AST checking    *)
(*                    *)

let check_contructor_modifer m =
  match m with
  | Modifier(PRIVATE) -> true
  | Modifier(PROTECTED) -> true
  | Modifier(PUBLIC) -> true
  | NormalAnnotation(_, _) -> true
  | MarkerAnnotation(_) -> true
  | SingleMemberAnnotation(_, _) -> true
  | _ -> false
;;

let check_class_modifier m =
  match m with
  | Modifier(PUBLIC) -> true
  | Modifier(PROTECTED) -> true
  | Modifier(PRIVATE) -> true
  | Modifier(ABSTRACT) -> true
  | Modifier(STATIC) -> true
  | Modifier(FINAL) -> true
  | Modifier(STRICTFP) -> true
  | NormalAnnotation(_, _) -> true
  | MarkerAnnotation(_) -> true
  | SingleMemberAnnotation(_, _) -> true
  | _ -> false
;;

let check_interface_modifier m =
  match m with
  | Modifier(PUBLIC) -> true
  | Modifier(PROTECTED) -> true
  | Modifier(PRIVATE) -> true
  | Modifier(ABSTRACT) -> true
  | Modifier(STATIC) -> true
  | Modifier(STRICTFP) -> true
  | NormalAnnotation(_, _) -> true
  | MarkerAnnotation(_) -> true
  | SingleMemberAnnotation(_, _) -> true
  | _ -> false
;;

let check_constant_modifier m =
  match m with
  | Modifier(PUBLIC) -> true
  | Modifier(STATIC) -> true
  | Modifier(FINAL) -> true
  | Modifier(STRICTFP) -> true
  | NormalAnnotation(_, _) -> true
  | MarkerAnnotation(_) -> true
  | SingleMemberAnnotation(_, _) -> true
  | _ -> false
;;

let check_abstract_method_modifier m =
  match m with
  | Modifier(PUBLIC) -> true
  | Modifier(PROTECTED) -> true
  | Modifier(PRIVATE) -> true
  | Modifier(STATIC) -> true
  | Modifier(FINAL) -> true
  | Modifier(STRICTFP) -> true
  | Modifier(ABSTRACT) -> true
  | _ -> false
;;

let check_method_modifier m =
  match m with
  | Modifier(PUBLIC) -> true
  | Modifier(PROTECTED) -> true
  | Modifier(PRIVATE) -> true
  | Modifier(ABSTRACT) -> true
  | Modifier(STATIC) -> true
  | Modifier(FINAL) -> true
  | Modifier(SYNCHRONIZED) -> true
  | Modifier(NATIVE) -> true
  | Modifier(STRICTFP) -> true
  | NormalAnnotation(_, _) -> true
  | MarkerAnnotation(_) -> true
  | SingleMemberAnnotation(_, _) -> true
  | _ -> false
;;

let check_package_modifier m =
  match m with
  | NormalAnnotation(_, _) -> true
  | MarkerAnnotation(_) -> true
  | SingleMemberAnnotation(_, _) -> true
  | _ -> false
;;

let rec check_modifiers checker cm =
    match cm with
    | [] -> true
    | m :: ms -> (check_modifiers checker ms) && (checker m)
;;

let rec check_formal_parameters fps =
  match fps with
  | [] -> true
  | [f] -> true
  | SingleVariableDeclaration(_, _, _, ellips, id, _, _)::tail -> (not ellips) && (check_formal_parameters tail)

(*                    *)
(* PRINTING FUNCTIONS *)
(*                    *)


(* Standard functions for printing and applying *)
let rec print_d d =
    if d <= 0 then
        ()
    else
        begin
            print_string "| ";
            print_d (d - 1)
        end
;;
let print_string_deep s deep =
    print_newline ();
    print_d deep;
    print_string s
;;

let apply_opt f aOpt deep =
    match aOpt with
        | None -> ()
        | Some a -> f a deep
;;

let rec apply_list f aList deep =
    match aList with
        | [] -> ()
        | a::restList -> f a deep; apply_list f restList deep
;;

let rec apply_opt_list f aOptList deep =
    apply_opt (apply_list f) aOptList deep
;;
(**)


let rec print_name name =
    match name with
        | SimpleName (name) -> print_string ("SimpleName(" ^ name ^ ")")
        | QualifiedName (n1, n2)
            -> print_string ("QualifiedName("); print_name n1; print_string " . "; print_name n2; print_string ")"
;;

let string_of_operator op =
    match op with
        | ASSIGN -> "="
        | PLUS_ASSIGN -> "+="
        | MINUS_ASSIGN -> "-="
        | MULTIPLY_ASSIGN -> "*="
        | DIVIDE_ASSIGN -> "/="
        | MODULUS_ASSIGN -> "%="
        | AND_BITWISE_ASSIGN -> "&="
        | OR_BITWISE_ASSIGN -> "|="
        | XOR_ASSIGN -> "^="
        | LEFT_SHIFT_ASSIGN -> "<<="
        | RIGHT_SHIFT_ASSIGN -> ">>="
        | RIGHT_SHIFT_UNSIGNED_ASSIGN -> ">>>="
        | INCREMENT -> "++"
        | DECREMENT -> "--"
        | PLUS -> "+"
        | MINUS -> "-"
        | COMPLEMENT -> "~"
        | NOT -> "!"
        | MULTIPLY -> "*"
        | DIVIDE -> "/"
        | MODULO -> "%"
        | LEFT_SHIFT -> "<<"
        | RIGHT_SHIFT -> ">>"
        | RIGHT_SHIFT_UNSIGNED -> ">>>"
        | LOWER -> "<"
        | GREATER -> ">"
        | LOWER_OR_EQUAL -> "<="
        | GREATER_OR_EQUAL -> ">="
        | EQUAL -> "=="
        | NOT_EQUAL -> "!="
        | XOR_BITWISE -> "|"
        | AND_BITWISE -> "&"
        | OR_BITWISE -> "^"
        | AND_LOGICAL -> "&&"
        | OR_LOGICAL -> "||"
;;

let string_of_modifier m =
    match m with
        | PUBLIC -> "public"
        | PROTECTED -> "protected"
        | PRIVATE -> "private"
        | STATIC -> "static"
        | ABSTRACT -> "abstract"
        | FINAL -> "final"
        | NATIVE -> "native"
        | SYNCHRONIZED -> "synchronized"
        | TRANSIENT -> "transient"
        | VOLATILE -> "volatile"
        | STRICTFP -> "strictfp"
        | DEFAULT -> "default"
;;

let print_modifier m deep =
    print_string_deep (string_of_modifier m) deep
;;

let rec print_variableDeclaration vd deep =
    match vd with
        | SingleVariableDeclaration (ml, t, el, b, i, d, e) ->
            print_string_deep "SingleVariableDeclaration" deep;
            print_string_deep "Modifiers: " (deep+1);
            apply_opt_list print_expression ml (deep+2);
            print_string_deep "Type: " (deep+1);
            print_type t (deep+2);
            print_string_deep "Annotations: " (deep+1);
            apply_opt_list print_expression el (deep+2);
            print_string_deep ("...: " ^ string_of_bool b) (deep+1);
            print_string_deep ("Identifier: " ^ i) (deep + 1);
            print_string_deep ("Dims: " ^ string_of_int d) (deep+1);
            print_string_deep "Exp: " (deep+1);
            apply_opt print_expression e (deep+2)
        | VariableDeclarationFragment (i, d, e) ->
            print_string_deep "VariableDeclarationFragment" deep;
            print_string_deep i (deep + 1);
            print_string_deep (string_of_int d) (deep + 1);
            apply_opt print_expression e (deep + 1)

and print_typeParameter tp deep =
    match tp with
        | TypeParameter (i, tL) ->
            print_string_deep "TypeParameter" deep;
            print_type i (deep + 1);
            apply_opt_list print_type tL (deep + 1)

and print_memberValuePair mvp deep =
    match mvp with
        | MemberValuePair (n, e) ->
            print_string_deep "MemberValuePair" deep;
            print_string_deep n (deep + 1);
            print_expression e (deep + 1)
and print_type t deep =
    match t with
        | Byte -> print_newline(); print_d deep; print_string "Byte"
        | Short -> print_newline(); print_d deep; print_string "Short"
        | Char -> print_newline(); print_d deep; print_string "Char"
        | Int -> print_newline(); print_d deep; print_string "Int"
        | Long -> print_newline(); print_d deep; print_string "Long"
        | Float -> print_newline(); print_d deep; print_string "Float"
        | Double -> print_newline(); print_d deep; print_string "Double"
        | Boolean -> print_newline(); print_d deep; print_string "Boolean"
        | Void -> print_newline(); print_d deep; print_string "Void"
        | QualifiedType (t, e) ->
            print_string_deep "QualifiedType" deep;
            print_type t (deep+1);
            print_expression e (deep+1);
        | SimpleType (e) ->
            print_string_deep "SimpleType" deep;
            print_expression e (deep+1);
        | WildcardType (t) ->
            print_string_deep "WildcardType" deep;
            apply_opt print_type t (deep+1)
        | ArrayType (t, d) ->
            print_string_deep "ArrayType" deep;
            print_type t (deep+1);
            print_string_deep ("Dims: " ^ string_of_int d) (deep+1)
        | ParameterizedType (t, tl) ->
            print_string_deep "ParameterizedType" deep;
            print_type t (deep+1);
            apply_opt_list print_type tl (deep+1)
and print_expression e deep =
    let print_array_access e1 e2 deep =
        print_string_deep "ArrayAccess" deep;
        print_expression e1 (deep + 1);
        print_expression e2 (deep + 1);
    in
    let print_array_creation t el d ai deep =
        print_string_deep "ArrayCreation" deep;
        print_string_deep "Type: " (deep+1);
        print_type t (deep + 2);
        print_string_deep "DimExps: " (deep+1);
        apply_opt_list print_expression el (deep+2);
        print_string_deep ("AdditionnalDims: " ^ (string_of_int d))  (deep+1);
        print_string_deep "ArrayInit" (deep+1);
        apply_opt print_expression ai (deep+2);
    in
    let print_array_initializer e deep =
        print_string_deep "ArrayInitializer" deep;
        apply_opt_list print_expression e (deep+1)
    in
    let print_assignment lfs op e deep =
        print_string_deep "Assignment" deep;
        print_expression lfs (deep + 1);
        print_string_deep ("Operator: " ^ string_of_operator op) (deep+1);
        print_expression e (deep + 1);
    in
    let print_bool_literal bool_ deep =
        print_string_deep ("BooleanLiteral: " ^ bool_) deep;
    in
    let print_cast_expression t e deep =
        print_string_deep "CastExpression" deep;
        print_type t (deep+1);
        print_expression e (deep+1);
    in
    let print_char_literal char_ deep =
        print_string_deep ("CharacterLiteral: " ^ char_) deep;
    in
    let print_class_instance_creation e1 t1 t2 e2 cd deep =
        print_string_deep "ClassInstanceCreation" deep;
        print_string_deep ("Left Exps:") (deep +1);
        apply_opt_list print_expression e2 (deep + 2);
        print_string_deep ("Template Types:") (deep + 1);
        apply_opt_list print_type t1 (deep + 2);
        print_string_deep ("Type:") (deep + 1);
        print_type t2 (deep + 2);
        print_string_deep ("Right Exps:") (deep + 1);
        apply_opt_list print_expression e2 (deep + 2);
        print_string_deep ("Body Declaration: ") (deep + 1);
        apply_opt (apply_list print_bodyDeclaration) cd (deep + 2);
    in
    let print_conditional_expression e1 e2 e3 deep =
        print_string_deep "ConditionalExpression" deep;
        print_expression e1 (deep+1);
        print_expression e2 (deep+1);
        print_expression e3 (deep+1);
    in
    let print_field_access e1 e2 deep =
        print_string_deep "FieldAccess" deep;
        print_expression e1 (deep+1);
        print_expression e2 (deep+1);
    in
    let print_infix_expression e1 op e2 deep =
        print_string_deep "InfixExpression" deep;
        print_expression e1 (deep+1);
        print_string_deep ("Operator: " ^ string_of_operator op) (deep+1);
        print_expression e2 (deep+1);
    in
    let print_instance_expression e t deep =
        print_string_deep "InstanceofExpression" deep;
        print_expression e (deep+1);
        print_type t (deep+1);
    in
    let print_expression_name name deep =
        print_string_deep "Name: " deep;
        print_name name;
    in
    let print_method_invocation el1 tl e el2 deep =
        print_string_deep "MethodInvocation" deep;
        print_string_deep "Exps: " (deep+1);
        apply_opt_list print_expression el1 (deep+2);
        print_string_deep "Types: " (deep+1);
        apply_opt_list print_type tl (deep+2);
        print_string_deep "Name: " (deep+1);
        print_expression e (deep+2);
        print_string_deep "Args: " (deep+1);
        apply_opt_list print_expression el2 (deep+2)
    in
    let print_null_literal deep =
        print_string_deep "NullLiteral" deep;
    in
    let print_number_literal number deep =
        print_string_deep ("NumberLiteral: " ^ number) deep;
    in
    let print_parenthesized_expression e deep =
        print_string_deep ("ParenthesizedExpression") deep;
        print_expression e (deep+1);
    in
    let print_postfix_expression e op deep =
        print_string_deep ("PostfixExpression") deep;
        print_expression e (deep+1);
        print_string_deep (string_of_operator op) (deep+1);
    in
    let print_prefix_expression e op deep =
        print_string_deep "PrefixExpression" deep;
        print_expression e (deep+1);
        print_string_deep (string_of_operator op) (deep+1);
    in
    let print_string_literal string_ deep =
        print_string_deep ("StringLiteral: " ^ string_) deep;
    in
    let print_super_field_access e1 e2 deep =
        print_string_deep "SuperFieldAccess" deep;
        apply_opt print_expression e1 (deep+1);
        print_expression e2 (deep+1);
    in
    let print_super_method_invocation c tl e el deep =
        print_string_deep "SuperMethodInvocation" deep;
        print_string_deep "ClassName: " (deep+1);
        apply_opt_list print_expression c (deep+2);
        print_string_deep "Types: " (deep+1);
        apply_opt_list print_type tl (deep+2);
        print_string_deep "Name: " (deep+1);
        print_expression e (deep+2);
        print_string_deep "Args: " (deep+1);
        apply_opt_list print_expression el (deep+2)
    in
    let print_this_expression e deep =
        print_string_deep "ThisExpression" deep;
        apply_opt print_expression e (deep+1);
    in
    let print_type_literal t deep =
        print_string_deep "TypeLiteral" deep;
        apply_opt print_type t (deep+1);
    in
    match e with
        | NormalAnnotation(tn, evpL) ->
            print_string_deep "NormalAnnotation" deep;
            print_expression tn (deep + 1);
            apply_opt_list print_memberValuePair evpL (deep + 1)
        | MarkerAnnotation (tn) ->
            print_string_deep "MarkerAnnotation" deep;
            print_expression tn (deep + 1)
        | SingleMemberAnnotation (tn, e) ->
            print_string_deep "SingleMemberAnnotation" deep;
            print_expression tn (deep + 1);
            print_expression e (deep + 1)
        | Modifier (s) ->
            print_string_deep "Modifier";
            print_modifier s deep
        | ArrayAccess(e1, e2) -> print_array_access e1 e2 deep
        | ArrayCreation(t, el, d, ai) -> print_array_creation t el d ai deep
        | ArrayInitializer(e) -> print_array_initializer e deep
        | Assignment (lfs, op, e) -> print_assignment lfs op e deep
        | BooleanLiteral (bool_) -> print_bool_literal bool_ deep
        | CastExpression (t, e) -> print_cast_expression t e deep
        | CharacterLiteral (char_) -> print_char_literal char_ deep
        | ClassInstanceCreation (e1, t1, t2, e2, cd) -> print_class_instance_creation e1 t1 t2 e2 cd deep
        | ConditionalExpression(e1, e2, e3) -> print_conditional_expression e1 e2 e3 deep
        | ExpressionName (name) -> print_expression_name name deep
        | FieldAccess (e1, e2) -> print_field_access e1 e2 deep
        | InfixExpression (e1, op, e2) -> print_infix_expression e1 op e2 deep
        | InstanceofExpression(e, t) -> print_instance_expression e t deep
        | MethodInvocation (el1, tl, e, el2) -> print_method_invocation el1 tl e el2 deep
        | NullLiteral -> print_null_literal deep
        | NumberLiteral (number) -> print_number_literal number deep
        | ParenthesizedExpression (e) -> print_parenthesized_expression e deep
        | PostfixExpression (e, op) -> print_postfix_expression e op deep
        | PrefixExpression (e, op) -> print_prefix_expression e op deep
        | StringLiteral (string_) -> print_string_literal string_ deep
        | SuperFieldAccess(e1, e2) -> print_super_field_access e1 e2 deep
        | SuperMethodInvocation (c, tl, e, el) -> print_super_method_invocation c tl e el deep
        | ThisExpression (e) -> print_this_expression e deep
        | TypeLiteral (t) -> print_type_literal t deep
and print_statement s deep =
    let print_assert_statement exps deep =
        print_string_deep "AssertStatement" deep;
        apply_list print_expression exps (deep+1);
    in
    let print_block s deep =
        print_string_deep "Block" deep;
        apply_list print_statement s (deep+1);
    in
    let print_break_statement s deep =
        print_string_deep "BreakStatement" deep;
        apply_opt print_string_deep s (deep+1);
    in
    let print_continue_statement s deep =
        print_string_deep "ContinueStatement" deep;
        apply_opt print_string_deep s (deep+1);
    in
    let print_do_statement s e deep =
        print_string_deep "DoStatement" deep;
        print_statement s (deep+1);
        print_expression e (deep+1);
    in
    let print_empty_statement deep =
        print_string_deep "EmptyStatement" deep;
    in
    let print_enhanced_for_statement fp e s deep =
        print_string_deep "EnhancedForStatement" deep;
        print_string_deep "FormalParams:" (deep+1);
        print_variableDeclaration fp (deep+2);
        print_string_deep "Exp:" (deep+1);
        print_expression e (deep+2);
        print_string_deep "Statement:" (deep+1);
        print_statement s (deep+2);
    in
    let print_exp_statement e deep =
        print_string_deep "ExpressionStatement" deep;
        print_expression e (deep+1);
    in
    let print_for_statement for_init e for_update s deep =
        print_string_deep "ForStatement" deep;
        print_string_deep "ForInit: " (deep+1);
        apply_opt_list print_statement for_init (deep+2);
        print_string_deep "Exp: " (deep+1);
        apply_opt print_expression e (deep+2);
        print_string_deep "ForUpdate: " (deep+1);
        apply_opt_list print_statement for_update (deep+2);
        print_statement s (deep+1);
    in
    let print_if_statement e s s_else deep =
        print_string_deep "IfStatement" deep;
        print_expression e (deep+1);
        print_statement s (deep+1);
        apply_opt print_statement s_else (deep+1);
    in
    let print_labeled_statement l s deep =
        print_string_deep "LabeledStatement" deep;
        print_string_deep l;
        print_statement s (deep+1);
    in
    let print_return_statement e deep =
        print_string_deep "ReturnStatement" deep;
        apply_opt print_expression e (deep+1)
    in
    let print_switch_case e deep =
        print_string_deep "SwitchCase" deep;
        apply_opt print_expression e (deep+1)
    in
    let print_switch_statement e s deep =
        print_string_deep "SwitchStatement" deep;
        print_expression e (deep+1);
        apply_list print_statement s (deep+1);
    in
    let print_synchronized_statement e s deep =
        print_string_deep "SynchronizedStatement" deep;
        print_expression e (deep+1);
        print_statement s (deep+1);
    in
    let print_throw_statement e deep =
        print_string_deep "ThrowStatement" deep;
        print_expression e (deep+1);
    in
    let print_try_statement b cl bf deep =
        print_string_deep "TryStatement" deep;
        print_statement b (deep+1);
        apply_opt_list print_catch_clause cl (deep+1);
        apply_opt print_statement bf (deep+1);
    in
    let print_variable_declaration_statement a t d deep =
        print_string_deep "VariableDeclarationStatement" deep;
        apply_list print_expression a (deep + 1);
        print_type t (deep + 1);
        apply_list print_variableDeclaration d (deep + 1);
    in
    let print_while_statement e s deep =
        print_string_deep "WhileStatement" deep;
        print_expression e (deep+1);
        print_statement s (deep+1);
    in
    let print_local_class_declaration_statement d deep =
        print_string_deep "LocalClassDecalarationStatement" deep;
        print_bodyDeclaration d (deep+1);
    in
    match s with
        | AssertStatement (exps) -> print_assert_statement exps deep
        | Block (s) -> print_block s deep
        | BreakStatement (s) -> print_break_statement s deep
        | ConstructorInvocation (nwta, arg) ->
            print_string_deep "ConstructorInvocation" deep;
            apply_opt_list print_type nwta (deep + 1);
            apply_opt_list print_expression arg (deep + 1)
        | ContinueStatement (s) -> print_continue_statement s deep
        | DoStatement (s, e) -> print_do_statement s e deep
        | EmptyStatement -> print_empty_statement deep
        | EnhancedForStatement (fp, e, s) -> print_enhanced_for_statement fp e s deep
        | ExpressionStatement (e) -> print_exp_statement e deep
        | ForStatement (for_init, e, for_update, s) -> print_for_statement for_init e for_update s deep
        | IfStatement (e, s1, s2) -> print_if_statement e s1 s2 deep
        | LabeledStatement (l, s) -> print_labeled_statement l s deep
        | ReturnStatement (e) -> print_return_statement e deep
        | SuperConstructorInvocation (p, nwta, arg) ->
            print_string_deep "SuperConstructorInvocation" deep;
            apply_opt print_expression p (deep + 1);
            apply_opt_list print_type nwta (deep + 1);
            apply_opt_list print_expression arg (deep + 1)
        | SwitchCase  (e) -> print_switch_case e deep
        | SwitchStatement  (e, s) -> print_switch_statement e s deep
        | SynchronizedStatement (e, s) -> print_synchronized_statement e s deep
        | ThrowStatement (e) -> print_throw_statement e deep
        | TryStatement (b, cl, bf) -> print_try_statement b cl bf deep
        | VariableDeclarationStatement (a, t, d) -> print_variable_declaration_statement a t d deep
        | WhileStatement (e, s) -> print_while_statement e s deep
        | LocalClassDeclarationStatement (d) -> print_local_class_declaration_statement d deep
and print_opt_statement os depth =
  match os with
    | Some s -> print_statement s depth
    | None -> ()
(*
let rec print_bodyDeclaration bd deep =
    let print_string_deep a deep =
        print_string_deep a
    in
    let print_opt f aOpt deep =
        match aOpt with
            | None -> ()
            | Some a -> f a deep
    in
    let rec print_list f aList deep =
        match aList with
            | [] -> ()
            | a::restList -> f a deep; print_list f restList deep
    in
    let print_classDeclaration jdOpt emList i tpList tOpt tList cbdList deep =
        print_string_deep "ClassDeclaration" deep;
        print_opt   print_string_deep jdOpt   (deep + 1);
        print_list  print_string_deep emList  (deep + 1);
        print_string_deep i (deep + 1);
        print_list  print_string_deep tpList  (deep + 1);
        print_opt   print_string_deep tOpt    (deep + 1);
        print_list  print_string_deep tList   (deep + 1);
        print_list  print_string_deep cbdList (deep + 1);
    in
    match bd with
        | ClassDeclaration (jdOpt, emList, i, tpList, tOpt, tList, cbdList) -> print_classDeclaration jdOpt emList i tpList tOpt tList cbdList deep
;;
*)
and print_catch_clause c deep =
    match c with
        | CatchClause (p, b) ->
            print_string_deep "CatchClause" deep;
            print_variableDeclaration p (deep+1);
            print_statement b (deep+1)
and print_packageDeclaration p deep =
    match p with
        | PackageDeclaration(annotations, name) ->
            print_string_deep "PackageDeclaration" deep;
            apply_list print_expression annotations (deep + 1);
            (*print_string_deep "annotations" deep;*)
            print_expression name (deep + 1)
            (*print_string_deep "name" deep*)
and print_importDeclaration i deep =
    match i with
        | ImportDeclaration_(static, name, import_all) ->
            print_string_deep "ImportDeclaration" deep;
            print_string_deep (string_of_bool static) (deep + 1);
            print_expression name (deep + 1);
            print_string_deep (string_of_bool import_all) (deep + 1)
and print_bodyDeclaration bd deep =
    match bd with
        | ClassDeclaration(cm, i, tp, s, it, cbLO) ->
            print_string_deep "ClassDeclaration"       deep;
            apply_list print_expression      cm   (deep + 1);
            print_string_deep                    i    (deep + 1);
            apply_opt_list print_typeParameter   tp   (deep + 1);
            apply_opt print_type                 s    (deep + 1);
            apply_opt_list print_type            it   (deep + 1);
            apply_opt_list print_bodyDeclaration cbLO (deep + 1)
        | InterfaceDeclaration (im, i, tp, ei, ib) ->
            print_string_deep "InterfaceDeclaration"   deep;
            apply_list print_expression      im   (deep + 1);
            print_string_deep                    i    (deep + 1);
            apply_opt_list print_typeParameter   tp   (deep + 1);
            apply_opt_list print_type            ei   (deep + 1);
            apply_opt_list print_bodyDeclaration ib   (deep + 1);
        | FieldDeclaration(fm, t, vdL) ->
            print_string_deep "FieldDeclaration" deep;
            apply_list print_expression      fm   (deep + 1);
            print_type                           t    (deep + 1);
            apply_list print_variableDeclaration vdL  (deep + 1)
        | EnumDeclaration (em, i, it, ec, eb) ->
            print_string_deep "EnumDeclaration" deep;
            apply_list print_expression em        (deep + 1);
            print_string_deep i                       (deep + 1);
            apply_opt_list print_type            it   (deep + 1);
            apply_opt_list print_bodyDeclaration ec   (deep + 1);
            apply_opt_list print_bodyDeclaration eb   (deep + 1)
        | AnnotationTypeDeclaration (em, i, atb) ->
            print_string_deep "AnnotationTypeDeclaration" deep;
            print_string_deep "Modifiers : "             (deep + 1);
            apply_list print_expression em               (deep + 2);
            print_string_deep "Identifier : "            (deep + 1);
            print_string_deep i                          (deep + 2);
            print_string_deep "ElementsDeclaration : "   (deep + 1);
            apply_list print_bodyDeclaration atb         (deep + 2)
        | AnnotationTypeMemberDeclaration (imm, t, i, dv) ->
            print_string_deep "AnnotationTypeMemberDeclaration" deep;
            print_string_deep "InterfaceMemberModifiers : "    (deep + 1);
            apply_list print_expression imm                    (deep + 2);
            print_string_deep "Type : "                        (deep + 1);
            print_type t                                       (deep + 2);
            print_string_deep "Identifier : "                  (deep + 1);
            print_string_deep i                                (deep + 2);
            print_string_deep "DefaultValue : "                (deep + 1);
            apply_opt print_expression dv                      (deep + 2)
        | EnumConstantDeclaration (an, i, args, eb) ->
            print_string_deep "EnumConstantDeclaration" deep;
            apply_list print_expression      an   (deep + 1);
            print_string_deep i                       (deep + 1);
            apply_opt_list print_expression args (deep + 1);
            apply_opt_list print_bodyDeclaration eb   (deep + 1);
        | InstanceInitializer b ->
            print_string_deep "InstanceInitializer" deep;
            print_statement b (deep + 1)
        | StaticInstanceInitializer b ->
            print_string_deep "StaticInstanceInitializer" deep;
            print_statement b (deep + 1)
        | ConstructorDeclaration (cm,  tp, id, params, throws, bd) ->
            print_string_deep "ConstructorDeclaration" deep;
            print_string_deep id    (deep + 1);
            apply_list print_expression cm (deep + 1);
            apply_opt_list print_typeParameter tp (deep + 1);
            apply_list print_variableDeclaration params (deep + 1);
            apply_opt_list print_type throws (deep + 1);
            print_bodyDeclaration bd (deep + 1)
        | MethodDeclaration (em, tp, rt, i, lpl, t, mb) ->
            print_string_deep "MethodDeclaration" deep;
            apply_opt_list print_typeParameter tp (deep + 1);
            print_type rt (deep + 1);
            print_string_deep i (deep + 1);
            apply_opt_list print_variableDeclaration lpl (deep + 1);
            apply_opt_list print_type t (deep + 1);
            print_opt_statement mb (deep + 1)
        | ConstructorBody (ei, bs ) ->
            print_string_deep "ConstructorBody" deep;
            apply_opt print_statement ei (deep + 1);
            apply_opt_list print_statement bs (deep + 1)
        | EmptyBodyDeclaration ->
            print_string_deep "EmptyDeclaration" deep;
;;


let print_compilationUnit cu deep =
    match cu with
        | CompilationUnit(p, i, t) ->
            print_string_deep "CompilationUnit" deep;
            apply_opt print_packageDeclaration             p (deep + 1);
            apply_opt (apply_list print_importDeclaration) i (deep + 1);
            apply_opt (apply_list print_bodyDeclaration)   t (deep + 1)
;;

let print_ast ast =

    let print_elt name deep =
            print_d deep;
            print_string "\\";
            print_string name;
            print_newline ();
    in
    let print_leaf name deep =
            print_d deep;
            print_string name;
            print_newline ();
    in
    let rec print_ast_rec ast deep =
        let rec print_list list_to_print deep =
            match list_to_print with
                | [] -> ()
                | x::m -> print_ast_rec x deep; print_list m deep
        in
        let rec print_list_opt list_to_print deep =
            match list_to_print with
                | [] -> ()
                | (None)::m -> print_ast_rec (Leaf("None")) deep; print_list_opt m deep
                | (Some x)::m -> print_ast_rec x deep; print_list_opt m deep
        in
        match ast with
          | Tree (name, astlist) -> print_elt name deep; print_list astlist (deep + 1)
          | Treeopt (name, astlist) -> print_elt name deep; print_list_opt astlist (deep + 1)
          | Leaf (name) -> print_leaf name deep
          | Expression (e) -> print_expression e (deep + 1)
          | Statement (s) -> print_statement s (deep + 1)
    in
    print_ast_rec ast 0;
    print_newline ()
;;

(*
let testval = Tree("name1", [Tree("name2", [Leaf("leaf1") ; Leaf("leaf2")]) ; Leaf("leaf3") ; Treeopt("tree1", [Some (Leaf("leaf4")) ; None])]);;
print_ast testval ;;
*)
