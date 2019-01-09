type names =
    SimpleName of string
  | QualifiedName of string list

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


type expression =
    (* Annotation *)
  | ArrayAccess of expression * expression
  (* | ArrayCreation *)
  | ArrayInitializer of expression list option
  | Assignment of expression * operator * expression
  | BooleanLiteral of string
  (* | CastExpression *)
  | CharacterLiteral of string
  (* | ClassInstanceCreation *)
  | ConditionalExpression of expression * expression * expression
  (* | CreationReference *)
  (* | ExpressionMethodReference *)
  (* Different from doc where InfixExpression is: Expression InfixOperator Expression { InfixOperator Expression } *)
  | InfixExpression of expression * operator * expression
  | FieldAccess of expression * expression
  (* | InstanceofExpression of expression * string TODOreplace string by type *)
  (* | LambdaExpression *)
  (* | MethodInvocation *)
  (* | MethodReference *)
  | ExpressionName of names
  | NullLiteral
  | NumberLiteral of string
  | ParenthesizedExpression of expression
  | PostfixExpression of expression * operator
  | PrefixExpression of expression * operator
  | StringLiteral of string
  | SuperFieldAccess of expression option * expression
  (* | SuperMethodInvocation *)
  (* | SuperMethodReference *)
  (* | ThisExpression *)
  (* | TypeLiteral *)
  (* | TypeMethodReference *)
  (* | VariableDeclarationExpression *)

type statement =
    AssertStatement of expression list
  | Block of statement list
  | BreakStatement of string option
  (* | ConstructorInvocation *)
  | ContinueStatement of string option
  | DoStatement of statement * expression
  | EmptyStatement
  (* | EnhancedForStatement *)
  | ExpressionStatement of expression
  | ForStatement of expression list option * expression option * expression list option * statement
  | IfStatement of expression * statement * statement option
  | LabeledStatement of string * statement
  | ReturnStatement of expression option
  (* | SuperConstructorInvocation *)
  | SwitchCase of expression option
  | SwitchStatement of expression * statement list
  | SynchronizedStatement of expression * statement  (*TODOMAYBE PROBLEM HERE *)
  | ThrowStatement of expression
  (* | TryStatement *)
  (* | TypeDeclarationStatement *)
  (* | VariableDeclarationStatement *)
  | WhileStatement of expression * statement

type bodyDeclaration =
  (* | AbstractTypeDeclaration_AnnotationTypeDeclaration *)
  (* | AbstractTypeDeclaration_EnumDeclaration *)
  | ClassDeclaration of string option * string * string option * string option * string option * string (* ExtendedModifier list * Identifier * TypeParameter list * Type option * Type list * ClassBodyDeclaration list *)
  (* | AbstractTypeDeclaration_TypeDeclaration_InterfaceDeclaration *)
  (* | AnnotationTypeMemberDeclaration *)
  (* | EnumConstantDeclaration *)
  (* | FieldDeclaration *)
  (* | Initializer *)
  (* | MethodDeclaration *)

type importDeclaration =
    ImportDeclaration_ of bool (* static *) * string (* name *) * bool (* .* : import all *)

type packageDeclaration =
    PackageDeclaration_ of string (* annotations *) * string (* name *)

type compilationUnit =
    CompilationUnit_ of packageDeclaration option (* PackageDeclaration *) * importDeclaration list option (* ImportDeclaration *) * bodyDeclaration list option

type ast =
    | Tree of string * (ast list)
    | Treeopt of string * (ast option list)
    | Expression of expression
    | Statement of statement
    | CompilationUnit of compilationUnit
    | Leaf of string

let rec print_d d =
    if d <= 0 then
        ()
    else
        begin
            print_string "| ";
            print_d (d - 1)
        end
;;

let print_global_name name =
    match name with
        | SimpleName (name) -> print_string ("SimpleName(" ^ name ^ ")")
        | QualifiedName (names) -> print_string ("QualifiedName(" ^ (String.concat "." names) ^ ")")
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

let print_opt_string s deep =
    match s with
        | None -> ()
        | Some str -> print_newline (); print_d deep; print_string str
;;

let rec print_expression e deep =
    let rec print_expression_list s deep =
        match s with
            | [] -> ()
            | e::l -> print_expression e deep; print_expression_list l deep;
    in
    let print_opt_expression e deep=
        match e with
            | None -> ()
            | Some ex -> print_expression ex deep
    in
    let print_array_access e1 e2 deep =
        print_newline ();
        print_d deep;
        print_string "ArrayAccess";
        print_expression e1 (deep + 1);
        print_expression e2 (deep + 1);
    in
    let print_array_initializer e deep =
        let print_opt_exp_list e deep =
            match e with
                | None -> ()
                | Some e -> print_expression_list e deep
        in
        print_newline ();
        print_d deep;
        print_string "ArrayInitializer";
        print_opt_exp_list e (deep+1)
    in
    let print_assignment lfs op e deep =
        print_newline ();
        print_d deep;
        print_string "Assignment";
        print_expression lfs (deep + 1);
        print_newline ();
        print_d (deep+1);
        print_string ("Operator: " ^ string_of_operator op);
        print_expression e (deep + 1);
    in
    let print_bool_literal bool_ deep =
        print_newline ();
        print_d deep;
        print_string ("BooleanLiteral: " ^ bool_);
    in
    let print_char_literal char_ deep =
        print_newline ();
        print_d deep;
        print_string ("CharacterLiteral: " ^ char_);
    in
    let print_conditional_expression e1 e2 e3 deep =
        print_newline ();
        print_d deep;
        print_string ("ConditionalExpression");
        print_expression e1 (deep+1);
        print_expression e2 (deep+1);
        print_expression e3 (deep+1);
    in
    let print_field_access e1 e2 deep =
        print_newline ();
        print_d deep;
        print_string ("FieldAccess");
        print_expression e1 (deep+1);
        print_expression e2 (deep+1);
    in
    let print_infix_expression e1 op e2 deep =
        print_newline ();
        print_d deep;
        print_string ("InfixExpression");
        print_expression e1 (deep+1);
        print_newline ();
        print_d (deep+1);
        print_string ("Operator: " ^ string_of_operator op);
        print_expression e2 (deep+1);
    in
    let print_name name deep =
        print_newline ();
        print_d deep;
        print_string "Name: ";
        print_global_name name;
    in
    let print_null_literal deep =
        print_newline ();
        print_d deep;
        print_string "NullLiteral";
    in
    let print_number_literal number deep =
        print_newline ();
        print_d deep;
        print_string ("NumberLiteral: " ^ number);
    in
    let print_parenthesized_expression e deep =
        print_newline ();
        print_d deep;
        print_string ("ParenthesizedExpression");
        print_expression e (deep+1);
    in
    let print_postfix_expression e op deep =
        print_newline ();
        print_d deep;
        print_string ("PostfixExpression");
        print_expression e (deep+1);
        print_newline ();
        print_d (deep+1);
        print_string (string_of_operator op);
    in
    let print_prefix_expression e op deep =
        print_newline ();
        print_d deep;
        print_string ("PrefixExpression");
        print_expression e (deep+1);
        print_newline ();
        print_d (deep+1);
        print_string (string_of_operator op);
        in
    let print_string_literal string_ deep =
        print_newline ();
        print_d deep;
        print_string ("StringLiteral: " ^ string_);
    in
    let print_super_field_access e1 e2 deep =
        print_newline ();
        print_d deep;
        print_string ("SuperFieldAccess");
        print_opt_expression e1 (deep+1);
        print_expression e2 (deep+1);
    in
    match e with
        | ArrayAccess(e1, e2) -> print_array_access e1 e2 deep
        | ArrayInitializer(e) -> print_array_initializer e deep
        | Assignment (lfs, op, e) -> print_assignment lfs op e deep
        | BooleanLiteral (bool_) -> print_bool_literal bool_ deep
        | CharacterLiteral (char_) -> print_char_literal char_ deep
        | ConditionalExpression(e1, e2, e3) -> print_conditional_expression e1 e2 e3 deep
        | ExpressionName (name) -> print_name name deep
        | FieldAccess (e1, e2) -> print_field_access e1 e2 deep
        | InfixExpression (e1, op, e2) -> print_infix_expression e1 op e2 deep
        | NullLiteral -> print_null_literal deep
        | NumberLiteral (number) -> print_number_literal number deep
        | ParenthesizedExpression (e) -> print_parenthesized_expression e deep
        | PostfixExpression (e, op) -> print_postfix_expression e op deep
        | PrefixExpression (e, op) -> print_prefix_expression e op deep
        | StringLiteral (string_) -> print_string_literal string_ deep
        | SuperFieldAccess(e1, e2) -> print_super_field_access e1 e2 deep
;;

let print_opt_expression e deep=
    match e with
        | None -> ()
        | Some ex -> print_expression ex deep
;;

let rec print_statement s deep =
    let rec print_expression_list s deep =
        match s with
            | [] -> ()
            | e::l -> print_expression e deep; print_expression_list l deep;
    in
    let rec print_statement_list s deep =
        match s with
            | [] -> ()
            | e::l -> print_statement e deep; print_statement_list l deep
    in
    let print_opt_statement s deep =
        match s with
            | None -> ()
            | Some st -> print_statement st deep
    in
    let print_assert_statement exps deep =
        print_newline ();
        print_d deep;
        print_string "AssertStatement";
        print_expression_list exps (deep+1);
    in
    let print_block s deep =
        print_newline ();
        print_d deep;
        print_string "Block";
        print_statement_list s (deep+1);
    in
    let print_break_statement s deep =
        print_newline ();
        print_d deep;
        print_string "BreakStatement";
        print_opt_string s (deep+1);
    in
    let print_continue_statement s deep =
        print_newline ();
        print_d deep;
        print_string "ContinueStatement";
        print_opt_string s (deep+1);
    in
    let print_do_statement s e deep =
        print_newline ();
        print_d deep;
        print_string "DoStatement";
        print_statement s (deep+1);
        print_expression e (deep+1);
    in
    let print_empty_statement deep =
        print_newline ();
        print_d deep;
        print_string "EmptyStatement";
    in
    let print_exp_statement e deep =
        print_newline ();
        print_d deep;
        print_string "ExpressionStatement";
        print_expression e (deep+1);
    in
    let print_for_statement for_init e for_update s deep =
        let print_opt_expression e deep =
            match e with
                | None -> print_string "None"
                | Some exp -> print_expression exp deep
        in
        let print_opt_expression_list e deep =
            match e with
                | None -> print_string "None"
                | Some exp -> print_expression_list exp deep
        in
        print_newline ();
        print_d deep;
        print_string "ForStatement";
        print_newline ();
        print_d (deep+1);
        print_string "ForInit: ";
        print_opt_expression_list for_init (deep+2);
        print_newline ();
        print_d (deep+1);
        print_string "Exp: ";
        print_opt_expression e (deep+2);
        print_newline ();
        print_d (deep+1);
        print_string "ForUpdate: ";
        print_opt_expression_list for_update (deep+2);
        print_statement s (deep+1);
    in
    let print_if_statement e s s_else deep =
        let print_opt_statement s deep =
            match s with
                | None -> ()
                | Some st -> print_statement st deep
        in
        print_newline ();
        print_d deep;
        print_string "IfStatement";
        print_expression e (deep+1);
        print_statement s (deep+1);
        print_opt_statement s_else (deep+1);
    in
    let print_labeled_statement l s deep =
        print_newline ();
        print_d deep;
        print_string "LabeledStatement";
        print_newline ();
        print_d (deep+1);
        print_string l;
        print_statement s (deep+1);
    in
    let print_return_statement e deep =
        print_newline ();
        print_d deep;
        print_string "ReturnStatement";
        print_opt_expression e (deep+1)
    in
    let print_switch_case e deep =
        print_newline ();
        print_d deep;
        print_string "SwitchCase";
        print_opt_expression e (deep+1)
    in
    let print_switch_statement e s deep =
        print_newline ();
        print_d deep;
        print_string "SwitchStatement";
        print_expression e (deep+1);
        print_statement_list s (deep+1);
    in
    let print_synchronized_statement e s deep =
        print_newline ();
        print_d deep;
        print_string "SynchronizedStatement";
        print_expression e (deep+1);
        print_statement s (deep+1);
    in
    let print_throw_statement e deep =
        print_newline ();
        print_d deep;
        print_string "ThrowStatement";
        print_expression e (deep+1);
    in
    let print_while_statement e s deep =
        print_newline ();
        print_d deep;
        print_string "WhileStatement";
        print_expression e (deep+1);
        print_statement s (deep+1);
    in
    match s with
        | AssertStatement (exps) -> print_assert_statement exps deep
        | Block (s) -> print_block s deep
        | BreakStatement (s) -> print_break_statement s deep
        | ContinueStatement (s) -> print_continue_statement s deep
        | DoStatement (s, e) -> print_do_statement s e deep
        | EmptyStatement -> print_empty_statement deep
        | ExpressionStatement (e) -> print_exp_statement e deep
        | ForStatement (for_init, e, for_update, s) -> print_for_statement for_init e for_update s deep
        | IfStatement (e, s1, s2) -> print_if_statement e s1 s2 deep
        | LabeledStatement (l, s) -> print_labeled_statement l s deep
        | ReturnStatement (e) -> print_return_statement e deep
        | SwitchCase  (e) -> print_switch_case e deep
        | SwitchStatement  (e, s) -> print_switch_statement e s deep
        | SynchronizedStatement (e, s) -> print_synchronized_statement e s deep
        | ThrowStatement (e) -> print_throw_statement e deep
        | WhileStatement (e, s) -> print_while_statement e s deep
;;
(*
let rec print_bodyDeclaration bd deep =
    let print_string_deep a deep =
        print_newline ();
        print_d deep;
        print_string a
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
(**)
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

let print_packageDeclaration p deep =
    match p with
        | PackageDeclaration_(annotations, name) ->
            print_string_deep "PackageDeclaration" deep;
            print_string_deep "annotations" deep;
            print_string_deep "name" deep
;;

let print_importDeclaration i deep =
    match i with
        | ImportDeclaration_(static, name, import_all) ->
            print_string_deep "ImportDeclaration" deep;
            print_string_deep (string_of_bool static) (deep + 1);
            print_string_deep "name" (deep + 1);
            print_string_deep (string_of_bool import_all) (deep + 1)
;;

let print_bodyDeclaration bd deep =
    match bd with
        | ClassDeclaration(cm, i, tp, s, it, cb) ->
            print_string_deep "ClassDeclaration" deep;
            print_string_deep "ClassModifier" (deep + 1);
            print_string_deep i (deep + 1);
            print_string_deep "TypeParameter" (deep + 1);
            print_string_deep "super" (deep + 1);
            print_string_deep "Interfaces" (deep + 1);
            print_string_deep "ClassBody" (deep + 1)
;;
            

let print_compilationUnit cu deep =
    match cu with
        | CompilationUnit_(p, i, t) ->
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
          | CompilationUnit (cu) -> print_compilationUnit cu (deep + 1)
    in
    print_ast_rec ast 0
;;

(*
let testval = Tree("name1", [Tree("name2", [Leaf("leaf1") ; Leaf("leaf2")]) ; Leaf("leaf3") ; Treeopt("tree1", [Some (Leaf("leaf4")) ; None])]);;
print_ast testval ;;
*)
