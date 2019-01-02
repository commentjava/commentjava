type names =
    SimpleName of string
  | QualifiedName of string list

type assignment_operator =
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

type prefix_operator =
    INCREMENT
  | DECREMENT
  | PLUS
  | MINUS
  | COMPLEMENT
  | NOT

type postfix_operator =
    INCREMENT
  | DECREMENT

type expression =
    (* Annotation *)
  (* | ArrayAccess *)
  (* | ArrayCreation *)
  (* | ArrayInitializer *)
  | Assignment of expression * assignment_operator * expression
  | BooleanLiteral of string
  (* | CastExpression *)
  | CharacterLiteral of string
  (* | ClassInstanceCreation *)
  (* | ConditionalExpression of expression * expression * expression *)
  (* | CreationReference *)
  (* | ExpressionMethodReference *)
  (* | FieldAccess *)
  (* | InfixExpression *)
  (* | InstanceofExpression of expression * string TODO: replace string by type *)
  (* | LambdaExpression *)
  (* | MethodInvocation *)
  (* | MethodReference *)
  | ExpressionName of names
  | NullLiteral
  | NumberLiteral of string
  | ParenthesizedExpression of expression
  | PostfixExpression of expression * postfix_operator
  | PrefixExpression of expression * postfix_operator
  | StringLiteral of string
  (* | SuperFieldAccess *)
  (* | SuperMethodInvocation *)
  (* | SuperMethodReference *)
  (* | ThisExpression *)
  (* | TypeLiteral *)
  (* | TypeMethodReference *)
  (* | VariableDeclarationExpression *)

  type ast =
    Tree of string * (ast list)
    | Treeopt of string * (ast option list)
    | Expression of expression
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

let print_name name =
    let rec format_qualified_name names =
        match names with
            [] -> ""
            | e::l -> e ^ "." ^ format_qualified_name l
    in
    match name with
        | SimpleName (name) -> print_string ("SimpleName: " ^ name)
        | QualifiedName (names) -> print_string ("QualifiedName: " ^ format_qualified_name names)
;;

let operator_to_string op =
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
;;

let rec print_expression e deep =
    let print_assignment_operator op deep =
        print_d deep;
        print_string ("Operator: " ^ operator_to_string op);
        print_newline ()
    in
    let print_assignment lfs op e deep =
        print_d deep;
        print_string "Assignment: ";
        print_newline ();
        print_expression lfs (deep + 1);
        print_assignment_operator op (deep + 1);
        print_expression e (deep + 1);
        print_newline ()
    in
    let print_number_literal number deep =
        print_d deep;
        print_string ("NumberLiteral: " ^ number);
        print_newline ()
    in
    let print_string_literal string_ deep =
        print_d deep;
        print_string ("StringLiteral: " ^ string_);
        print_newline ()
    in
    let print_char_literal char_ deep =
        print_d deep;
        print_string ("CharacterLiteral: " ^ char_);
        print_newline ()
    in
    match e with
        | Assignment (lfs, op, e) -> print_assignment lfs op e deep
        | ExpressionName (name) -> print_d deep; print_name name; print_newline ()
        | NullLiteral -> print_d deep; print_string "null"; print_newline ()
        | NumberLiteral (number) -> print_number_literal number deep
        | StringLiteral (string_) -> print_string_literal string_ deep
        | CharacterLiteral (char_) -> print_char_literal char_ deep
;;

let print_ast ast =

    let print_elt name deep =
            print_d deep;
            print_string "\\";
            print_string name;
            print_newline ()
    in
    let print_leaf name deep =
            print_d deep;
            print_string name;
            print_newline ()
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
    in
    print_ast_rec ast 0
;;
(*
let testval = Tree("name1", [Tree("name2", [Leaf("leaf1") ; Leaf("leaf2")]) ; Leaf("leaf3") ; Treeopt("tree1", [Some (Leaf("leaf4")) ; None])]);;
print_ast testval ;;
*)
