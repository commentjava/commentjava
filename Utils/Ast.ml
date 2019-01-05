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
  (* | InstanceofExpression of expression * string TODOreplace string by type *)
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

type ast =
    | Tree of string * (ast list)
    | Treeopt of string * (ast option list)
    | Expression of expression
    | Statement of statement
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
;;

let print_opt_string s deep =
    match s with
        | None -> ()
        | Some str -> print_newline (); print_d deep; print_string str
;;

let rec print_expression e deep =
    let print_assignment_operator op deep =
        print_newline ();
        print_d deep;
        print_string ("Operator: " ^ string_of_operator op);
    in
    let print_assignment lfs op e deep =
        print_newline ();
        print_d deep;
        print_string "Assignment";
        print_expression lfs (deep + 1);
        print_assignment_operator op (deep + 1);
        print_expression e (deep + 1);
    in
    let print_name name deep =
        print_newline ();
        print_d deep;
        print_string "Name: ";
        print_global_name name;
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
    let print_string_literal string_ deep =
        print_newline ();
        print_d deep;
        print_string ("StringLiteral: " ^ string_);
    in
    let print_postfix_expression e op deep =
        let string_of_postfix_op op =
            match op with
                | INCREMENT -> "++"
                | DECREMENT -> "--"
        in
        print_newline ();
        print_d deep;
        print_string ("Postfix Expression");
        print_expression e (deep+1);
        print_newline ();
        print_d (deep+1);
        print_string (string_of_postfix_op op);
    in
    match e with
        | Assignment (lfs, op, e) -> print_assignment lfs op e deep
        | ExpressionName (name) -> print_name name deep
        | BooleanLiteral (bool_) -> print_bool_literal bool_ deep
        | CharacterLiteral (char_) -> print_char_literal char_ deep
        | NullLiteral -> print_null_literal deep
        | NumberLiteral (number) -> print_number_literal number deep
        | StringLiteral (string_) -> print_string_literal string_ deep
        | PostfixExpression (e, op) -> print_postfix_expression e op deep
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
    print_ast_rec ast 0
;;

(*
let testval = Tree("name1", [Tree("name2", [Leaf("leaf1") ; Leaf("leaf2")]) ; Leaf("leaf3") ; Treeopt("tree1", [Some (Leaf("leaf4")) ; None])]);;
print_ast testval ;;
*)
