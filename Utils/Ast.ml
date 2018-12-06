type ast = 
    Tree of string * (ast list)
  | Treeopt of string * (ast option list)
  | Leaf of string

let print_ast ast =
    let rec print_d d =
        if d <= 0 then
            ()
        else
            begin
                print_string "| ";
                print_d (d - 1)
            end
    in
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
    in
    print_ast_rec ast 0
;;
(*
let testval = Tree("name1", [Tree("name2", [Leaf("leaf1") ; Leaf("leaf2")]) ; Leaf("leaf3") ; Treeopt("tree1", [Some (Leaf("leaf4")) ; None])]);;
print_ast testval ;;
*)
