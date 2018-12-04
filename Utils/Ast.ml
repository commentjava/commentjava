type ast = 
    Tree of string * (ast list)
  | Leaf of string

let print_ast ast =
    let rec print_d d =
        if d <= 0 then
            ()
        else
            begin
                print_string "|";
                print_d (d - 1)
            end
    in
    let print_elt name deep =
            print_d deep;
            print_string "-";
            print_string name;
            print_newline ()
    in
    let rec print_ast_rec ast deep =
        let rec print_list list_to_print deep =
            match list_to_print with
                | [] -> ()
                | x::m ->
                    print_ast_rec x deep;
                    print_list m deep
        in
        match ast with
          | Tree (name, astlist) -> 
            print_elt name deep;
            print_list astlist (deep + 1)
          | Leaf (name) ->
            print_elt name deep
    in
    print_ast_rec ast 0
;;
(*
let testval = Tree("name1", [Tree("name2", [Leaf("leaf1") ; Leaf("leaf2")]) ; Leaf("leaf3")]);;
print_ast testval ;;
*)
