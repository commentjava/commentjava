%%

(* 6.5 *)
%public package_name:
  | package_name PERIOD identifier { Tree("package_name", [$1; Leaf($3)]) }
  | identifier { Tree("package_name", [Leaf($1)]) }

%public type_name:
  | package_or_type_name { Tree("type_name", [$1]) }

%public expression_name:
  | ambiguous_name PERIOD identifier { ExpressionName(QualifiedName($1 @ [$3])) }
  | identifier { ExpressionName(SimpleName($1)) }

%public method_name:
  | ambiguous_name PERIOD identifier { Tree("method_name", [$1; Leaf($3)]) }
  | identifier { Tree("method_name", [Leaf($1)]) }

package_or_type_name:
  | package_or_type_name PERIOD identifier { Tree("package_or_type_name", [$1; Leaf($3)]) }
  | identifier { Tree("package_or_type_name", [Leaf($1)]) }

ambiguous_name:
  | ambiguous_name PERIOD identifier { $1 @ [$3] }
  | identifier { [$1] }
