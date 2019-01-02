%%

(* 6.5 *)
%public package_name:
  | identifier { Tree("package_name", [Leaf($1)]) }
  | package_name PERIOD identifier { Tree("package_name", [$1; Leaf($3)]) }

%public type_name:
  | identifier { Tree("type_name", [Leaf($1)]) }
  | package_or_type_name PERIOD identifier { Tree("type_name", [$1; Leaf($3)])  }

%public expression_name:
  | identifier { ExpressionName(SimpleName($1)) }
  | ambiguous_name PERIOD identifier { ExpressionName(QualifiedName($1 @ [$3])) }

%public method_name:
  | identifier { Tree("method_name", [Leaf($1)]) }
  | ambiguous_name PERIOD identifier { Tree("method_name", [$1; Leaf($3)]) }

package_or_type_name:
  | identifier { Tree("package_or_type_name", [Leaf($1)]) }
  | package_or_type_name PERIOD identifier { Tree("package_or_type_name", [$1; Leaf($3)]) }

ambiguous_name:
  | identifier { [$1] }
  | ambiguous_name PERIOD identifier { $1 @ [$3] }
