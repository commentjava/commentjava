%%

(* 6.5 *)
%public package_name:
  | package_name PERIOD identifier { Tree("package_name", [$1; Leaf($3)]) }
  | identifier { Tree("package_name", [Leaf($1)]) }

%public type_name:
  | package_or_type_name { Tree("type_name", [$1]) }

%public expression_name:
  | en=expression_name PERIOD i=identifier { match en with 
                                              | ExpressionName(SimpleName (n)) -> ExpressionName(QualifiedName([n] @ [i]))
                                              | ExpressionName(QualifiedName (ln)) -> ExpressionName(QualifiedName(ln @ [i]))
                                           }
  | i=identifier { ExpressionName(SimpleName(i)) }
  
%public method_name:
  | en=method_name PERIOD i=identifier { match en with 
                                              | Expression(ExpressionName(SimpleName (n))) -> Expression(ExpressionName(QualifiedName([n] @ [i])))
                                              | Expression(ExpressionName(QualifiedName (ln))) -> Expression(ExpressionName(QualifiedName(ln @ [i])))
                                       }
  | i=identifier { Expression(ExpressionName(SimpleName(i))) }

%public package_or_type_name:
  | package_or_type_name PERIOD identifier { Tree("package_or_type_name", [$1; Leaf($3)]) }
  | identifier { Tree("package_or_type_name", [Leaf($1)]) }

%public class_name:
  | en=expression_name { en }
