%type < Ast.expression > name
%%

(* 6.5 *)
%public name:
  | n=name PERIOD i=identifier { match n with
                                  | ExpressionName(QualifiedName(n1, n2) as qName)  -> ExpressionName(QualifiedName(qName, SimpleName(i)))
                                  | ExpressionName(SimpleName(n) as sName) -> ExpressionName(QualifiedName(sName, SimpleName(i)))
                               }
  | i=identifier { ExpressionName(SimpleName(i)) }
