%{
    open Ast
%}

%start array_initializer
%type < Ast.expression > array_initializer

%%

array_initializer:
  | L_BRACE vi=variable_initializers? COMMA? R_BRACE { ArrayInitializer(vi) }

variable_initializers:
  | i=variable_initializer { [i] }
  | vi=variable_initializers COMMA i=variable_initializer { vi @ [i] }

