%{
    open Ast
%}

%start array_initializer
%type < Ast.expression > array_initializer

%%

array_initializer:
  | L_BRACE vi=variable_initializers? R_BRACE { ArrayInitializer(vi) }

variable_initializers:
  | vi=variable_initializer { [vi] }
  | vi=variable_initializer COMMA vis=variable_initializers { vi::vis }
  | vi=variable_initializer COMMA { [vi] }