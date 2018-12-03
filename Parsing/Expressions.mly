%start expression
%type < string > expression
%%

expression:
  | EOF { "" }