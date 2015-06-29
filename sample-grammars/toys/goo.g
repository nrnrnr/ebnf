%attribute region yes
%term STRING   (string * (int*int))
%term IDENT    (string * (int*int))
%term INT      (int)
%term CHAR     (char * (int*int))
%term NEWLINE
datatype result = RESULT
%%
Exp : Term {"+" Term} /* foldr op + ii1 ii2 */;
Term : Factor {"*" Factor} /* foldr op * ii1 ii2 */;
Factor : INT | "(" Exp ")";
%%
