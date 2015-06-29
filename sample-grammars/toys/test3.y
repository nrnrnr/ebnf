%token IF ELSE THEN SKIP EXP
%%
s : SKIP | IF e THEN s | IF e THEN s ELSE s;
e : EXP;

