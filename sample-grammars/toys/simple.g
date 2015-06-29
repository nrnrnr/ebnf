%term IDENT (string)
%term INT (int)
%%
start: noise "number" INT count /* ii3+ii4 */;
count: {IDENT} /* length ii1 */;
noise: "this" "is" "noise" /*()*/;

