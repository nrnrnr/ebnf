%term IDENT (string)
%term INT (int)
%%
stuff : list | {INT} /* map makestring ii1 */ | maybe IDENT ":=" stuff /* ii2 :: ii4 */
      ;
list : "alphabetically" {"a" | "b"};
maybe : ["perhaps"];


