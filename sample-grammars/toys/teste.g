%right ":="
%left "+" "-"
%left "*" "/"
%term IDENT
%term INT
%%
start : {exp /* write(ii1) */};
exp : "(" exp ")"  /* ii2 */ 
    | exp "+" exp  /* ecat(ii1, ii3, ii2) */
    | exp "-" exp  /* ecat(ii1, ii3, ii2) */
    | exp "*" exp  /* ecat(ii1, ii3, ii2) */
    | exp "/" exp  /* ecat(ii1, ii3, ii2) */
    | exp ":=" exp /* ecat(ii1, ii3, ii2) */
    | IDENT | INT
    ;
%%
procedure main()
  lex()
  P_start()
end

link commafy, ebnflex, gtoks
procedure ecat(L[])
  return commaseparate(L, " ")
end

