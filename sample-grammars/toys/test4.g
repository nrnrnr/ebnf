%left ","
%%
list : list "," list | element;
element : "a" | "b";

