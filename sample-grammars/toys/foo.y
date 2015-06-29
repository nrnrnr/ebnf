%start type
%token '%'
%token '('
%token ')'
%token '*'
%token '+'
%token ','
%token '-'
%token '.'
%token '/'
%token ':'
%token '<'
%token '='
%token '>'
%token '['
%token ']'
%token '{'
%token '|'
%token '}'
%token ALPHA
%token AM
%token AND
%token BIT
%token BITS
%token CHOICE
%token DATE
%token DIGIT
%token DOTS
%token FLOAT
%token FOR
%token FROM
%token GE
%token GETS
%token INT
%token Ident
%token LE
%token LOWEST
%token MOD
%token NE
%token NOT
%token NUMERAL
%token Numerals
%token OF
%token OPTIONAL
%token OR
%token OTHERS
%token PERCENT
%token PM
%token SELECTION
%token SET
%token STRING
%token String
%token TABLE
%token TELNO
%token TIME
%token TYPE
%token Telno
%token VECTOR
%left LOWEST
%left '|'
%left 
%nonassoc GETS
%left AND
%left OR
%nonassoc NOT
%nonassoc LE NE '<' GE '=' '>'
%left '+' '-'
%left '*' MOD '/'
%left ')' '[' '{' ']' '}' '.' '('
%nonassoc AM '%' PM PERCENT
%left ':'
%%
type : typelit { $0 = typelit_type($1); }
     | qualid { $0 = qualid_type($1); }
     ;
typelit : '{' opt_type_tag enumids opt_comma '}' { $0 = enum_type($2, $3, $4); }
        | '{' opt_type_tag fieldbindings opt_comma '}' { $0 = struct_type($2, $3, $4); }
        | BITS constexp FOR type { $0 = bits_type($2, $4); }
        | OPTIONAL type { $0 = optional_type($2); }
        | CHOICE opt_bar choices %prec LOWEST { $0 = choice_type($2, $3); }
        | string_element STRING opt_stringrange { $0 = string_type($1, $3); }
        | TELNO { $0 = $1; }
        | '[' opt_type_tag ranges ']' { $0 = subset_type($2, $3); }
        | SET OF type { $0 = set_type($3); }
        | TABLE opt_stringrange OF type { $0 = table_type($2, $4); }
        | SELECTION FROM type { $0 = selection_type($3); }
        | VECTOR OF type { $0 = vector_type_1($3); }
        | VECTOR OF '(' types ')' { $0 = vector_type_list($4); }
        | INT { $0 = $1; }
        | FLOAT { $0 = $1; }
        | DATE { $0 = $1; }
        | TIME { $0 = $1; }
        ;
opt_type_tag : TYPE type ',' { $0 = type_tag($2); }
             | { $0 = no_type_tag(); }
             ;
enumids : enumids ',' enumid { $0 = enumids_app($1, $3); }
        | enumid { $0 = enumids_1($1); }
        ;
enumid : Ident opt_eq_const { $0 = enumid($2); }
       ;
opt_eq_const : '=' constexp { $0 = eq_const($2); }
             | { $0 = no_eq_const(); }
             ;
opt_comma : ',' { $0 = comma($1); }
          | { $0 = no_comma(); }
          ;
fieldbindings : fieldbindings ',' fieldbinding { $0 = fieldbindings_app($1, $3); }
              | fieldbinding { $0 = fieldbindings_1($1); }
              ;
fieldbinding : Ident opt_star ':' type GETS constexp { $0 = field_type_default($2, $4, $6); }
             | Ident opt_star ':' type { $0 = field_type($2, $4); }
             | Ident opt_star GETS constexp { $0 = field_default($2, $4); }
             ;
opt_star : '*' { $0 = star($1); }
         | { $0 = no_star(); }
         ;
choices : choices '|' choice { $0 = choices_app($1, $3); }
        | choice { $0 = choices_1($1); }
        ;
choice : Ident opt_eq_const OF type { $0 = choice($2, $4); }
       ;
opt_bar : '|' { $0 = bar($1); }
        | { $0 = no_bar(); }
        ;
string_element : BIT { $0 = $1; }
               | NUMERAL { $0 = $1; }
               | ALPHA { $0 = $1; }
               | DIGIT { $0 = $1; }
               ;
opt_stringrange : '[' stringrange ']' { $0 = stringrange($2); }
                | { $0 = no_stringrange(); }
                ;
stringrange : constexp { $0 = max_length($1); }
            | constexp DOTS constexp { $0 = range_length($1, $3); }
            | '=' constexp { $0 = exact_length($2); }
            ;
ranges : ranges ',' range { $0 = ranges_app($1, $3); }
       | range { $0 = ranges_1($1); }
       ;
range : constexp DOTS constexp { $0 = range_pair($1, $3); }
      | constexp { $0 = range_single($1); }
      | OTHERS { $0 = range_others($1); }
      ;
types : types ',' type { $0 = types_app($1, $3); }
      | type { $0 = types_1($1); }
      ;
exp : exp OR exp { $0 = binop_exp($1, $2, $3); }
    | exp AND exp { $0 = binop_exp($1, $2, $3); }
    | NOT exp { $0 = preop_exp($1, $2); }
    | exp relop exp %prec '<' { $0 = binop_exp($1, $2, $3); }
    | exp addop exp %prec '+' { $0 = binop_exp($1, $2, $3); }
    | exp mulop exp %prec '*' { $0 = binop_exp($1, $2, $3); }
    | exp ':' exp { $0 = binop_exp($1, $2, $3); }
    | exp_1 exp %prec '+' { $0 = preop_exp($1, $2); }
    | exp percent %prec '%' { $0 = postop_exp($1, $2); }
    | exp ampm %prec AM { $0 = postop_exp($1, $2); }
    | '(' exp ')' { $0 = $2; }
    | designator
    | designator GETS exp { $0 = binop_exp($1, $2, $3); }
    | literal
    | exp '(' opt_actuals ')' { $0 = call_exp($1, $3); }
    ;
exp_1 : '+' { $0 = $1; }
      | '-' { $0 = $1; }
      ;
relop : '<' { $0 = $1; }
      | LE { $0 = $1; }
      | '=' { $0 = $1; }
      | NE { $0 = $1; }
      | '>' { $0 = $1; }
      | GE { $0 = $1; }
      ;
addop : '+' { $0 = $1; }
      | '-' { $0 = $1; }
      ;
mulop : '*' { $0 = $1; }
      | '/' { $0 = $1; }
      | MOD { $0 = $1; }
      ;
percent : PERCENT { $0 = $1; }
        | '%' { $0 = $1; }
        ;
ampm : AM { $0 = $1; }
     | PM { $0 = $1; }
     ;
constexp : exp { $0 = constant_exp($1); }
         ;
designator : Ident { $0 = ident_exp($1); }
           | exp '.' Ident { $0 = binop_exp($1, $2, $3); }
           | exp '[' exps ']' { $0 = array_exp($1, $3); }
           ;
exps : exps ',' exp { $0 = exps_app($1, $3); }
     | exp { $0 = exps_1($1); }
     ;
opt_actuals : actuals { $0 = actuals($1); }
            | { $0 = no_actuals(); }
            ;
actuals : actuals ',' actual { $0 = actuals_app($1, $3); }
        | actual { $0 = actuals_1($1); }
        ;
actual : typelit
       | exp
       ;
literal : Numerals { $0 = numerals_literal($1); }
        | String { $0 = string_literal($1); }
        | Telno { $0 = telno_literal($1); }
        ;
qualid : Ident { $0 = simple_qualid($1); }
       | qualid '.' Ident { $0 = qualified_qualid($1); }
       ;
