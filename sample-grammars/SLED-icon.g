%term IDENT
%term INT
%term CASELINE
%term CODELINE
%term NEWLINE
%term WHITESPACE
%%
# EBNF grammar for toolkit specification language
Parsers   : "bogus spec marker" Spec | "bogus code marker" CodeFile;
Spec      : { Fieldspec | Patterns | Constructors | Placeholder 
            | FetchSpec | PCSpec   | RelocSpec    | AsmSpec 
            | BitSpec   
            } ;
PCSpec : "pc_unit_bits" INT /* if ii2 > 0 then pc_unit_bits := ii2 
                               else error("pc_unit_bits must be positive") */;
Fieldspec : "fields" "of" Ident "(" INT ")" 
            { Ident INT [":" INT /*ii2*/]  /* newfield(ii1, ii2, (\ii3 | ii2)+1) */ }
            /* if ii5 % 8 ~= 0 then
                  error("element size ", ii5, " is not a multiple of 8 bits.")
               if /bit_zero_is_lsb then {
                 if !ii7 then bit_numbering_used := 1
                 every f := !ii7 do {
                   f.hi := ii5 - f.hi
                   f.lo := ii5 - f.lo
                   f.lo :=: f.hi
                 }
               }
               (/symtab[ii3] := equivclass(ii3, ii7, ii5)) | 
                         deferror(type(symtab[ii3]) || " ", image(ii3))
               put(equivclasses, symtab[ii3])
               every (!ii7).class := symtab[ii3] */
          ;
BitSpec      : "bit" Zero "is" Significance Significant
                 /* xxx := if ii4 == "least" then 1 else &null 
                    if xxx ~=== bit_zero_is_lsb then 
                      if \bit_numbering_set then
                        warning("Multiple inconsistent bit numberings --- hope you really mean it!")
                      if \bit_numbering_used then
                        warning("You change the bit numbering, but you've already used the old numbering" ||
                                "... seems like a crazy idea, but I'll do it")
                    bit_zero_is_lsb := xxx
                    bit_numbering_set := 1
                  */
             ;
Zero         : INT /* ii1 = 0 | error("expected `0'") */;
Significance : Ident 
                /* ii1 == ("most"|"least") | error("expected `most' or `least'") */;
Significant  : Ident /* ii1 == "significant" | error("expected `significant'") */;
Placeholder : "placeholder" "for" Ident "is" Pattern 
              /* class := lookuptype(ii3, "equivclass")
                 (/class.holder := pnf(ii5, globals)) |
                 error("Placeholder for ", ii3, " is already defined") 
                 if pattern_length(class.holder) ~= class.size then
                   error("Length of placeholder `", patimage(class.holder), 
                     "' \nfor ", class.name, " does not match class size ", class.size)
               */;
Fieldspec         : "fieldinfo" { Fieldinfo } ;
Fieldinfo         : IdentBinding "is" "[" { Fielditem } "]" 
                    /*  every fieldinfo(lookuptype(!ii1, "field"), !ii4) */;
IdentBinding      : Ident /* [ii1] */ | "[" { Ident } "]";
Fielditem         : Ident  | SparseFieldNames | FieldNameList ;
SparseFieldNames  : "sparse" "[" FieldNameBindings "]" /* sparse_name_table(ii3) */ ;
FieldNameBindings : FieldNameBinding {"," FieldNameBinding}  /* push(ii2, ii1) */;
FieldNameBinding  : FieldName "=" Integer /* enumbinding(ii1, ii3) */ ;
FieldNameList     : "names" "[" {FieldName} "]" /* full_name_table(ii3) */ ;
FieldName         : (String | Ident) /* {if member(operands_and_ids, ii1) then {
                                           every (if not member(warned_literals, ii1) then warning else verbose)(
                                             ii1 || " is used as a field-name literal and an operand or id..." |
                                             "  the literal takes priority in field bindings"
                                           )
                                           insert(warned_literals, ii1)
                                         } else
                                           insert(fieldname_literals, ii1)}; ii1 */;
SparseNames : "sparse" "[" Bindings "]" /* sparse_name_table(ii3) */ ;
Bindings    : Binding {"," Binding}  /* push(ii2, ii1) */;
Binding     : (String|Ident) "=" Integer /* enumbinding(ii1, ii3) */ ;
DenseNames  : "names" "[" {String|Ident} "]" /* full_name_table(ii3) */ ;
NameTable   : SparseNames | DenseNames;
Fieldspec : "wordsize" INT /* wordsize := ii2 */;
Patterns  : "patterns" { PatBinding };
PatBinding: (Ident /* patlhs := ii1 */) "is" BindingRHS
          | "[" {Ident} "]" "is" Pattern /* patbinding(ii2, ii5) */
          ;
BindingRHS: Pattern /* patbinding(patlhs, ii1) */
          | "any" "of" "[" {Ident} "]" "," "which" "is" Pattern
              /* patbinding(copy(ii4), ii9)
                 l := []
                 every put(l, Pident("_" ~== !ii4))
                 patbinding(patlhs, Por(l)) */
          ;
Pattern   : Disjunct      { "|" Disjunct }      /* Por(push(ii2, ii1))   */ ;
Disjunct  : Sequent       { ";" Sequent 
                          | ":" Sequent /* colon_mark(ii2) */
                          }      /* Pseq(colons_to_labels(push(ii2, ii1)))  */ ;
Sequent   : Conjunct      { "&" Conjunct }      /* Pand(push(ii2, ii1))  */ ;

Conjunct  : ["..."] DotsR    /* if \ii1 then Pseq([dots_pattern(),ii2]) else ii2 */;
DotsR     : Atomic ["..."]   /* if \ii2 then Pseq([ii1,dots_pattern()]) else ii1 */ ;

Atomic    : (Ident /* atomicid := ii1 */)
            (                                    /* Pident(atomicid) */
            | Relop (Generator | Expr)           /* Pcon(atomicid, ii1, ii2) */
            | {"^" Opname} "(" ConstructorArgs ")" 
                  /* Papp(push(ii1, \symtab[atomicid] | atomicid), ii3) */
            ) /* ii2 */
          | String {"^" Opname} "(" ConstructorArgs ")"
                  /* Papp(push(ii2, ii1), ii4) */
          | "(" Pattern ")"
          | "[" {Ident /* Pident(ii1) */} "]"   /* Plist(ii2) */
          ;
ConstructorArgs : AppExpr {"," AppExpr} /* push(ii2, ii1) */
                | /* [] */
                ;
Relop     : "=" | "!=" | "<=" | ">=" | "<" | ">" ;
Generator : "{" Integer "to" Integer [ "columns" INT /* ii2 */ ] "}"  
                                /* Gfor(ii2, ii4+1, \ii5 | 1) */
          | "[" { Integer } "]"     /* Glist(ii2) */
          ;
Atomic : "epsilon"   /* epsilon() */
       | "some" Ident /* wildcard(lookuptype(ii2, "equivclass")) */
       ;
Constructors : "constructors" (/*see_newline()*/) { Constructor /* see_newline() */} ;
Constructor  : Opcode Operands [ ":" ConstType ] NLBranches
                 /* note_constructor(ii1, ii2, \ii3 | instructionctype, ii4) */ 
               ;
Operands : SeeWhite {Operand} StopWhite /* process_operands(ii2) */;
NLBranches   : Branches
             | NEWLINE (Branches | /* [ [ [], &null ] ] */)
             ;
Branches     : SingleBranch /* [ii1] */
             | WhenBranch {WhenBranch | OtherwiseBranch}
                /* push(ii2, ii1); ii2 */
             ;
SingleBranch : "{" Equations "}" [ "is" Pattern ] /* [ ii2, \ii4 | &null] */ 
             | "is" Pattern    /* [ [] , ii2 ] */ 
             ;
WhenBranch      : "when" "{" Equations "}" "is" Pattern /* [ii3, ii6] */;
OtherwiseBranch : "otherwise" "is" Pattern /* [[], ii3] */;
Opcode    : Opname { "^" Opname }  /* push(ii2, ii1) */ ;
Opname    : Ident /* \symtab[ii1] | ii1 */ | String ;
Operand      : (Ident | AddressAsIdent) ["!"] /* {if member(fieldname_literals, ii1) then {
                                                    every (if not member(warned_literals, ii1) then warning else verbose)(
                                                      ii1 || " is used as a field-name literal and an operand or id..." |
                                                      "  the literal takes priority in field bindings"
                                                    )
                                                    insert(warned_literals, ii1)
                                                  } else
                                                    insert(operands_and_ids, ii1)};
                              name_to_input(ii1, ii2) */
             | (Literal | GlobOperator | White) /* literal(ii1) */
             ;
AddressAsIdent : (/*NEWLINEVISION := 1*/) "address" SeeWhite /* ii2 */;
  # reserved word automatically causes newlines to be ignored, so fix it
Literal      : String | Integer /* string(ii1) */ | Relop | "=>" 
             | "[" | "]" | "(" | ")" | "+" | "-" | "/" 
             | "&" | "@" | "#" | "%" | ";" | "|" 
             ;
GlobOperator : "*" | "$" | ","
             ;
ConstType : (Ident | "address")
            /* 1(/symtab[ii1] := t := constype(ii1, set()), put(all_types ,t)) |
                     lookuptype(ii1, "constype")  */;
RelocSpec : "relocatable" Ident {Ident} 
                /* if /no_reloc then every make_relocatable(ii2 | !ii3) */;
Constructors : "discard" {Opcode} /* every discard_cons_named(explode_names(!ii2)) */
             | "keep"    {Opcode} 
                  /* s := set()
                     every insert(s, is_constructor(explode_names(!ii2), warning))
                     every k := key(constructors) do
                       if not member(s, constructors[k]) then
                         delete(constructors, k)
                   */
             ;
Equations : [Equation { "," Equation } /* push(ii2, ii1) */] /* \ii1 | [] */ ;
Equation  : Expr Relop Expr /* eqn(ii1, ii2, ii3) */ ;
Expr      : AppExpr /* if has_app_or_literal(ii1) then 
                         error("Application or literal string not legal")
                       else ii1 */
          ;
AppExpr   : Term { AOp Term }   
            /* every t := !ii2 do ii1 := binop(ii1, t[1], t[2]); ii1 */ ; 
Term      : Factor { Mop Factor }     
            /* every f := !ii2 do ii1 := binop(ii1, f[1], f[2]); ii1 */ ; 
AOp       : "+" | "-" ; 
Mop       : "*" | "/" ; 
Factor    : Integer
          | String /* literal(ii1) */   # legal only in constructor apps
          | "_" /* fresh_variable("_") */
          | (IDENT|"address") ( [ Bitrange ] [ "!" ]       /* SyntaxRange(ii1, ii2) */
                              | "(" Args ")" 
                              ) 
             /* if member(fieldname_literals, ii1) then {
                  every (if not member(warned_literals, ii1) then warning else verbose)(
                    ii1 || " is used as a field-name literal and an operand or id..." |
                    "  the literal takes priority in field bindings"
                  )
                  insert(warned_literals, ii1)
                } else
                  insert(operands_and_ids, ii1)
                if type(ii2) == "SyntaxRange" then mkfactor(ii1, ii2.bits, ii2.bang) 
                else Eapp(ii1, ii2) */
          | "(" AppExpr ")"        
          | "-" Factor /* binop(0, ii1, ii2) */
          ;
Bitrange  : "@" "[" INT [":" INT /*ii2*/] "]" /* [ii3, (\ii4|ii3)+1] */ ;
Args      : AppExpr {"," AppExpr} /* push(ii2, ii1) */
          | /* [] */
          ;

CodeFile      : (/*codeheader := arm(filename, lineno)*/)
                ({CODELINE} /* codeheader.code := ii1 */)
                { Casestmt {CODELINE} /* ii1.trailer.code := ii2 ; ii1 */ } 
                /* codeheader.original := codeheader; matching_stmts := ii3 */;
Casestmt      : CASELINE { Casearm } [ElseArm] "endmatch" 
               /* x := matching_stmt(ii2, ii1, succptr) ; put(x.arms, \ii3)
                  x.trailer := arm(filename, lineno); x */
              ;
Casearm       : ("|" /*arm(filename, lineno) */)
                Pattern OptEquations OptName "=>" {CODELINE}
                /* ii1.pattern := ii2; ii1.eqns := ii3; ii1.name := ii4; ii1.code := ii6
                   ii1.original := ii1 */;   # value is ii1
ElseArm       : ("else"           /* arm(filename, lineno, epsilon()) */) {CODELINE} 
                /* ii1.code := ii2; ii1.original := ii1 */ ;
OptName       : [ "[" Ident "]" ];
OptEquations  : [ "{" Equations "}" ];
FetchSpec : "fetch" ( INT   "using" String /* newfetch(ii1, ii3, 'a') */
                    | "any" "using" String /* newfetch(ii1, ii3, 'aw') */
                    )
          | "address" (Add "using" String          /* newfetch(ii1, ii3, 'ao') */
                      | "type" "is" String         /* newfetch(ii1, ii3, '') */
                      | "to" IntIdent "using" String     /* newfetch(ii2, ii4, 'a') */
                      )
          ;

Add      : Ident /* (ii1 == "add") | error("expected `add', `type', or `to'") */;
IntIdent : Ident /* (ii1 == "integer") | error("expected", image("integer")) */;
AsmSpec : "assembly"
   ( "operand" {IdentBinding "is" OperandSyntaxSpec
                                 /* every asmoperand(!ii1, ii3[1], ii3[2]) */}
   | "component"  {Globbing "is" GlobTarget  /* asmopcode (ii1, ii3) */}
   | "opcode"  {Globbing "is" GlobTarget  /* asmopcode (ii1, ii3, 1) */}
   | "syntax"  (/*see_newline()*/) { AsmSyntax /* see_newline() */}
   );
OperandSyntaxSpec : String ["using" OperandNameSpec ] /* [ii1, ii2] */
                  | OperandNameSpec /* ["%s", ii1] */
                  ;
OperandNameSpec   : NameTable
                  | "field" Ident /* lookuptype(ii2, "field") */
                  ;
AsmSyntax: Opcode Operands NEWLINE 
               /* every set_asmsyntax(is_constructor(explode_names(ii1), warning), ii2) */;
Globbing         : SeeWhite GlobPattern StopWhite White /* ii2 */;
GlobPattern      : {GlobAlternatives | Literal | Ident} 
                          /* number_braces(cat_adjacent_strings(ii1)) */;
GlobAlternatives : "{" GlobPattern {"," GlobPattern } "}" /* glob_any(push(ii3, ii2))*/
                 | "*"                                    /* the_glob_wildcard */
                 ;
GlobTarget       : SeeWhite GlobTargets StopWhite White /* ii2 */;
GlobTargets      : {GlobTargetSpecial | GlobTargetLiteral} 
                                                /* cat_adjacent_strings(ii1) */;
GlobTargetSpecial: "$" (Integer /* glob_dollar(ii1) */ | "$")
                 ;
GlobTargetLiteral: Literal | Ident | "*" | "{" | "}" | "," ;
Ident     : IDENT | "_" ;
String    : '"' ;
Integer   : INT | "'" /* ord(ii1) */ ;
White     : WHITESPACE;
SeeWhite  : /* see_whitespace() */;
StopWhite : /* ignore_whitespace() */;
%%
global symtab           # table of top-level symbols
global globals          # top-level environment
global equivclasses     # list of equivalence classes of fields
global constructors     # table mapping constructor name to Stype
global conslist         # list of all constructors ever defined
global instructionctype # the type of instruction constructors
global bit_zero_is_lsb  # non-null if bits are numbered with 0 == least significant
global vanishing_latent_patlabel  # should vanish
global fieldname_literals, operands_and_ids, warned_literals # find conflicts
global all_types        # list of all possible constructor types, in order
procedure init_parser()
    every symtab | constructors := table()
    globals := [symtab]
    every unchecked_fields | guaranteed_fields := set()
    every fieldname_literals | operands_and_ids | warned_literals := set()
    every conslist | equivclasses | all_types := []
    bit_zero_is_lsb := 1
    vanishing_latent_patlabel := latent_patlabel()
    wordsize := 32
    the_relocatable := relocatable()
    the_glob_wildcard := glob_wildcard()
end
procedure kept_constructors(constype)
  local thelist 
  thelist := (\constype).members | conslist
  suspend 1(c := !thelist, constructors[c.name] === c)
end
procedure newfield(name, lo, hi)
  return (/symtab[name] := field(name, lo, hi)) | deferror("Field", image(name))
end
global wordsize         # word size of the machine the application runs on
global patlhs           # hold lhs for later action
record colon_mark(x)

procedure colons_to_labels(l)
  every i := 1 to *l & type(l[i]) == "colon_mark" do {
    l[i] := l[i].x  # strip out tag
    (l[i-1] := undo_identifier_syntax(l[i-1])) |
      error("Colon must be preceded by an identifier")
  }
  return l
end

procedure undo_identifier_syntax(seq)
  if type(seq) == "Pand" & *seq.patterns = 1 & con := seq.patterns[1] &
     type(con) == "Pident"
  then
    return Plabel(con.name)
end     
global atomicid
record Por (patterns)                   # disjunction
record Pseq(patterns)                   # sequence
record Pand(patterns)                   # conjunction
record Pcon(name, relop, value)         # constraint on field
record Pident(name)                     # identifier standing for a pattern
record Plabel(name)                     # pattern label
record Papp(cons, args)                 # constructor applied to arguments
                                        #   (args are AppExprs)
record Plist(patterns)                  # list of patterns in square brackets
procedure explode_apps(opcode, args, rho)
  l := []
  every c := explode_names(opcode, rho) do {
    put(l, Papp(cons_named(c), args))
  }
  return if *l = 1 then l[1] else Por(l)
end
record Glist(values)                    
procedure Gfor(lo, hi, cols)
    local l, r
    l := []
    r := (hi - lo) / cols
    every put(l, lo + (0 to r - 1) + r * (0 to cols - 1))
    return Glist(l)
end
procedure patbinding(id, ast)
    local p
    case type(id) of {
      "string" : verbose("Pattern ", id)
      "list"   : verbose(*id, " patterns")
    }
    p := pnf(ast, globals)
    case type(p) of {
      "pattern" : insist_global_pattern(p)
      "list"    : every insist_global_pattern(!p)
    }
    case type(id) || "," || type(p) of {
        "string,pattern" : patbind(id, p, globals)
        "list,list"      : if *id = *p then
                             while patbind(get(id), get(p), globals)
                           else 
                             error("identifier list/generator length mismatch: ", 
                                   *id, " vs ", *p)
        "string,list"    : error("generator must be bound to an identifier list")
        "list,pattern"   : error("identifier list must be bound to a generator")
        default          : impossible("pattern binding")
    }
    return
end
record literal(s)       # holds string or list to be emitted literally
record input(name, meaning)                     # input name and meaning
procedure reversetrailing(c)
  local i
  suspend *&subject + 1
  i := *&subject 
  while any(c, &subject, 0 < i) do { suspend i; i -:= 1 }
end
procedure has_app_or_literal_f(e)
  return type(e) == ("Eapp"|"literal")
end

procedure has_app_or_literal(e)
  suspend expwalk(e, has_app_or_literal_f)
end
record SyntaxRange(bits, bang)
global matching_stmts, codeheader
global bit_numbering_set, bit_numbering_used
procedure process_operands(ops)
  if type(ops[-1]) == "literal" & ops[-1].s ? (white(), pos(0)) then
    pull(ops) # discard trailing white space
  l := []
  every x := !ops do
    if type(x) == "literal" & type(l[-1]) == "literal" then
      l[-1].s ||:= x.s
    else
      put(l, x)
  return l
end
procedure name_to_input(name, bang)
  i := input(name, 
    if name ? type(ct := symtab[tab(reversetrailing('0123456789_'))]) == "constype" then
      mark_ct_as_used(ct)
    else case type(x := symtab[name]) of {
      "field"       : if /bang then x else fwidth(x)
      "null"        : { \bang | 
                        warning("Use ", name, "! for signed inputs --- future versions will require it"); x }
      "relocatable" : "reloc"
      "constype"    : impossible("missed first-round search for constype")
      default       : typeerror(x, "free variable, field, or constructor input", name,
                                globals)
  })
  /bang | type(i.meaning) == ("integer"|"null"|"string") |
    typeerror(i.meaning, "field or integer (to be sign-extended)", name, globals)
  return i
end
record relocatable()    # type of a name that means relocatable address
global the_relocatable  # we only need one...
procedure make_relocatable(name)
  return (/symtab[name] := the_relocatable) | deferror("Relocatable name", image(name))
end
global used_types
procedure mark_ct_as_used(type)
  initial used_types := []
  if /type.used := lineno then {
    put(used_types, type)
    t := table()
    every m := !type.members do
      t[m.name] := m
    t := sort(t)
    type.members := []
    every put(type.members, (!t)[2])
  }
  return type
end
procedure mkfactor(ident, range, ext)
  e := ident
  if \range then {
    e := mkslice(e, range[1], range[2])
    w := e.n
  }
  if \ext then {
    /w := if type(f := symtab[ident]) == "field" then fwidth(f)
          else error("Can't sign-extend ", ident, " (not a field)")
    e := Ewiden(e, w)
  }
  return e
end
procedure newfetch(k, template, expected)
  c := ''
  template ? while tab(upto('%')) & ="%" do c := c ++ move(1)
  every x := !(expected -- c) do
    warning("expected %", x, " in template")
  return fetchtab[k] := template
end
record glob_any(alternatives, number)   # braced list of alternate globbing patterns
record glob_wildcard()
global the_glob_wildcard                # only need one value
record glob_dollar(number)              # $n on right-hand side
procedure cat_adjacent_strings(l)
  m := []
  every x := !l do 
    if type(x) == "string" & type(m[-1]) == "string" then
      m[-1] ||:= x
    else
      put(m, x)
  return m
end
