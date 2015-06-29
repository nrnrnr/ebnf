#line 1403 "grammar.nw"
%term IDENT
%term INT
%term CASELINE
%term CODELINE
%term NEWLINE
%term WHITESPACE
#line 4 "grammar.nw"
%%
# EBNF grammar for toolkit specification language
#line 56 "grammar.nw"
Parsers   : "bogus spec marker" Spec | "bogus code marker" CodeFile;
#line 61 "grammar.nw"
Spec      : { Fieldspec | Patterns | Constructors | Placeholder 
            | FetchSpec | PCSpec   | RelocSpec    | AsmSpec 
            | BitSpec
            } ;
#line 74 "grammar.nw"
PCSpec : "pc_unit_bits" INT /* if ii2 > 0 then pc_unit_bits := ii2 
                               else error("pc_unit_bits must be positive") */;
#line 112 "grammar.nw"
Fieldspec : "fields" "of" Ident "(" INT ")" 
            { Ident INT ":" INT  /* newfield(ii1, ii2, ii4+1) */ }
            /* if ii5 % 8 ~= 0 then
                  error("element size ", ii5, " is not a multiple of 8 bits.")
               
#line 130 "grammar.nw"
if /bit_zero_is_lsb then {
  if !ii7 then bit_numbering_used := 1
  every f := !ii7 do {
    f.lo := ii5 - f.hi
    f.hi := ii5 - f.lo
  }
}
#line 117 "grammar.nw"
               (/symtab[ii3] := equivclass(ii3, ii7, ii5)) | 
                         deferror(type(symtab[ii3]) || " ", image(ii3))
               put(equivclasses, symtab[ii3])
               every (!ii7).class := symtab[ii3] */
          ;
#line 155 "grammar.nw"
BitSpec      : "bit" Zero "is" Significance Significant
                 /* xxx := if ii4 == "least" then 1 else &null 
                    if xxx ~=== bit_zero_is_lsb then 
                      
#line 170 "grammar.nw"
if \bit_numbering_set then
  warning("Multiple inconsistent bit numberings --- hope you really mean it!")
if \bit_numbering_used then
  warning("You change the bit numbering, but you've already used the old numbering" ||
          "... seems like a crazy idea, but I'll do it")
#line 159 "grammar.nw"
                    bit_zero_is_lsb := xxx
                    bit_numbering_set := 1
                  */
             ;
Zero         : INT /* ii1 = 0 | error("expected `0'") */;
Significance : Ident 
                /* ii1 == ("most"|"least") | error("expected `most' or `least'") */;
Significant  : Ident /* ii1 == "significant" | error("expected `significant'") */;
#line 207 "grammar.nw"
Placeholder : "placeholder" "for" Ident "is" Pattern 
              /* class := lookuptype(ii3, "equivclass")
                 (/class.holder := pnf(ii5, globals)) |
                 error("Placeholder for ", ii3, " is already defined") 
                 
#line 215 "grammar.nw"
if pattern_length(class.holder) ~= class.size then
  error("Length of placeholder `", patimage(class.holder), 
    "' \nfor ", class.name, " does not match class size ", class.size)
#line 212 "grammar.nw"
               */;
#line 239 "grammar.nw"
Fieldspec    : "fieldinfo" { Fieldinfo } ;
Fieldinfo    : IdentBinding "is" "[" { Fielditem } "]" 
               /*  every fieldinfo(lookuptype(!ii1, "field"), !ii4) */;
IdentBinding : Ident /* [ii1] */ | "[" { Ident } "]";
Fielditem    : Ident  | Bindlist | Namelist ;
Bindlist     : "sparse" "[" Bindings "]" /* sparse_name_table(ii3) */ ;
Bindings     : Binding {"," Binding}  /* push(ii2, ii1) */;
Binding      : (Ident|String) "=" Integer /* enumbinding(ii1, ii3) */ ;
Namelist     : "names" "[" { Ident | String } "]" /* full_name_table(ii3) */ ;
#line 324 "grammar.nw"
Fieldspec : "wordsize" INT /* wordsize := ii2 */;
#line 347 "grammar.nw"
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
#line 401 "grammar.nw"
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
                  /* explode_apps(push(ii1, \symtab[atomicid] | atomicid), ii3) */
            ) /* ii2 */
          | String {"^" Opname} "(" ConstructorArgs ")"
                  /* explode_apps(push(ii2, ii1), ii4) */
          | "(" Pattern ")"
          | "[" {Ident /* Pident(ii1) */} "]"   /* Plist(ii2) */
          ;
ConstructorArgs : AppExpr {"," AppExpr} /* push(ii2, ii1) */
                | /* [] */
                ;
#line 583 "grammar.nw"
Relop     : "=" | "!=" | "<=" | ">=" | "<" | ">" ;
Generator : "{" Integer "to" Integer [ "columns" INT /* ii2 */ ] "}"  
                                /* Gfor(ii2, ii4+1, \ii5 | 1) */
          | "[" { Integer } "]"     /* Glist(ii2) */
          ;
#line 613 "grammar.nw"
Atomic : "epsilon"   /* epsilon() */
       | "some" Ident /* wildcard(lookuptype(ii2, "equivclass")) */
       ;
#line 695 "grammar.nw"
Constructors : "constructors" (/*see_newline()*/) { Constructor /* see_newline() */} ;
Constructor  : Opcode Operands [ ":" ConstType ] NLBranches
                 /* note_constructor(ii1, ii2, \ii3 | instructionctype, ii4) */ 
               ;
Operands : SeeWhite {Operand} StopWhite /* process_operands(ii2) */;
#line 734 "grammar.nw"
NLBranches   : Branches
             | NEWLINE (Branches | /* [ [ [], &null ] ] */)
             ;
Branches     : SingleBranch /* [ii1] */
             | WhenBranch {WhenBranch} [OtherwiseBranch]
                /* push(ii2, ii1); put(ii2, \ii3); ii2 */
             ;
SingleBranch    : [ "{" Equations "}" ] "is" Pattern /* [\ii1 | [], ii3] */ ;
WhenBranch      : "when" "{" Equations "}" "is" Pattern /* [ii3, ii6] */;
OtherwiseBranch : "otherwise" "is" Pattern /* [[], ii3] */;
#line 786 "grammar.nw"
Opcode    : Opname { "^" Opname }  /* push(ii2, ii1) */ ;
Opname    : Ident /* \symtab[ii1] | ii1 */ | String ;
#line 857 "grammar.nw"
Operand      : Ident ["!"] /* name_to_input(ii1, ii2) */
             | (Literal | GlobOperator | White) /* literal(ii1) */
             ;
Literal      : String | Integer /* string(ii1) */ | Relop | "=>" 
             | "[" | "]" | "(" | ")" | "+" | "-" | "/" 
             | "&" | "@" | "#" | "%" | ";" | "|" 
             ;
GlobOperator : "*" | "$" | ","
             ;
#line 887 "grammar.nw"
ConstType : Ident /* (/symtab[ii1] := constype(ii1, set())) | 
                     lookuptype(ii1, "constype")  */;
#line 1005 "grammar.nw"
RelocSpec : "relocatable" Ident {Ident} /* every make_relocatable(ii2 | !ii3) */;
#line 1052 "grammar.nw"
Constructors : "discard" {Opcode} /* every discard_cons_named(explode_names(!ii2)) */
             | "keep"    {Opcode} 
                  /* s := set()
                     every insert(s, cons_named(explode_names(!ii2)))
                     every k := key(constructors) do
                       if not member(s, constructors[k]) then
                         delete(constructors, k)
                   */
             ;
#line 1077 "grammar.nw"
Equations : [Equation { "," Equation } /* push(ii2, ii1) */] /* \ii1 | [] */ ;
Equation  : Expr Relop Expr /* eqn(ii1, ii2, ii3) */ ;
#line 1107 "grammar.nw"
Expr      : AppExpr /* if has_app_or_literal(ii1) then 
                         error("Application or literal string not legal")
                       else ii1 */
          ;
#line 1121 "grammar.nw"
AppExpr   : Term { AOp Term }   
            /* every t := !ii2 do ii1 := binop(ii1, t[1], t[2]); ii1 */ ; 
Term      : Factor { Mop Factor }     
            /* every f := !ii2 do ii1 := binop(ii1, f[1], f[2]); ii1 */ ; 
AOp       : "+" | "-" ; 
Mop       : "*" | "/" ; 
Factor    : Integer
          | String /* literal(ii1) */   # legal only in constructor apps
          | Ident ( [ Bitrange ] [ "!" ]          /* SyntaxRange(ii1, ii2) */
                  | "(" AppExpr {"," AppExpr} ")" /* push(ii3, ii2) */
                  ) 
             /* if type(ii2) == "SyntaxRange" then mkfactor(ii1, ii2.bits, ii2.bang) 
                else Eapp(ii1, ii2) */
          | "(" AppExpr ")"        
          | "-" Factor /* binop(0, ii1, ii2) */
          ;
Bitrange  : "@" "[" INT [":" INT /*ii2*/] "]" /* [ii3, (\ii4|ii3)+1] */ ;
#line 1208 "grammar.nw"
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
#line 1305 "grammar.nw"
FetchSpec : "fetch" ((INT | "any") "using" String  /* fetchtab[ii1] := ii3 */)
          | "address" (Add "using" String          /* fetchtab[ii1] := ii3 */
                      | "type" "is" String         /* fetchtab[ii1] := ii3 */
                      | "to" IntIdent "using" String     /* fetchtab[ii2] := ii4 */
                      )
          ;

Add      : Ident /* (ii1 == "add") | error("expected `add', `type', or `to'") */;
IntIdent : Ident /* (ii1 == "integer") | error("expected", image("integer")) */;
#line 1348 "grammar.nw"
AsmSpec : "assembly"
   ( "operand" {IdentBinding "is" OperandSyntaxSpec
                                        /* every asmoperand(!ii1, ii3[1], ii3[2]) */}
   | "opcode"  {Globbing "is" GlobRHS                /* asmopcode (ii1, ii3) */     }
   | "syntax"  (/*see_newline()*/) { AsmSyntax /* see_newline() */}
   );
OperandSyntaxSpec : String ["using" OperandNameSpec] /* [ii1, ii2] */
                  | OperandNameSpec /* ["%s", ii1] */
                  ;
OperandNameSpec   : Bindlist 
                  | Namelist 
                  | "field" Ident /* lookuptype(ii2, "field") */
                  ;
AsmSyntax: Opcode Operands NEWLINE 
               /* every set_asmsyntax(cons_named(explode_names(ii1)), ii2) */;
#line 1377 "grammar.nw"
Globbing        : SeeWhite GlobList StopWhite White /* ii2 */;
GlobList        : {GlobSpecial | Literal | Ident} 
                          /* number_braces(cat_adjacent_strings(ii1)) */;
GlobSpecial     : "{" GlobList {"," GlobList } "}" /* glob_any(push(ii3, ii2)) */
                | "*"                              /* the_glob_wildcard */
                ;
#line 1384 "grammar.nw"
GlobRHS         : SeeWhite GlobRHSList StopWhite White /* ii2 */;
GlobRHSList     : {GlobRHSSpecial | GlobRHSLiteral} /* cat_adjacent_strings(ii1) */;
GlobRHSSpecial  : "$" Integer /* glob_dollar(ii2) */
                ;
GlobRHSLiteral  : Literal | Ident | "*" | "{" | "}" | "," ;
#line 1411 "grammar.nw"
Ident     : IDENT | "_" ;
String    : '"' ;
Integer   : INT | "'" /* ord(ii1) */ ;
White     : WHITESPACE;
SeeWhite  : /* see_whitespace() */;
StopWhite : /* ignore_whitespace() */;
#line 7 "grammar.nw"
%%
#line 12 "grammar.nw"
global symtab           # table of top-level symbols
global globals          # top-level environment
global equivclasses     # list of equivalence classes of fields
global constructors     # table mapping constructor name to Stype
global conslist         # list of all constructors ever defined
global instructionctype # the type of instruction constructors
global bit_zero_is_lsb  # non-null if bits are numbered with 0 == least significant
global vanishing_latent_patlabel  # should vanish
#line 21 "grammar.nw"
procedure init_parser()
    every symtab | constructors := table()
    globals := [symtab]
    every unchecked_fields | guaranteed_fields := set()
    every conslist | equivclasses := []
    instructionctype := constype("(anonymous constructor type)", set())
    bit_zero_is_lsb := 1
    vanishing_latent_patlabel := latent_patlabel()
    
#line 328 "grammar.nw"
wordsize := 32
#line 1002 "grammar.nw"
the_relocatable := relocatable()
###  symtab["reloc"] := the_relocatable  # "reloc" no longer predefined
#line 1375 "grammar.nw"
the_glob_wildcard := glob_wildcard()
#line 30 "grammar.nw"
end
#line 42 "grammar.nw"
procedure kept_constructors(constype)
  local thelist 
  thelist := (\constype).members | conslist
  suspend 1(c := !thelist, constructors[c.name] === c)
end
#line 124 "grammar.nw"
procedure newfield(name, lo, hi)
  return (/symtab[name] := field(name, lo, hi)) | deferror("Field", image(name))
end
#line 326 "grammar.nw"
global wordsize         # word size of the machine the application runs on
#line 345 "grammar.nw"
global patlhs           # hold lhs for later action
#line 426 "grammar.nw"
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
#line 446 "grammar.nw"
global atomicid
#line 450 "grammar.nw"
record Por (patterns)                   # disjunction
record Pseq(patterns)                   # sequence
record Pand(patterns)                   # conjunction
record Pcon(name, relop, value)         # constraint on field
record Pident(name)                     # identifier standing for a pattern
record Plabel(name)                     # pattern label
record Papp(cons, args)                 # constructor applied to arguments
                                        #   (args are AppExprs)
record Plist(patterns)                  # list of patterns in square brackets
#line 563 "grammar.nw"
procedure explode_apps(opcode, args)
  l := []
  every c := explode_names(opcode) do
    put(l, Papp(cons_named(c), args))
  return if *l = 1 then l[1] else Por(l)
end
#line 626 "grammar.nw"
record Glist(values)                    
procedure Gfor(lo, hi, cols)
    local l, r
    l := []
    r := (hi - lo) / cols
    every put(l, lo + (0 to r - 1) + r * (0 to cols - 1))
    return Glist(l)
end
#line 639 "grammar.nw"
procedure patbinding(id, ast)
    local p
    
#line 663 "grammar.nw"
case type(id) of {
  "string" : verbose("Pattern ", id)
  "list"   : verbose(*id, " patterns")
}
#line 642 "grammar.nw"
    p := pnf(ast, globals)
    
#line 658 "grammar.nw"
case type(p) of {
  "pattern" : insist_global_pattern(p)
  "list"    : every insist_global_pattern(!p)
}
#line 644 "grammar.nw"
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
#line 920 "grammar.nw"
record literal(s)       # holds string or list to be emitted literally
#line 925 "grammar.nw"
record input(name, meaning)                     # input name and meaning
#line 1040 "grammar.nw"
procedure reversetrailing(c)
  local i
  suspend *&subject + 1
  i := *&subject 
  while any(c, &subject, 0 < i) do { suspend i; i -:= 1 }
end
#line 1112 "grammar.nw"
procedure has_app_or_literal_f(e)
  return type(e) == ("Eapp"|"literal")
end

procedure has_app_or_literal(e)
  suspend expwalk(e, has_app_or_literal_f)
end
#line 1142 "grammar.nw"
record SyntaxRange(bits, bang)
#line 1206 "grammar.nw"
global matching_stmts, codeheader
#line 168 "grammar.nw"
global bit_numbering_set, bit_numbering_used
#line 704 "grammar.nw"
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
#line 980 "grammar.nw"
procedure name_to_input(name, bang)
  i := input(name, 
    if name ? type(ct := symtab[tab(reversetrailing('0123456789_'))]) == "constype" then
      mark_ct_as_used(ct)
    else case type(x := symtab[name]) of {
      "field"       : if /bang then x else fwidth(x)
      "null"        : x
      "relocatable" : "reloc"
      "constype"    : impossible("missed first-round search for constype")
      default       : typeerror(x, "free variable, field, or constructor input", name,
                                globals)
  })
  if \bang & type(i.meaning) ~== "integer" then
    typeerror(i.meaning, "field (only fields can be sign-extended)", name, globals)
  return i
end
#line 999 "grammar.nw"
record relocatable()    # type of a name that means relocatable address
global the_relocatable  # we only need one...
#line 1007 "grammar.nw"
procedure make_relocatable(name)
  return (/symtab[name] := the_relocatable) | deferror("Relocatable name", image(name))
end
#line 1024 "grammar.nw"
procedure mark_ct_as_used(type)
  if /type.used := lineno then
    {
#line 1030 "grammar.nw"
t := table()
every m := !type.members do
  t[m.name] := m
t := sort(t)
type.members := []
every put(type.members, (!t)[2])
#line 1026 "grammar.nw"
                             }
  return type
end
#line 1188 "grammar.nw"
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
#line 1370 "grammar.nw"
record glob_any(alternatives, number)   # braced list of alternate globbing patterns
record glob_wildcard()
global the_glob_wildcard                # only need one value
record glob_dollar(number)              # $n on right-hand side
#line 1391 "grammar.nw"
procedure cat_adjacent_strings(l)
  m := []
  every x := !l do 
    if type(x) == "string" & type(m[-1]) == "string" then
      m[-1] ||:= x
    else
      put(m, x)
  return m
end
