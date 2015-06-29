%attribute region yes
%term T_STRING   (string)
%term T_IDENT    (string)
%term T_INT      (int)
%term CASELINE   ((string * (int*int)) * string option)
%term CODELINE   (string)
%term NEWLINE
%term WHITESPACE (string)
%attribute arg-pat {seeing_newlines : bool ref, seeing_ws : bool ref}
%attribute arg-pat
open Ast 
fun fst(x, _) = x
fun snd(_, y) = y
fun colons_to_labels (s, tail) =
  let exception NotLabel
      fun rawlabel (NARY(AND, [IDENT l])) = NARY(PLABEL l, [])
        | rawlabel _ = raise NotLabel
      fun tolabel e = MARK_EXP(expRegion e, rawlabel (unmarkExp e))
      fun cvt(s, [], r) = rev (s::r)
        | cvt(l, (s,true,colonRgn) ::tail, r) = (cvt(s, tail, tolabel l :: r)
                handle NotLabel => 
                   synerror(R'COLON, colonRgn, "Pattern", 
                            ["Colon must be preceded by an identifier"]))
        | cvt(l, (s,false,_)::tail, r) = cvt(s, tail, l :: r)
  in  cvt(s, tail, [])
  end
fun process_operands ops =
  let val iswhitec = fn #" " => true | #"\t" => true | #"\n" => true | _ => false
      fun forall f [] = true
        | forall f (h::t) = f h andalso forall f t
      val iswhite = forall iswhitec o explode
      fun striprev([rand as LITERAL_operand w], r) = if iswhite w then r else rand :: r
        | striprev([], r) = r
        | striprev(h::t, r) = striprev(t, h::r)
      fun mergerev(LITERAL_operand o1 :: LITERAL_operand o2 :: t, r) =
                mergerev(LITERAL_operand (o2^o1) (* reversed! *) :: t, r)
        | mergerev(h::t, r) = mergerev(t, h::r)
        | mergerev([], r) = r
  in  mergerev(striprev(ops, []), [])
  end : operand list
fun idcode  (name, rgn) = (OPCODE         name, rgn)
fun strcode (name, rgn) = (LITERAL_opcode name, rgn)
fun mkopcode((firstop, firstreg), rest) =
  let val region  = foldr (fn ((s, r), r') => SourceMap.span(r, r')) firstreg rest
      val opnames = firstop :: map fst rest
  in  {opcode=(opnames, region)}
  end
fun identExp (id, rgn) = MARK_EXP (rgn, IDENT id)
fun mark (rgn, e as MARK_EXP _) = e
  | mark (rgn, e) = MARK_EXP(rgn, e)
fun mkfactor (id, bits, range) =
  let val e   = identExp id
      val e'  = case bits  of NONE => e  | SOME r => NARY(SLICE, e::r)
      val e'' = case range of NONE => e' | _ => NARY(EXTEND, [e'])
  in  e''
  end
fun cat_adjacent_strings (Glob.LIT l1 :: Glob.LIT l2 :: r) =
                cat_adjacent_strings(Glob.LIT (l1^l2) :: r)
  | cat_adjacent_strings(h::t) = h :: cat_adjacent_strings t
  | cat_adjacent_strings [] = []
fun see_newline ()      = seeing_newlines := true
fun ignore_newlines()   = seeing_newlines := false
fun see_whitespace()    = seeing_ws := true
fun ignore_whitespace() = seeing_ws := false
fun charconst ([c], _) = ord c
  | charconst (l, rgn) = synerror(R'TICK, rgn, "char", 
                           ["character constant of length <> 1 : '", implode l, "'"])

type ast = spec list list
val nullRegion = SourceMap.nullRegion
%%
# EBNF grammar for toolkit specification language
Parsers   : "bogusspecmarker " Spec | "boguscodemarker " CodeFile;
Spec      : { Fieldspec | Patterns | Constructors | Placeholder 
            | FetchSpec | PCSpec   | RelocSpec    | AsmSpec 
            | BitSpec   
            } /* SPEC (foldr op @ [] ii1) */ ;
PCSpec : "pc_unit_bits" T_INT /* [PC_unit_bits (ii2,rr2)] */;
Fieldspec : "fields" "of" Ident "(" Integer ")" 
            { Ident Integer [":" Integer]  /* (ii1, ii2, ii3, rr1) */ }
            /* [TOKENDEF (ii3, ii5, ii7, rr3)] */
          ;
BitSpec      : "bit" Zero "is" Significance Significant
                 /* [BIT_zero_significance ii4] */
             ;
Zero         : Integer /* ii1 = 0 orelse 
                        synerror(T_INT ii1, rr1, "Significance", ["expected `0'"]) */;
Significance : Ident
                /* (case ii1 
                      of "most" => MOST | "least" => LEAST
                       | _ => synerror(T_IDENT ii1, rr1, "Significance", 
                                       ["expected `most' or `least'"]),
                    rr1)
                 */;
Significant  : Ident /* ii1 = "significant" orelse 
                        synerror(T_IDENT ii1, rr1, "Significance", 
                                 ["expected `significant'"]) */;
Placeholder : "placeholder" "for" Ident "is" Pattern 
              /* [PLACEHOLDER (ii3, ii5, rr3)] */
            ;
Fieldspec     : "fieldinfo" { Fieldinfo } ;
Fieldinfo     : IdentBinding "is" "[" { Fielditem } "]" /*  FIELDINFO(ii1, ii4) */;
IdentBinding  : IdentRgn /* [ii1] */ | "[" { IdentRgn } "]";
Fielditem     : FieldChecking /* CHECKING ii1 */ | NameTable /* FIELDNAME ii1 */;
NameTable     : "sparse" "[" NameBindings "]" /* SPARSE_NAMES ii3 */ 
              | "names" "[" {String|Ident} "]" /* DENSE_NAMES ii3 */ 
              ;
NameBindings  : NameBinding {"," NameBinding}  /* ii1::ii2 */;
NameBinding   : (String|Ident) "=" IntExp /* (ii1, ii3) */ ;
FieldChecking : Ident /* case ii1 
                           of "guaranteed" => GUARANTEED
                            | "checked"    => CHECKED
                            | "unchecked"  => UNCHECKED
                            | i => synerror(T_IDENT ii1, rr1, "fieldinfo", 
                                            ["unknown field attribute ", i]) */;
Fieldspec : "wordsize" Integer /* [WORDSIZE ii2] */;
Patterns  : "patterns" { PatBinding } /* foldr op @ [] ii2 */;
PatBinding: IdentRgn "is" BindingRHS /* ii3 (ONE_NAME ii1) */
          | "[" {IdentRgn} "]" "is" Pattern /* [PATBINDING(TABLE ii2, ii5)] */
          ;
BindingRHS: Pattern /* fn lhs => [PATBINDING(lhs, ii1)] */
          | "any" "of" "[" {IdentRgn} "]" "," "which" "is" Pattern
              /* let fun notWild (id, rgn) = id <> "_"
                 in  fn lhs => 
                       [PATBINDING(TABLE ii4, ii9),
                        PATBINDING(lhs, 
                           mark(rr4,
                                    NARY(OR, map identExp (List.filter notWild ii4))))]
                 end
               */
          ;
Pattern   : Disjunct      { "|" Disjunct }     /* mark(rr0, NARY(OR, ii1::ii2)) */;
Disjunct  : Sequent       { ";" Sequent /* (ii2, false, rr1) */
                          | ":" Sequent /* (ii2, true, rr1) */
                          }  
                          /* mark(rr0, NARY(CONCAT, colons_to_labels(ii1,ii2))) */
          ;
Sequent   : Conjunct      { "&" Conjunct }    /* mark(rr0, NARY(AND, ii1::ii2)) */;

Conjunct  : "..." DotsR    /* mark(rr0, NARY(PREFIX_DOTS, [ii2])) */
          | DotsR 
          ;
DotsR     : Atomic ["..."]  
                 /* case ii2 of NONE => ii1
                              | SOME _ => mark(rr0, NARY(POSTFIX_DOTS, [ii1])) */;

Atomic    : UAtomic /* mark(rr1, ii1) */;
UAtomic   : IdentRgn
            (                       /* fn (i, r) => IDENT i */
            | Relop (Generator | Expr)       
                                    /* fn id => NARY(RELOP ii1, [identExp id, ii2]) */
            | {"^" Opname} "(" Explist ")"   
                                    /* fn id => APP(mkopcode(idcode id,ii1), ii3) */
            ) /* ii2 ii1 */
          | StringRgn {"^" Opname} "(" Explist ")" 
                                    /* APP(mkopcode(strcode ii1,ii2), ii4) */
          | "(" Pattern ")"
          | "[" {IdentRgn /* identExp ii1 */} "]"   /* NARY(LIST,ii2) */
          ;
Explist   : Expr {"," Expr} /* ii1::ii2 */
          | /* [] */
          ;
Relop      : "="   /* Relop.EQ */
           | "!="  /* Relop.NE */
           | "<="  /* Relop.LE */
           | ">="  /* Relop.GE */
           | "<"   /* Relop.LT */
           | ">"   /* Relop.GT */;
Generator  : UGenerator /* mark(rr1, ii1) */;
IntExp     : Integer /* mark(rr0, INT ii1) */;
UGenerator : "{" IntExp "to" IntExp [ "columns" IntExp /* ii2 */ ] "}"  
                /* NARY(GEN_to, [ii2, ii4] @ 
                                 (case ii5 of NONE => [] | SOME n => [n])) */
           | "[" { IntExp } "]"     /* NARY(LIST, ii2) */
           ;
UAtomic : "epsilon" /* IDENT "epsilon" */
        | "some" IdentRgn /* NARY(SOME_token, [identExp ii2]) */
        ;
Constructors : "constructors" (/*see_newline()*/) 
                    { Constructor /* see_newline(); ii1 */} 
                    /* ignore_newlines(); ii3 */;
Constructor  : Opcode Operands [ ":" AddressIdent ] NLBranches
                 /* CONSTRUCTOR (ii1, ii2, ii3, ii4) */
               ;
AddressIdent : "address" | Ident;
Operands : SeeWhite {Operand} StopWhite /* process_operands ii2 */;
NLBranches   : Branches
             | NEWLINE (Branches | /* [ {eqns=[], rhs=NONE} ] */)
             ;
Branches     : SingleBranch /* [ii1] */
             | WhenBranch {WhenBranch | OtherwiseBranch}
                /* ii1 :: ii2 */
             ;
SingleBranch : "{" Equations "}" [ "is" Pattern ] /* {eqns=ii2, rhs=ii4} */ 
             | "is" Pattern                       /* {eqns=[],  rhs=SOME ii2} */ 
             ;
WhenBranch      : "when" "{" Equations "}" "is" Pattern /* {eqns=ii3, rhs=SOME ii6} */;
OtherwiseBranch : "otherwise" "is" Pattern              /* {eqns=[],  rhs=SOME ii3} */;
Opcode    : Opname { "^" Opname }  /* mkopcode(ii1, ii2) */ ;
Opname    : IdentRgn  /* idcode ii1 */ | StringRgn /* strcode ii1 */;
Operand      : (IdentRgn | AddressAsIdentRgn) ["!"]
                    /* OPERAND(ii1, case ii2 of NONE => false | _ => true) */
             | (Literal | GlobOperator | White) /* LITERAL_operand ii1 */
             ;
AddressAsIdentRgn : "address" SeeWhite /* (ii1, rr1) */;
  # reserved word automatically causes newlines to be ignored, so fix it
Literal      : String | Integer /* Int.toString ii1 */ | "=>" 
             | "=" | "!=" | "<=" | ">=" | "<" | ">"   
             | "[" | "]" | "(" | ")" | "+" | "-" | "/" 
             | "&" | "@" | "#" | "%" | ";" | "|" 
             ;
GlobOperator : "*" | "$" | ","
             ;
RelocSpec : "relocatable" Ident {Ident} /* [RELOCATABLE (ii2::ii3)] */;
Constructors : "overload" "operand" Operand "of" Opcode "at" "[" {Integer} "]" "with"
               "{" Operands "}"
                 /* [OVERLOAD {constructor = #opcode ii5,
                               operand     = ii3,
                               widths      = ii8,
                               instances   = process_operands ii12}] */;
Constructors : "discard" {Opcode} /* [DISCARD ii2] */
             | "keep"    {Opcode} /* [KEEP ii2] */
             ;
Equations : Equation { "," Equation } /* (ii1::ii2) */
          |  /* [] */ ;
Equation  : Expr Relop Expr /* NARY(RELOP ii2, [ii1, ii3]) */;
Expr      : Term { AOp Term /* (ii1, ii2) */ } 
                /* foldl (fn ((rator, rand), e) => NARY(rator, [e, rand])) ii1 ii2 */;
Term      : UTerm /* mark(rr0, ii1) */;
UTerm     : Factor { Mop Factor  /* (ii1, ii2) */ } 
                /* foldl (fn ((rator, rand), e) => NARY(rator, [e, rand])) ii1 ii2 */;
AOp       : "+" /* PLUS */  | "-" /* MINUS */ ; 
Mop       : "*" /* TIMES */ | "/" /* DIV */ ; 
Factor    : UFactor /* mark(rr0, ii1) */
          | "(" Expr ")"        
          ;
UFactor   : Integer /* INT ii1 */
          | String  /* LITERAL_exp ii1 */   # legal only in constructor apps
          | "_"     /* NARY(WILDCARD, []) */
          | (T_IDENT | "address")
              ( [ Bitrange ] [ "!" ]    /* fn id => mkfactor(id, ii1, ii2) */
              | "(" Expr {"," Expr} ")" 
                                  /* fn id => APP(mkopcode(idcode id,[]), ii2::ii3) */
              ) /* ii2 (ii1,rr1) */
          | "-" Factor /* NARY(MINUS, [ii2]) */
          ;
Bitrange  : "@" "[" IntExp [":" IntExp] "]"
                 /* ii3 :: (case ii4 of SOME i => [i] | _ => []) */;
CodeFile      : Code { Casestmt Code /* (ii1, ii2) */ } /* CODEFILE (ii1, ii2) */;
Casestmt      : CASELINE { Casearm } [ElseArm] "endmatch" 
                      /* let val ((pc, rgn), optname) = ii1
                         in  (([pc], rgn), optname, ii2, ii3)
                         end */;
Casearm       : "|" Pattern OptEquations OptName "=>" Code
                      /* (ii2, ii3, ii4, ii6 : code) */;
ElseArm       : "else" Code;
OptName       : [ "[" Ident "]" ];
OptEquations  : "{" Equations "}" | /* [] */;
Code : /* ([], nullRegion) */
     | Codeline {Codeline} /* #opcode (mkopcode(ii1, ii2)) : code */
     ;
Codeline : CODELINE /* (ii1, rr1) */;
FetchSpec : "fetch" (Integer /* SOME ii1 */ | "any" /* NONE */) "using" String 
                /* [FETCH (ii2, ii4)] */
          | "address" (Add "using" String             /* [ADDRESS_add ii3] */
                      | "type" "is" String            /* [ADDRESS_type ii3] */
                      | "to" IntIdent "using" String  /* [ADDRESS_convert ii4] */
                      )
          ;
Add      : Ident /* ii1 = "add" orelse
                    synerror(T_IDENT ii1, rr1, "address-spec", 
                             ["expected `add', `type', or `to'"]) */;
IntIdent : Ident /* ii1 = "integer" orelse
                    synerror(T_IDENT ii1, rr1, "address-spec",
                             ["expected \"integer\""]) */;
AsmSpec : "assembly"
   ( "operand" {IdentBinding "is" OperandSyntaxSpec /* ASM_operand (ii1, ii3) */}
   | "component"  {Globbing "is" GlobTarget  /* ASM_component(ii1, ii3) */}
   | "opcode"  {Globbing "is" GlobTarget  /* ASM_opcode (ii1, ii3) */}
   | "syntax"  (/*see_newline()*/) { AsmSyntax /* see_newline(); ii1 */} 
                /* ignore_newlines(); ii3 */
   );
OperandSyntaxSpec : String ["using" OperandNameSpec ] /* STRING_operand(ii1, ii2) */
                  | OperandNameSpec                   /* NAMED_operand ii1 */
                  ;
OperandNameSpec   : NameTable     /* OPERAND_name_table ii1 */
                  | "field" Ident /* OPERAND_like_field ii2 */
                  ;
AsmSyntax: Opcode Operands NEWLINE /* ASM_syntax(ii1, ii2) */;
Globbing         : SeeWhite GlobPattern StopWhite White /* ii2 */;
GlobPattern      : {GlobAlternatives 
                   | Literal /* Glob.LITPAT ii1 */
                   | Ident   /* Glob.LITPAT ii1 */
                   } 
                 ;
GlobAlternatives : "{" GlobPattern {"," GlobPattern } "}" 
                        /* Glob.ALTS (ii2::ii3) */
                 | "*"  /* Glob.WILD */
                 ;
GlobTarget       : SeeWhite GlobTargets StopWhite White /* ii2 */;
GlobTargets      : {GlobTargetSpecial | GlobTargetLiteral /* Glob.LIT ii1 */} 
                                                /* cat_adjacent_strings(ii1) */;
GlobTargetSpecial: "$" (Integer /* Glob.DOLLAR ii1 */ 
                       | "$"    /* Glob.LIT "$" */
                       )
                 ;
GlobTargetLiteral: Literal | Ident | "*" | "{" | "}" | "," ;
Ident     : T_IDENT | "_";
IdentRgn  : Ident /*(ii1,rr1)*/;
String    : T_STRING;
StringRgn : String /*(ii1,rr1)*/;
Integer   : T_INT | "'" /* charconst(explode ii1, rr1) */;
White     : WHITESPACE;
SeeWhite  : /* see_whitespace() */;
StopWhite : /* ignore_whitespace() */;
%%
