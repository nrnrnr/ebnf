%attribute region yes
%term TYVAR      (string)
%term VNAME      (string)
%term CNAME      (string)
%term QVNAME      (string)
%term QCNAME      (string)
%term CAPSNAME   (string)
%term FLOAT      (string)
%term INT (string)
structure A = Ast
structure D = Ast.Def
structure T = Ast.Test
structure Decl = Ast.Decl
structure AN = Ast.Namespace

val lift = A.BeforeInfix.EXP
type digit = int
fun fst(x, _) = x
fun snd(_, y) = y
fun foo x = foo x

fun id x = x

fun bleat s = TextIO.output (TextIO.stdErr, s ^ "\n")
fun errorAt r s = raise Ast.TmpError (r, s)

fun impossible s = let exception Can'tHappen of string in raise Can'tHappen s end

fun foldl1 f [] = impossible "foldl1 applied to empty list"
  | foldl1 f [x] = x
  | foldl1 f (x::xs) = f (x, foldl1 f xs)


fun applyTyconsAt r [] [tau] = tau
  | applyTyconsAt r [] (_::_::_) = errorAt r "multiple types separated by commas"
  | applyTyconsAt r [] [] = impossible "empty list of type parameters"
  | applyTyconsAt r (c::cs) taus =
       foldl (fn (c, tau) => A.TYAPP (c, [tau])) (A.TYAPP (c, taus)) cs

fun don'tApplyTo r what = errorAt r ("You can apply a function to a '" ^ what
                                     ^ "' expression, but you have to wrap the '" ^
                                     what ^ "' in parentheses")
fun changeMinus #"-" = #"~"
  | changeMinus c = c
fun intOfString rr s =
  case IntInf.fromString (String.map changeMinus s)
    of SOME i => i
     | NONE => errorAt rr "This can't happen: failed to convert integer literal"
 
fun doubleOfString rr s =
  case Real.fromString (String.map changeMinus s)
    of SOME x => x
     | NONE => errorAt rr "This can't happen: failed to convert floating-point"
fun qname' []  = impossible "empty qualified name"
  | qname' [e] = A.NAME e
  | qname' (m :: path) = A.IN_MODULE (m, qname' path)

fun qname s =
  let val cs = UTF8.explode s
      val dot = UTF8.fromAscii #"."
      val uimplode = String.concat o map UTF8.encode
      fun splitCons (c, cs::css) = if c = dot then []::cs::css
                                   else (c::cs)::css
        | splitCons (c, []) = impossible "a nonempty list became empty"
      val names = map uimplode (foldr splitCons [[]] (UTF8.explode s))
  in  qname' names
  end

fun qnameString q =
  let fun parts (A.NAME n) = [n]
        | parts (A.IN_MODULE (m, path)) = m :: "." :: parts path
  in  String.concat (parts q)
  end


fun sameName x = (x, x)

fun imports convert default what =
  let val what = convert (getOpt (what, default))
  in  map (fn (ext, int) => { what = what, ename = ext, iname = int })
  end

fun asUnit _ AN.MODULE = AN.Unit.MODULE
  | asUnit _ AN.MODTY    = AN.Unit.MODTY
  | asUnit r AN.VAL    = errorAt r ("A value cannot be imported directly; use 'from ... import ...")
  | asUnit r AN.TY     = errorAt r ("A type cannot be imported directly; use 'from ... import ...")

fun nameCheck region namespace external internal =
  bleat ("Import of " ^ external ^ " not checked")
%%
# EBNF grammar for Jumbo ML
Unit : { Imports }
       { "module" ( "type" SigName "=" ModTy
                           /* A.Unit.MODTYPE (ii2, ii4) */
                   | ModName {ModTy} [":>" ModTy] "=" Module
                           /* A.Unit.MODULE (ii1, ii3, ii5) */
                   ) }
       /* A.UNIT (List.concat ii1, ii2) */
      ;

Imports : "import" ImportModulesOrRenamed {"," ImportModulesOrRenamed /* ii2 */}
           /* List.concat (ii2 :: ii3) */
       ;

ImportModulesOrRenamed
   : [ImportWhat] ModName
        ("as" ModName /* fn mn => [(mn, ii2)] */
        | {ModName}   /* fn mn => sameName mn :: map sameName ii1 */
        )
     /* map A.IMPORT (imports (asUnit rr1) AN.MODULE ii1 (ii3 ii2)) */
   ;

ImportWhat : "type" /* AN.TY */
           | "module" ( /* AN.MODULE */ | "type" /* AN.MODTY */)
           | "val" /* AN.VAL */
           ;

FromImports : "from" QModName "import" ImportOrRenamed {"," ImportOrRenamed}
                 /* map (fn f => f ii2) (ii4::ii5) */
            ;

QModName : QCName | CAPSNAME /* A.NAME ii1 */;

ImportOrRenamed
   : [ImportWhat] ModName
        ("as" ModName /* fn mn => [(mn, ii2)] */
        | {ModName}   /* fn mn => sameName mn :: map sameName ii1 */
        )
     /* fn from => D.FROM_IMPORT (from, imports id AN.VAL ii1 (ii3 ii2)) */
   ;


ModTy : AtomicModTy {"&&" AtomicModTy} /* foldl1 Decl.SUP (ii1::ii2) */;

AtomicModTy : SigName        /* Decl.MODTYNAME ii1 */
            | "{" {Decl} "}" /* Decl.MODTY ii2 */
            | "(" ModTy ")"
            ;

SigName : VNAME /* errorAt rr1 ("In this context, I'm expecting " ^
                                "the name of a module type, but I saw " ^ ii1) */
        | CNAME
        | CAPSNAME
        ;
Module : "{" { Def /* [ii1] */ | FromImports } "}" /* D.MODULE (List.concat ii2) */
       | QModName /* D.MODNAME ii1 */
       ;
Def : DefM /* D.MARK (rr0, ii1) */;

DefM : "def-type" TyParms TyconName "=" Ty 
         /* D.TY (ii3, ii2, ii5) */
          
     | "def-mix" TyParms TyconName "=" "{"
           ( [";"]  TypedCname TypedCnames /* ii2 :: ii3 */ )
       "}"
         /* D.MIX (ii3, ii2, ii6) */

     | "def-struct" TyParms TyconName "=" "{"
           ( [";"]  TypedVname TypedVnames /* ii2 :: ii3 */ )
       "}"
         /* D.STRUCT (ii3, ii2, ii6) */
     | ("def" /* D.DEF */ | "redef" /* D.REDEF */)
       VNAME TyParms {VNAME} "=" Exp
         /* ii1 (ii2, ii3, ii4, ii6) */
     | "val" VNAME ":" Ty /* D.VAL (ii2, ii4) */
     | Fixity /* D.FIXITY ii1 */
     | Tests
     ;

Fixity : ( "infix" /*A.NONASSOC*/
         | "infixl" /*A.LEFT*/
         | "infixr" /*A.RIGHT*/
         ) Precedence Name {Name} /* (A.INFIX (ii2, ii1), ii3::ii4) */
       | "nonfix" Name {Name} /* (A.PREFIX, ii2::ii3) */
       ;
Tests : "check"
          ( "val" Exp ":" Ty   /* T.TY (ii2, ii4) */
          | Exp ( "expect" Exp ["within" Exp]
                           /* fn e => T.EXPECT_WITHIN (e, ii2, ii3) */
                | "errors" /* T.ERRORS */
                | "raises" Exp /* fn e => T.RAISES (e, ii2) */
                ) /* ii2 ii1 */
          ) /* D.TEST ii2 */
      ;

Precedence : Int /* (IntInf.toInt ii1, []) handle _ => errorAt rr1 "precedence too big" */
           | FLOAT /* errorAt rr1 "don't want to deal with fractional precedence today" */
           ;

# WE SHOULD DO BETTER WITH MARKING HERE 
Ty  : TyM /* A.MARKTY (rr0, ii1) */ ;
TyM : "forall" { TYVAR } "." Ty /* A.FORALL (ii2, ii4) */
    | TyApp {"->" TyApp} /* case rev (ii1 :: ii2)
                             of tau :: taus => foldl A.ARROW tau taus
                              | [] => let exception No in raise No end */
   ;

TyApp : Atomic {Tycon} /* A.MARKTY (rr0, applyTyconsAt rr1 ii2 ii1) */
      ;

QVName : VNAME /* A.NAME ii1 */
       | QVNAME /* qname ii1 */
       ;

QCName : CNAME /* A.NAME ii1 */
       | QCNAME /* qname ii1 */
       ;

TyconName : VNAME | CNAME | CAPSNAME
          ;
QTyconName : CAPSNAME /* A.NAME ii1 */ | QCName | QVName;
Tycon : QTyconName /* A.TYCON ii1 */;

Atomic : Tycon /* [ii1] */
       | TYVAR /* [A.TYVAR ii1] */
       | "(" Ty {"," Ty} ")" /* ii2 :: ii3 */
       ;

Exp : GoodSoloExp /* lift (A.MARKE (rr0, lift ii1)) */
    | ArgExp {ArgExp|BadSoloExp} [RightGrabbingSoloExp]
        /* lift (A.MARKE (rr0, A.BeforeInfix.APPLY (lift ii1 :: map lift ii2))) */
    ;


ArgExp : QVName /* A.VNAME ii1 */
       | QCName /* A.CNAME ii1 */
       | CAPSNAME /* A.CNAME (A.NAME ii1) */
       | Double /* A.LIT (A.DOUBLE ii1) */
       | "(" Exp ")" /* A.BRACKETS ii2 */
       | Int /* A.LIT (A.INT ii1) */ 
       | "..." /* A.HOLE rr1 */
       | "[" ( /* [] */ | Exp {"," Exp} /* ii1 :: ii2 */ ) "]"  /* A.LIST ii2 */
       ;

Int : INT /* intOfString rr1 ii1 */;
Double : FLOAT /* doubleOfString rr1 ii1 */;

BadSoloExp : SoloExp /* let val (what, _) = ii1 in don'tApplyTo rr1 what end */ ;
BadRGSoloExp : RightGrabbingSoloExp
                     /* let val (what, _) = ii1 in don'tApplyTo rr1 what end */ ;
GoodSoloExp : ( SoloExp
              | RightGrabbingSoloExp
              ) /* let val (_, exp) = ii1 in exp end */ ;

SoloExp : "case" Exp "of" "{"
              ( [";"]  CaseArm CaseArms /* ii2 :: ii3 */ )
          "}"
            /* ("case", A.CASE (ii2, ii5)) */

        | "cond" "{"
              ( [";"]  QnA QnAs /* ii2 :: ii3 */ )
          "}"
            /* ("cond", A.COND ii3) */
 
        ;

RightGrabbingSoloExp 
        : Lambda VNAME {VNAME} "=>" Exp /* ("lambda", A.LAMBDA (ii2::ii3, ii5)) */
        | "local" {Def} "in" Exp /* ("local", A.LOCAL (ii2, ii4)) */
        ;        

Lambda : "fn" | "\" ;  # would also like Unicode lambda

CaseArm  : CasePat "=>" Exp /* (ii1, ii3) */ ;
CaseArms : /* [] */
         | ";" ( /* [] */ | CaseArm CaseArms /* ii1 :: ii2 */)
         ;

# need to test for fixity here
CasePat : VNAME ( CName VNAME /* fn x => (ii1, [A.PAT x, A.PAT ii2]) */
                | [ (VNAME | "_") {AtomicPat} /* ii1 */]
                    /* fn x => errorAt rr1 ("After " ^ x ^ ", expected an infix value constructor but found " ^ (getOpt (ii1, "nothing"))) */
                ) /* ii2 ii1 */
        | CName {AtomicPat} /* (ii1, ii2) */
        ;

CName : QCName
      | CAPSNAME /* A.NAME ii1 */
      ;

AtomicPat : CNAME  /* errorAt rr1 ("in a pattern, a constructor is applied to a value name, but here it is applied to the constructor name " ^ ii1) */
      | VNAME /* A.PAT ii1 */
      | "_"   /* A.WILD */
      ;

QnA  : Exp "=>" Exp /* (ii1, ii3) */ ;
QnAs : /* [] */
     | ";" ( /* [] */ | QnA QnAs /* ii1 :: ii2 */)
     ;


TyParms : /* [] */
        | TYVAR /* [ii1] */
        | "(" TYVAR {"," TYVAR /* ii2 */} ")" /* ii2 :: ii3 */
        ;

TypedCname  : (CNAME|CAPSNAME) ":" Ty /* (ii1, ii3) */ ;
TypedCnames : /* [] */
            | ";" ( /* [] */ | TypedCname TypedCnames /* ii1 :: ii2 */)
            ;

TypedVname  : VNAME ":" Ty /* (ii1, ii3) */ ;
TypedVnames : /* [] */
            | ";" ( /* [] */ | TypedVname TypedVnames /* ii1 :: ii2 */)
            ;

Decl : ("mutable" /* A.MUTABLE */ | /* A.IMMUTABLE */) "type" TyParms TyconName 
          ["=" Ty]
          /* case ii5
               of NONE => Decl.ABSTYPE (ii4, ii3, ii1)
                | SOME ty =>
                    case ii1
                      of A.IMMUTABLE => Decl.MANIFEST (ii4, ii3, ty) 
                       | A.MUTABLE =>
                           errorAt rr1 "You can't declare a manifest type to be mutable"
           */
     | "val" (VNAME | CNAME) ":" Ty /* Decl.VAL (ii2, ii4) */
     | "exn" (VNAME /* errorAt rr1 "an exception is a value constructor and must be named like one" */ | CNAME) ":" Ty /* Decl.EXN (ii2, ii4) */
     | "module" ModName ":" ModTy /* Decl.MODULE (ii2, ii4) */
     | Fixity /* Decl.FIXITY ii1 */
     ;


ModName : CNAME | CAPSNAME | VNAME /* errorAt rr1 (ii1 ^ " cannot be the name of a module.  Try using a capital letter at the beginning.") */ ;

#ImportModName : ( "type" /* AN.TY */
#                | "module" ( /* AN.MODULE */ | "type" /* AN.MODTY */)
#                | "val" /* AN.VAL */
#                |    /* AN.VAL (*default*) */  
#                )
#                | Name ["as" Name] 
#                  /* A.IMPORT { ename = ii2, iname = getOpt (ii3, ii2), what = ii1 } 
#                     before nameCheck rr0 ii1 ii2 ii3 */
#              ;

Name: CNAME | CAPSNAME | VNAME ;
