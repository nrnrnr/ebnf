(* set functions *)
type 'a set = 'a list
val emptyset : 'a set = []
fun member(x, [])   = false
  | member(x, h::t) = x = h orelse member(x, t)
fun insert(x, l) = if member(x, l) then l else x::l
fun delete(x, [])   = []
  | delete(x, h::t) = if x = h then t else h :: delete(x, t)

(* lexing *)

datatype 'a stream = STREAM of unit -> 'a * 'a stream
fun invoke (STREAM lex) = lex()

exception TokenMismatch of string
exception Expected of string * string option 
        (* token expected, nonterminal being parsed *)

fun expect (f, token, lex, nt) =
  (f token, invoke lex) handle TokenMismatch t => raise Expected(t, nt)

exception SyntaxError of string
fun synerror s = raise SyntaxError s


(* closure and optional for parsing *)
fun closure((token, lex), predicate, parse) =
  let fun cl(token, lex, l) =
        if predicate token then
          let val (x, (token, lex)) = parse(token, lex)
          in  cl(token, lex, x::l)
          end
        else
          (rev l, (token, lex))
  in  cl(token, lex, [])
  end

fun optional((token, lex), predicate, parse) = 
   if predicate token then 
     let val (x, (token, lex)) = parse(token, lex)
     in  (SOME x, (token, lex))
     end
   else 
     (NONE, (token, lex))

exception LeftoverTokens of token stream
fun make_parser (f, unEOF) stream =
  let val (result, (token, lex)) = f (invoke stream)
  in  (unEOF token; result) handle TokenMismatch _ => raise LeftoverTokens stream
  end
