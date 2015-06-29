#include <stdio.h>
#include "tokens.h"

enum token token;
extern int yylineno;

void parse_error() {
	fprintf(stderr, "line %d: syntax error\n", yylineno);
	exit(0);
}

void match(enum token t) {
	if (token != t) {
		parse_error();
	}
	token = yylex();
}

int yyparse() {
	token = yylex();
	module();
}

%term ident		IDENT
%term integer 		INTEGER
%term real 		REAL
%term CharConstant	CHARCONST
%term string		STRING
%term "+"		PLUS
%term "-"		MINUS
%term "*"		STAR
%term "/"		SLASH
%term "~"		TILDE
%term "&"		AMBERSAND
%term "."		DOT
%term ","		COMMA
%term ";"		SEMI
%term "|"		BAR

%term ":="		ASSIGN
%term "^"		CARAT
%term "="		EQUALS
%term "#"		HASH
%term "<"		LT
%term ">"		GT
%term "<="		LE
%term ">="		GE
%term ".."		DOTDOT
%term ":"		COLON
%term "["		LSQUARE
%term "]"		RSQUARE
%term "("		LPAREN
%term ")"		RPAREN
%term "{"		LCURLY
%term "}"		RCURLY
%term ARRAY
%term BEGIN
%term CASE
%term CONST
%term DIV
%term DO
%term ELSE
%term ELSIF
%term END
%term EXIT
%term IF
%term IMPORT
%term IN
%term IS
%term LOOP
%term MOD
%term MODULE
%term NIL
%term OF
%term OR
%term POINTER
%term PROCEDURE
%term RECORD
%term REPEAT
%term RETURN
%term THEN
%term TO
%term TYPE
%term UNTIL
%term VAR
%term WHILE
%term WITH
%%
module = MODULE ident ";" ImportList DeclarationSequence OptionalBody END ident "."
number = integer 
	| real

qualident = ident qualidentTail
qualidentTail = "." ident
	|

identdef = ident identdefTail
identdefTail = "*"
	|

ConstantDeclaration = identdef "=" ConstExpression
ConstExpression = expression
TypeDeclaration = identdef "=" type
type = qualident 
	| ArrayType 
	| RecordType 
	| PointerType 
	| ProcedureType

ArrayType = ARRAY lengthList OF type
lengthList = length lengthListTail
lengthListTail = "," length lengthListTail
	|

length = ConstExpression
RecordType = RECORD OptionalBaseType FieldListSequence END
OptionalBaseType = "(" BaseType ")"
	|

BaseType = qualident
FieldListSequence = FieldList FieldListTail
FieldListTail = ";" FieldList FieldListTail
	|

FieldList = IdentList ":" type
	|

IdentList = identdef IdentListTail
IdentListTail = "," identdef IdentListTail
	|

PointerType = POINTER TO type
ProcedureType = PROCEDURE OptionalFormalParameters
OptionalFormalParameters = FormalParameters
	|

VariableDeclaration = IdentList ":" type
designator = ident designatorTail
designatorTail = "." ident designatorTail
	| "[" ExpList "]" designatorTail
	| "^" designatorTail
	|

ExpList = expression ExpListTail
ExpListTail = "," expression ExpListTail
	|

expression = SimpleExpression expressionTail
expressionTail = relation SimpleExpression
	|

relation = "=" 
	| "#" 
	| "<" 
	| "<=" 
	| ">" 
	| ">=" 
	| IN 
	| IS

SimpleExpression = SimpleExpressionPrefix term SimpleExprTail
SimpleExpressionPrefix = "+"
	| "-"
	|
SimpleExprTail = AddOperator term SimpleExprTail
	|

AddOperator = "+" 
	| "-" 
	| OR

term = factor termTail
termTail = MulOperator factor termTail
	|

MulOperator = "*" 
	| "/" 
	| DIV 
	| MOD 
	| "&"

factor = number 
	| CharConstant 
	| string 
	| NIL 
	| set 
	| designator Actuals
	| "(" expression ")" 
	| "~" factor

Actuals = ActualParameters
	|

set = "{" elementList "}"
elementList = element elementListTail
	|
elementListTail = "," element
	|

element = expression elementTail
elementTail = ".." expression
	|

ActualParameters = "(" OptionalExpList ")"
statement = AssignOrCall 
	| IfStatement 
	| CaseStatement 
	| WhileStatement 
	| RepeatStatement 
	| LoopStatement 
	| WithStatement 
	| EXIT 
	| RETURN ReturnVal
	|

OptionalExpList = ExpList
	|

ReturnVal = expression
	|

assignment = ":=" expression
ProcedureCall = Actuals

AssignOrCall = designator AssignOrCallTail
AssignOrCallTail = assignment
	| ProcedureCall

StatementSequence = statement StatementSequenceTail
StatementSequenceTail = ";" statement StatementSequenceTail
	|

IfStatement = IF expression THEN StatementSequence ElsifList ElseClause END
ElsifList = ELSIF expression THEN StatementSequence ElsifList
	|
ElseClause = ELSE StatementSequence
	|
CaseStatement = CASE expression OF Case caseList ElseClause END  
caseList = "|" Case caseList
	|

Case = CaseLabelList ":" StatementSequence 
	|

CaseLabelList = CaseLabels CaseLabelListTail
CaseLabelListTail = "," CaseLabels CaseLabelListTail
	|

CaseLabels = ConstExpression CaseLabelsTail
CaseLabelsTail = ".." ConstExpression
	|

WhileStatement = WHILE expression DO StatementSequence END
RepeatStatement = REPEAT StatementSequence UNTIL expression
LoopStatement = LOOP StatementSequence END
WithStatement = WITH qualident ":" qualident DO StatementSequence END
ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident
ProcedureHeading = OptionalStar identdef OptionalFormalParameters
ForwardDeclaration = "^" identdef OptionalFormalParameters
OptionalStar = "*"
	|
ProcedureBody = DeclarationSequence OptionalBody END
OptionalBody = BEGIN StatementSequence
	|

DeclarationSequence = FirstDeclList SecondDeclList
FirstDeclList = FirstDecl FirstDeclList
	|
FirstDecl = CONST ConstDeclList
	| TYPE TypeDeclList
	| VAR VarDeclList
ConstDeclList = ConstantDeclaration ";" ConstDeclList
	|
TypeDeclList = TypeDeclaration ";" TypeDeclList
	|
VarDeclList = VariableDeclaration ";" VarDeclList
	|
SecondDeclList = PROCEDURE SecondDecl SecondDeclList
	|
SecondDecl = ProcedureDeclaration ";" 
	| ForwardDeclaration ";"

FormalParameters = "(" FPSectionList ")" qualidentSuffix
FPSectionList = FPSection FPSectionTail
	|
FPSectionTail = ";" FPSection FPSectionTail
	|
FPSection = OptionalVar identList ":" FormalType
OptionalVar = VAR
	|
identList = ident identListTail
identListTail = "," ident identListTail
	|

FormalType = ArrayOfList qualident
ArrayOfList = ARRAY OF ArrayOfList
	|

qualidentSuffix = ":" qualident
	|
ImportList = IMPORT import ImportListTail ";"
	|
ImportListTail = "," import ImportListTail
	|

import = ident OptionalImportAssign
OptionalImportAssign = ":=" ident
	|
