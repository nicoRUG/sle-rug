module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

syntax Question
  = question:         Str Id ":" Type
  | computedQuestion: Str Id ":" Type "=" Expr
  | block:            "{" Question* "}"
  | ifThen:           "if" "(" Expr ")" Question
  | ifThenElse:       "if" "(" Expr ")" Question "else" Question
  ;

// C/Java style precedence rules used for operator precedence
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  > Str
  | Int
  | Bool
  > parenthesis:  "(" Expr ")"
  >        not:        "!" Expr
  > left (mult: Expr  "*"  Expr
  |        div: Expr  "/"  Expr)
  > left ( add: Expr  "+"  Expr
  |        sub: Expr  "-"  Expr)
  > left (  lt: Expr "\<"  Expr
  |        lte: Expr "\<=" Expr 
  |         gt: Expr "\>"  Expr
  |        gte: Expr "\>=" Expr)
  > left (  eq: Expr "=="  Expr
  |        neq: Expr "!="  Expr)
  > left   and: Expr "&&"  Expr
  > left    or: Expr "||"  Expr
  ;

syntax Type
  = "string"
  | "integer"
  | "boolean"
  ;  

lexical Str = "\"" ![\"]* "\""; 

//integers can be negative whole numbers, no leading zeros
lexical Int
  = zero:    "0"
  | nonZero: "-"?[1-9][0-9]*
  ;

lexical Bool = "true" | "false";