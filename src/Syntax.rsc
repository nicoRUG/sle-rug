module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = question: Str Id ":" Type
  | computedQuestion: Str Id ":" Type "=" Expr
  | block: "{" Question* "}"
  | ifThenElse: "if" "(" Expr ")" Question "else" Question
  | ifThen: "if" "(" Expr ")" Question
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  > parenthesis: "(" Expr ")"
  > not: "!" Expr
  > left (mult:  Expr "*" Expr
  | div: Expr "/" Expr)
  > left ( add: Expr "+" Expr
  | sub: Expr "-" Expr)
  > left (lt: Expr "\<" Expr
  | lte: Expr "\<=" Expr 
  | gt: Expr "\>" Expr
  | gte: Expr "\>=" Expr)
  > left (eq: Expr "==" Expr
  | neq: Expr "!=" Expr)
  > left and: Expr "&&" Expr
  > left or: Expr "||" Expr
  > Str
  | Int
  | Bool
  ;
  
syntax Type
  = "string"
  | "integer"
  | "boolean"
  ;  
  
lexical Str = "\"" ![\"]* "\""; 

lexical Int //allow negative
  = zero: "0"
  | nonZero: [1-9][0-9]*
  ;

lexical Bool = "true" | "false";



