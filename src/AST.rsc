module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(AId id, AType typ)
  | computedQuestion(AId id, AType typ, AExpr expr)
  | block(list[AQuestion] questions)
  | ifThenElse(AExpr cond, AQuestion ifTrue, AQuestion ifFalse)
  | ifThen(AExpr cond, AQuestion ifTrue)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | parenthesis(AExpr expr)
  | not(AExpr expr)
  | mult(AExpr l, AExpr r)
  | div(AExpr l, AExpr r)
  | add(AExpr l, AExpr r)
  | sub(AExpr l, AExpr r)
  | lt(AExpr l, AExpr r)
  | lte(AExpr l, AExpr r)
  | gt(AExpr l, AExpr r)
  | gte(AExpr l, AExpr r)
  | eq(AExpr l, AExpr r)
  | neq(AExpr l, AExpr r)
  | and(AExpr l, AExpr r)
  | or(AExpr l, AExpr r)
  //like so??, or without constructor?
  | string(str string)
  | integer(int integer)
  | boolean(bool boolean)
  ;

data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
  = typ(str name);
  
