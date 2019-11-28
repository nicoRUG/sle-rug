module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  //TODO: remove switfch
   switch (f){
    case (Form)`form<Id x>{<Question* q>}`: return form("<x>", [cst2ast(i) | i <- q], src=f@\loc);
  }
  throw "implementation fail";
  //return form("", [cst2ast(i) | i <- q, f:=(Form)`form<Id x>{<Question* q>}` ]); 
}

//TODO: do we have to add the location as last parameter for every AST constructor?

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question)`<Str _><Id x>:<Type t>`: return question(id("<x>", src=x@\loc), cst2ast(t), src=x@\loc);
    case (Question)`<Str _><Id x>:<Type t>=<Expr expr>`: return computedQuestion(id("<x>", src=x@\loc), cst2ast(t), cst2ast(expr), src=x@\loc);
    case (Question)`{<Question* qu>}`: return block([cst2ast(i) | i <- qu]);
    case (Question)`if(<Expr cond>)<Question ifTrue>else<Question ifFalse>`: return ifThenElse(cst2ast(cond), cst2ast(ifTrue), cst2ast(ifFalse));
    case (Question)`if(<Expr cond>)<Question ifTrue>`: return ifThen(cst2ast(cond), cst2ast(ifTrue));
    default: throw "Cant match question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc), src=x@\loc);
    
    // etc.
    case (Expr)`true`: return boolean(true);
    case (Expr)`false`: return boolean(false);
    //case Int: return integer(e); //TODO
    //case Str: return string(e);
    default: throw "Unhandled expression: <e>";
  }
}

//TODO: this might have to be changed
AType cst2ast(Type t){
  return typ("<t>", src=t@\loc);
}
