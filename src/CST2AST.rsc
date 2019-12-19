module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  if((Form) `form<Id x>{<Question* q>}` := f){
    return form("<x>", [cst2ast(i) | i <- q], src=f@\loc);
  }
    throw "Cant match form: <f>";
  }

AQuestion cst2ast(Question q) {
  switch (q) {
    case (Question) `<Str l><Id x>:<Type t>`:                      return question        (id("<x>", src=x@\loc), "<l>"[1..-1], cst2ast(t),                src=q@\loc);
    case (Question) `<Str l><Id x>:<Type t>=<Expr expr>`:          return computedQuestion(id("<x>", src=x@\loc), "<l>"[1..-1], cst2ast(t), cst2ast(expr), src=q@\loc);
    case (Question) `{<Question* qu>}`:                            return block           ([cst2ast(i) | i <- qu], src=q@\loc);
    case (Question) `if(<Expr cond>)<Question ifTrue>`:            return ifThen          (cst2ast(cond), cst2ast(ifTrue),        src=q@\loc);
    case (Question) `if(<Expr cond>)<Question t>else<Question f>`: return ifThenElse      (cst2ast(cond), cst2ast(t), cst2ast(f), src=q@\loc);
    default: throw "Cant match question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`:              return ref(id("<x>", src=x@\loc),   src=x@\loc);
    case (Expr)`(<Expr exp>)`:        return parenthesis(cst2ast(exp),    src=e@\loc);
    case (Expr)`!<Expr exp>`:         return not        (cst2ast(exp),    src=e@\loc);
    case (Expr)`<Expr l>*<Expr r>`:   return mult(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>/<Expr r>`:   return div (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>+<Expr r>`:   return add (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>-<Expr r>`:   return sub (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>\<<Expr r>`:  return lt  (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>\<=<Expr r>`: return lte (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>\><Expr r>`:  return gt  (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>\>=<Expr r>`: return gte (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>==<Expr r>`:  return eq  (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>!=<Expr r>`:  return neq (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>&&<Expr r>`:  return and (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l>||<Expr r>`:  return or  (cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`true`:                return boolean(true,                src=e@\loc);
    case (Expr)`false`:               return boolean(false,               src=e@\loc);
    case (Expr)`<Int i>`:             return integer(toInt("<e>"),        src=e@\loc);
    case (Expr)`<Str s>`:             return string ("<e>"[1..-1],        src=e@\loc);
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t){
  switch(t){
    case (Type)`integer`: return aint(src=t@\loc);
    case (Type)`string`:  return astr(src=t@\loc);
    case (Type)`boolean`: return abool(src=t@\loc);
    default: throw "invalid type: <t>";
  }
}