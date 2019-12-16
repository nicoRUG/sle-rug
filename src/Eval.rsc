module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
// TODOOOOOOOO
  VEnv env = ();
  visit(f.questions){
    case question(AId _, str label, AType t, src = _) : env += (label:defaultValue(t));
    case computedQuestion(AId _, str label, AType t, AExpr _, src = _) : env += (label:defaultValue(t)); 
  }
  return env;
}

Value defaultValue(AType t){
  switch(t){
   case typ("integer") : return vint(0);
   case typ("string") : return vstr("");
   case typ("boolean") : return vbool(false);
  }
  assert false : "invalid type";
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  //input is single input for one question, right??? (TODO: clear this)
  //reevaluate every question
  //TODO
  visit(f){
    case AQuestion q: eval(q, inp, venv);
  }
  return (); 
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  //TODO
  VEnv env = venv;
  switch(q){
    case question(AId _, str _, AType _) : ; //TODO: also use input
    case computedQuestion(AId _, str _, AType _, AExpr expr) : env += eval(expr, venv);
    case block(list[AQuestion] questions): return (() | it + eval(q) | q <-questions);
    case ifThenElse(AExpr cond, AQustion ifTrue, AQuestion ifFalse):{
      if(eval(cond) == vbool(true)){
        return venv + eval(ifTrue, inp, venv);
      }else{
        return venv + eval(ifFalse, inp, venv);
      }
    }
    case ifThen(AExpr cond, AQustion ifTrue):{
      if(eval(cond) == vbool(true)){
        return venv + eval(ifTrue, inp, venv);
      }
      return venv;
    }
    default: assert false : "unmatched question";
  }
  return ();
}

//TODO: refactor code if possible
//evaluate values and nested expressions
Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case parenthesis(AExpr expr): return eval(expr, venv);
    case not(AExpr expr) : {
      vbool(val) = eval(l, venv);
      return vbool(!val);
    }
    case mult(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vint(lval*rval);
    }
    case div(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vint(lval/rval); //what to do about division by zero
    }
    case add(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vint(lval+rval);
    }
    case sub(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vint(lval-rval);
    }
    case lt(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vbool(lval<rval);
    }
    case lte(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vbool(lval<=rval);
    }
    case gt(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vbool(lval<rval);
    }
    case gte(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vbool(lval<rval);
    }
    case eq(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vbool(lval==rval);
    }
    case neq(AExpr l, AExpr r):{
      vint(lval) = eval(l, venv);
      vint(rval) = eval(r, venv);
      return vbool(lval!=rval);
    }
    case string(str s): return vstr(s);
    case integer(int i): return vstr(i);
    case boolen(bool b): return vstr(b);
    
    default: throw "Unsupported expression <e>";
  }
}