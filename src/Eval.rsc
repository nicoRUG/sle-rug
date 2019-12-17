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
// choice of default values in defaultValue()
VEnv initialEnv(AForm f) {
  //initialise environment with default values
  VEnv venv = ();
  for(q <- f.questions){ //TODO: do we need this for loop?
    visit(q){
      case question(AId id(str x), str _, AType t, src = _) : venv += (x:defaultValue(t));
      case computedQuestion(AId id(str x), str _, AType t, AExpr _, src = _) : venv += (x:defaultValue(t)); 
    }
  }
  return venv;
}

Value defaultValue(AType t){
  switch(t){
   //use 1 as default value for ints to prevent Divison by Zero Exceptions caused by default values
   case typ("integer") : return vint(1);
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
  //note that the environment will only be updated AFTER each question was evaluated
  return (venv | it + eval(q, inp, venv) | q <- f.questions);
}

//passed tests:
//eval(question(id("i"), "l", typ("string")), input("l",vstr("test")), ());
//eval(computedQuestion(id("i"), "l", typ("integer"), mult(integer(2),integer(4))), input("l",vint(1)), ());
VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  switch(q){
    
    case question(id(str x), str _, AType _): {
      if(input(x, val) := inp){ //TODO: check this: label is fixed, val is free
        return venv + (x:val);  //TODO: this could be one level higher as well
      }
      return venv;
    }
    
    case computedQuestion(id(str x), str _, AType _, AExpr expr) : return venv + (x:eval(expr, venv));
    
    case block(list[AQuestion] questions): return (() | it + eval(q, inp, venv) | q <-questions);
    
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
    
    default: assert false : "unmatched question <q>";
  }
}

//TODO: refactor code if possible
//evaluate values and nested expressions
Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case parenthesis(AExpr expr): return eval(expr, venv);
    case not(AExpr expr) : {
      //TODO for all coming cases: how to correctly access val??
      vbool(val) = eval(expr, venv);
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
      //Check for division by zero
      if(rval == vint(0)){
        throw "Division by zero at <r.src>";
      }
      return vint(lval/rval);
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
    case integer(int i): return vint(i);
    case boolean(bool b): return vbool(b);
    
    default: throw "Unsupported expression <e>";
  }
}