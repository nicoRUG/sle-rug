module Check

import AST;
import Resolve;
import Message; // see standard library

/*
 * Type Checker for QL
 */

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// return the type environment of the form 
TEnv collect(AForm f) {
  c = {};
  visit(f){
    case question        (AId x, str label, AType t) : c += <x.src, "<x.name>", label, matchTypes(t)>;
    case computedQuestion(AId x, str label, AType t, AExpr expr) : c += <x.src, "<x.name>", label, matchTypes(t)>;
     }
  return c; 
}

//check the form for type correctness and return error messages
set[Message] check(AForm f, TEnv tenv, UseDef useDef) = ({} | it + check(i, tenv, useDef) | i <- f.questions);

//produces the following messages:
// - error:   declared questions with the same name but different types.
// - error:   the declared type of computed questions does not match the type of the expression.
// - warning: duplicate labels
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
//TODO: check and possibly refactor/improve readability of this function
  m = {};
  for(e <- tenv<name,\type, label>){
    if (("<q.id>" == e.name) && (matchTypes(q.typ) != e.\type)){
      m += error("multiple questions with same name but different type", q.src);
    }
    if (q.label == e.label && "<q.id.name>" != e.name){
      m += warning("duplicate labels", q.src);
    }
  }
  if (computedQuestion(_,_,_,_) := q && (typeOf(q.expr, tenv, useDef) != matchTypes(q.typ))){
      m += error("types dont match: TypeOf: <typeOf(q.expr, tenv, useDef)>,  q.typ: <matchTypes(q.typ)>", q.src);
  }
  return m; 
}

// Check operand compatibility with operators
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  //TODO: add location information
  switch (e) {
    case ref(AId x): msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };
    case not (AExpr expr, src = loc u) : if (typeOf(expr, tenv, useDef) != tbool()) msgs += error("not requires boolean", u);
    case mult(AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("mult requires int", u);
    case div (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("div requires int", u);
    case add (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("add requires int", u);
    case sub (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("sub requires int", u);
    case lt  (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("lt requires int", u);
    case lte (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("lte requires int", u);
    case gt  (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("gt requires int", u);
    case gte (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("gte requires int", u);
    case eq  (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("eq requires int", u);
    case neq (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tint()  && typeOf(r, tenv, useDef) == tint()))  msgs += error("neq requires int", u);
    case and (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tbool() && typeOf(r, tenv, useDef) == tbool())) msgs += error("and requires bool", u);
    case or  (AExpr l, AExpr r, src = loc u): if (!(typeOf(l, tenv, useDef) == tbool() && typeOf(r, tenv, useDef) == tbool())) msgs += error("sub requires bool", u);
  }
  return msgs; 
}

//TODO: came until this point at my revision
Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(str x, src = loc u)):{ 
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }}
    case parenthesis(AExpr expr):return typeOf(expr, tenv, useDef);
    case not (AExpr expr):       return typeOf(expr, tenv, useDef) == tbool() ? tbool() : tunknown();
    case mult(AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tint() : tunknown();
    case div (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tint() : tunknown();
    case add (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tint() : tunknown();
    case sub (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tint() : tunknown();
    case lt  (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tbool() : tunknown();
    case lte (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tbool() : tunknown();
    case gt  (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tbool() : tunknown();
    case gte (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tbool() : tunknown();
    case eq  (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tbool() : tunknown();
    case neq (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tint() && typeOf(r, tenv, useDef) == tint() ? tbool() : tunknown();
    case and (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tbool() && typeOf(r, tenv, useDef) == tbool() ? tbool() : tunknown();
    case or  (AExpr l, AExpr r): return typeOf(l, tenv, useDef) == tbool() && typeOf(r, tenv, useDef) == tbool() ? tbool() : tunknown();
    case string(str _):  return tstr();
    case integer(int _): return tint();
    case boolean(bool _):return tbool();
    // etc.
  }
  return tunknown(); 
}
 
 \Type matchTypes(AType t){
   switch(t){
     case aint() : return tint();
     case astr() : return tstr();
     case abool(): return tbool();
     default : return tunknown();
   }
 }