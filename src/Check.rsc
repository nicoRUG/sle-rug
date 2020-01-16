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
    case question        (AId x, str label, AType t):          c += <x.src, "<x.name>", label, mapTypes(t)>;
    case computedQuestion(AId x, str label, AType t, AExpr _): c += <x.src, "<x.name>", label, mapTypes(t)>;
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
  switch(q){
    case block(list[AQuestion] questions): return ({} | check(i, tenv, useDef) + it | i <- questions);
    case ifThen    (AExpr cond, AQuestion ifTrue                   ): return check(cond, tenv, useDef) + check(ifTrue, tenv, useDef                               );
    case ifThenElse(AExpr cond, AQuestion ifTrue, AQuestion ifFalse): return check(cond, tenv, useDef) + check(ifTrue, tenv, useDef) + check(ifFalse, tenv, useDef);    
    case question        (AId id, str _, AType typ            ): return checkTypes(q, tenv) + checkDuplicateLabels(q, tenv)                                     ;
    case computedQuestion(AId id, str _, AType typ, AExpr expr): return checkTypes(q, tenv) + checkDuplicateLabels(q, tenv) + checkDeclaredType(q, tenv, useDef) + check(expr, tenv, useDef);
    default: throw "could not match <q>";
  }
}

// - error:   declared questions with the same name but different types.
set[Message] checkTypes(AQuestion q, TEnv tenv){
  assert(question(_,_,_) := q || computedQuestion(_,_,_,_) := q);
  m = {};
  for(e <- tenv){
    if (("<q.id.name>" == e.name) && (mapTypes(q.typ) != e.\type)){
          m += error("same name, but different type as <e.def>", q.src);
    }
  }
  return m;
}

// - error:   the declared type of computed questions does not match the type of the expression.
set[Message] checkDeclaredType(q, TEnv tenv, UseDef useDef){
  assert(computedQuestion(_,_,_,_) := q);
  if(typeOf(q.expr, tenv, useDef) != mapTypes(q.typ)){
    return {error("types dont match! declared type <toString(q.typ)> evaluates to <toString(typeOf(q.expr, tenv, useDef))>", q.src)};
  }
  return {};
}

// - warning: duplicate labels
set[Message] checkDuplicateLabels(AQuestion q, TEnv tenv){
  assert(question(_,_,_) := q || computedQuestion(_,_,_,_) := q);
  m = {};
  for(e <- tenv){
    if (q.label == e.label && "<q.id.name>" != e.name){
        m += warning("duplicate labels with <e.def>", q.src);
      }
  }
  return m;
}


// Check operand compatibility with operators
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  switch (e) {
    case ref(AId x): msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };
    case not (AExpr expr,       src = loc u): if (typeOf(expr, tenv, useDef) != tbool())  msgs += error("operator ! requires boolean", u);
    case mult(AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator * requires integer", u);
    case div (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator / requires integer", u);
    case add (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator + requires integer", u);
    case sub (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator - requires integer", u);
    case lt  (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator \< requires integer", u);
    case lte (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator \<= requires integer", u);
    case gt  (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator \> requires integer", u);
    case gte (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator \>= requires integer", u);
    case eq  (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator == requires integer", u);
    case neq (AExpr l, AExpr r, src = loc u): if (!haveType(tint (), l, r, tenv, useDef)) msgs += error("operator != requires integer", u);
    case and (AExpr l, AExpr r, src = loc u): if (!haveType(tbool(), l, r, tenv, useDef)) msgs += error("operator && requires boolean", u);
    case or  (AExpr l, AExpr r, src = loc u): if (!haveType(tbool(), l, r, tenv, useDef)) msgs += error("operator || requires boolean", u);
  }
  return msgs; 
}

bool haveType(Type t, AExpr l, AExpr r, TEnv tenv, UseDef useDef) =
  (t :=typeOf(l, tenv, useDef) && t := typeOf(r, tenv, useDef));

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(str x, src = loc u)):{ 
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }}
    case parenthesis(AExpr expr):return typeOf(expr, tenv, useDef);
    case not (AExpr expr):       return typeOf(expr, tenv, useDef) == tbool() ? tbool() : tunknown();
    case mult(AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tint () : tunknown();
    case div (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tint () : tunknown();
    case add (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tint () : tunknown();
    case sub (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tint () : tunknown();
    case lt  (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tbool() : tunknown();
    case lte (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tbool() : tunknown();
    case gt  (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tbool() : tunknown();
    case gte (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tbool() : tunknown();
    case eq  (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tbool() : tunknown();
    case neq (AExpr l, AExpr r): return haveType(tint (), l, r, tenv, useDef) ? tbool() : tunknown();
    case and (AExpr l, AExpr r): return haveType(tbool(), l, r, tenv, useDef) ? tbool() : tunknown();
    case or  (AExpr l, AExpr r): return haveType(tbool(), l, r, tenv, useDef) ? tbool() : tunknown();
    case string (str  _): return tstr ();
    case integer(int  _): return tint ();
    case boolean(bool _): return tbool();
  }
  return tunknown(); 
}
 
 Type mapTypes(AType t){
   return switch(t){
     case aint() : return tint();
     case astr() : return tstr();
     case abool(): return tbool();
     default : return tunknown();
   }
 }
 
 str toString(Type t){
  return switch(t){
     case tint    (): return "integer";
     case tstr    (): return "string";
     case tbool   (): return "boolean";
     case tunknown(): return "unknown";
  }
 }