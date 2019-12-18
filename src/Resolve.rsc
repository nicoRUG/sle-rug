module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

Use uses(AForm f) {
  Use u = {};
  //visit all variable uses
  visit(f){
    case ref(AId x) : u += {<x.src, x.name>};
  }
  return u; 
}

Def defs(AForm f) {
  Def d = {};
  //visit all (computed)questions to find variable definitions
  visit(f){
    case question(AId x, _, _) : d += {<x.name, x.src>};
    case computedQuestion(AId x, _, _, _) : d += {<x.name, x.src>};
 }
 return d;
}