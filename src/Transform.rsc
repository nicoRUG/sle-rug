module Transform

import Syntax;
import Resolve;
import AST;

extend lang::std::Id;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */

AForm flatten(AForm f) {
  //TODO: refactor
  //TODO: flatten out blocks
  //create ifThen for every (computed)question
  f.questions = [flattenAux(q, boolean(true)) | q <- f.questions];
  //flatten all blocks
  f.questions = ([] | it + flattenBlocks(q) | q <- f.questions);
  return f; 
}

//TODO: put flatten blocks directly in this function?
AQuestion flattenAux(AQuestion q, AExpr conds){
//TODO; test/check
  switch(q){
    //recursively apply the flatten method on the statement and AND the condition with the conditions given by the parameter
    case ifThen    (AExpr cond, AQuestion ifTrue)                   : return        flattenAux(ifTrue, and(conds, cond))                                              ;
    //the same as ifThen, but also flatten for the false statement and negate the condition there
    case ifThenElse(AExpr cond, AQuestion ifTrue, AQuestion ifFalse): return block([flattenAux(ifTrue, and(conds, cond)), flattenAux(ifFalse, and(conds, not(cond)))]);
    //recursively apply method for all childs, block structures will be flattened out in the end
    case block(list[AQuestion] questions): return block([flattenAux(qs, conds) | qs <- questions]); //TODO: flatten block afterwards
    //default case: (computed)questions form the base cases of the recusion, they are returned with an ifThen, containing all applying conditions
    default: return ifThen(conds, q, src=q.src);
    //note: quick&dirty fix: src location is used to identify ifThen in Compile -> use same location as the question
  }
}

list[AQuestion] flattenBlocks(block(list[AQuestion] qList)) = ([] | it + flattenBlocks(q) | q <- qList);

list[AQuestion] flattenBlocks(AQuestion q) = [q];


/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
 
 //rename refactoring of names (variables)
 //does not preserve layout
 start[Form] rename(start[Form] f, loc useOrDef, str newName, RefGraph refs) {
   Id newId = [Id]newName;

   return visit(f){
     case (Question) `<Str l><Id x>:<Type t>`
       => (Question) `<Str l> <Id newId>: <Type t>`
       when   
         //(another) definition of the variable is renamed
         (<old, useOrDef> <- refs.defs &&
          <old, x@\loc>   in refs.defs) ||
         //or a use of the variable is renamed
         <useOrDef, x@\loc> in refs.useDef
         //note: since there can be definitions without uses, we cant use the useDef graph
         
     case (Question) `<Str l><Id x>:<Type t>=<Expr expr>`
       => (Question) `<Str l> <Id newId> :<Type t> = <Expr expr>`
       when
         //a definition of the same variable is renamed
         (<old, useOrDef> <- refs.defs &&
          <old, x@\loc>   in refs.defs) ||
         //or a use of the variable is renamed
         <useOrDef, x@\loc> in refs.useDef

     case (Expr) `<Id x>`
       => (Expr) `<Id newId>`
       when
         //a definition of variable is renamed
         <x@\loc, useOrDef> in refs.useDef ||
         //or (another) use of the variable is renamed
         (<useOrDef, def> <- refs.useDef &&
          <x@\loc, def>   in refs.useDef)
   }
 }