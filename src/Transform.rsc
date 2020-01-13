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
  return f; 
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
 
 start[Form] rename(start[Form] f, loc useOrDef, str newName, RefGraph refs) { 
   Id newId = [Id]newName;

   return visit(f){
     case (Question) `<Str l><Id x>:<Type t>`
       => (Question) `<Str l><Id newId>:<Type t>`
       when   
         //(another) definition of the variable is renamed
         (<old, useOrDef> <- refs.defs &&
          <old, x@\loc>   in refs.defs) ||
         //or a use of the variable is renamed
         <useOrDef, x@\loc> in refs.useDef
         
     case (Question) `<Str l><Id x>:<Type t>=<Expr expr>`
       => (Question) `<Str l><Id newId>:<Type t>=<Expr expr>`
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
 
 
 

