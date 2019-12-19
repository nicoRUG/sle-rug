module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
//TODO: fix script string in
  questions = div([question2html(x) | x <- f.questions]);

  return html(
           head(
             title("<f.name>")), 
           body(
             div(
               script(src("<f.name>.js")),
               questions
             )
           )
         );//div([question2html(q) | q <- f.questions])
}

HTML5Node question2html(AQuestion q){
  switch(q){
    //TODO: write function that depending on type, returns the corresponding html node
    //TODO implement computed question like question, but make read only
    //TODO implement ifThen/ifThenElse and make visible only if branch evaluates to true
    case question        (AId id, str label, aint()):                return div(input(\type("number"),   \name("<id.name>")), "<label>");
    case question        (AId id, str label, astr()):                return div(input(\type("text"),     \name("<id.name>")), "<label>");
    case question        (AId id, str label, abool()):               return div(input(\type("checkbox"), \name("<id.name>")), "<label>");
    //TODO: change
    case computedQuestion(AId id, str label, AType typ, AExpr expr): return div(button("TODO"));
    case block(list[AQuestion] qs): return div([question2html(qu) |qu <- qs]);
    //TODO: make only one visible
    case ifThen    (AExpr cond, AQuestion t)             : return div(question2html(t));
    case ifThenElse(AExpr cond, AQuestion t, AQuestion f): return div(question2html(t), question2html(f));
    default: throw "could not match question <q>";
  }
}

str form2js(AForm f) {
  return "";
}
