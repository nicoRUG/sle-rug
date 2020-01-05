module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

import String;

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

//use VueJS as framework
void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
//TODO: fix script string in
  questionsAST = div([question2html(x) | x <- f.questions]);

  htmlAST = html(
           head(
             title("<f.name>"),
             script(\type("text/javascript"), src("vue.js"))
           ),
           body(
             div(id("vue_form"), questionsAST),
             script(\type("text/javascript"), src("<f.src[extension="js"].file>"))
           )
         );
         //div([question2html(q) | q <- f.questions])
  return htmlAST;
}

HTML5Node question2html(AQuestion q){
  switch(q){
    //TODO: write function that depending on type, returns the corresponding html node
    //TODO: refactor question and computed question
    //TODO implement ifThen/ifThenElse and make visible only if branch evaluates to true
    case question        (AId id, str label, aint ()            ): return div(label, br(), input(\type("number")  , \name("<id.name>"), html5attr("v-model.number", "<id.name>")));
    case question        (AId id, str label, astr ()            ): return div(label, br(), input(\type("text")    , \name("<id.name>"), html5attr("v-model"       , "<id.name>")));
    case question        (AId id, str label, abool()            ): return div(label, br(), input(\type("checkbox"), \name("<id.name>"), html5attr("v-model"       , "<id.name>")));
    case computedQuestion(AId id, str label, aint (), AExpr expr): return div(label, br(), input(\type("number")  , \name("<id.name>"), html5attr("v-model.number", "<id.name>"), readonly("true")));
    case computedQuestion(AId id, str label, astr (), AExpr expr): return div(label, br(), input(\type("text")    , \name("<id.name>"), html5attr("v-model"       , "<id.name>"), readonly("true")));
    case computedQuestion(AId id, str label, abool(), AExpr expr): return div(label, br(), input(\type("checkbox"), \name("<id.name>"), html5attr("v-model"       , "<id.name>"), readonly("true")));
    case block(list[AQuestion] qs): return div([question2html(qu) |qu <- qs]);
    //TODO: make only one visible
    //TODO: find another way to implement this
    //since there is no id, use location offset to indentify ITE construct
    case ifThen    (AExpr _, AQuestion t, src=x)             : return div(html5attr("v-if",  "_<x.offset>"), question2html(t));
    case ifThenElse(AExpr _, AQuestion t, AQuestion f, src=x): return div(div(html5attr("v-if", "_<x.offset>"), question2html(t)), div(html5attr("v-if", "!_<x.offset>"), question2html(f)));
    default: throw "could not match question <q>";
  }
}

str form2js(AForm f) {
  //create Vue instance and assign data and computed attributes
  return 
    "<jsDataObject(f)>
    '
    '<jsComputedObject(f)>
    '
    'var vm = new Vue({
    '  el: \"#vue_form\",
    '  data: data,
    '  computed: computed,
    '})";
}

//returns default string for AType
str defaultValue(AType t){
  switch(t){
   //use 1 as default value for ints to prevent Divison by Zero Exceptions caused by default values
   case aint() : return "1";
   case astr() : return "\"\"";
   case abool() : return "false";
  }
}

//evaluate AEXpr to javascript string
str expr2js(AExpr expr){
  //currently division is integer division
  //TODO: integer division VS only allowing divisions with integer result
  //TODO: check and handle division by zero?
  switch(expr){
    case ref(AId x): return "data.<x.name>";
    case parenthesis(AExpr e):   return "(<expr2js(e)>)";
    case not (AExpr e):          return "!<expr2js(e)>";
    case mult(AExpr l, AExpr r): return "<expr2js(l)> * <expr2js(r)>";
    case div (AExpr l, AExpr r): return "parseInt(<expr2js(l)> / <expr2js(r)>)";
    case add (AExpr l, AExpr r): return "<expr2js(l)> + <expr2js(r)>";
    case sub (AExpr l, AExpr r): return "<expr2js(l)> - <expr2js(r)>";
    case lt  (AExpr l, AExpr r): return "<expr2js(l)> \< <expr2js(r)>";
    case lte (AExpr l, AExpr r): return "<expr2js(l)> \<= <expr2js(r)>";
    case gt  (AExpr l, AExpr r): return "<expr2js(l)> \> <expr2js(r)>";
    case gte (AExpr l, AExpr r): return "<expr2js(l)> \>= <expr2js(r)>";
    case eq  (AExpr l, AExpr r): return "<expr2js(l)> == <expr2js(r)>";
    case neq (AExpr l, AExpr r): return "<expr2js(l)> != <expr2js(r)>";
    case and (AExpr l, AExpr r): return "<expr2js(l)> && <expr2js(r)>";
    case or  (AExpr l, AExpr r): return "<expr2js(l)> || <expr2js(r)>";
    case string(str s):  return s;
    case integer(int i): return "<i>";
    case boolean(bool b):return "<b>"; 
  }
}

//collect questions and produce data object for Vue
str jsDataObject(AForm f){
  data_content = "";
  visit(f){
      case question(AId id, str label, AType typ): 
        data_content += "  <id.name> : <defaultValue(typ)>,\n";
   }

  str \data = 
    "var data = {
    '<data_content>}";
    
  return \data;
}

//collect computedQuestions and produce computed object for Vue
str jsComputedObject(AForm f){
  computed_content = "";
  visit(f){
      //computed questions map to computed functions
      case computedQuestion(AId id, str label, AType typ, AExpr expr): 
        computed_content += 
          "  <id.name> : function(){
          '    return <expr2js(expr)>
          '  },
          '";
      //conditions map to functions that compute the condition
      case ifThen(AExpr cond, _, src=x): 
        computed_content +=
          "  _<x.offset> : function(){
          '    return <expr2js(cond)>
          '  },
          '";
      case ifThenElse(AExpr cond, _, _, src=x): 
        computed_content +=
          "  _<x.offset> : function(){
          '    return <expr2js(cond)>
          '  },
          '";
   }

  str computed = 
    "var computed = {
    '<computed_content>}";
    
  return computed;
}