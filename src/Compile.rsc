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

// to avoid generating code that contains keywords, prepend a "js_" to every js variable and function and "html_" for html name attributes

//TODO: the toString method for html5node will write html attribute values as raw text as well in th "script" tag.
// see l.361 of lang::thml5::DOM.rsc


//use VueJS as framework
void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  html_str = toString(form2html(f));
  
  //TODO: remove when script tag problem is properly fixed
  html_str = replaceAll(html_str,"html5attr(\"type\",\"text/javascript\")"              , "");
  html_str = replaceAll(html_str,"html5attr(\"src\",\"vue.js\")"                       , "");
  html_str = replaceAll(html_str,"html5attr(\"type\",\"<f.src[extension="js"].file>\")" , "");
  
  writeFile(f.src[extension="html"].top, html_str);
}

HTML5Node form2html(AForm f) {
  questionsAST = div([question2html(x) | x <- f.questions]);
  
  //html boilerplate
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
  return htmlAST;
}

HTML5Node question2html(AQuestion q){
  switch(q){
    case question        (AId id, str label, AType t         ): return div(label, br(),input(inputAttr(id, t)                   ));
    case computedQuestion(AId id, str label, AType t, AExpr _): return div(label, br(),input(inputAttr(id, t) + readonly("true")));
    case block(list[AQuestion] qs): return div([question2html(qu) |qu <- qs]);
    //since there is no id, use location offset to indentify ITE construct
    case ifThen    (AExpr _, AQuestion t, src=x)             : return div(    html5attr("v-if", "js_<x.offset>"), question2html(t));
    case ifThenElse(AExpr _, AQuestion t, AQuestion f, src=x): return div(div(html5attr("v-if", "js_<x.offset>"), question2html(t)), div(html5attr("v-if", "!js_<x.offset>"), question2html(f)));
    default: throw "could not match question <q>";
  }
}

//return list with attributes for the <input> tag
list[value] inputAttr(AId id, AType t) =
  [\type(mapInputType(t)), \name("html_<id.name>"), html5attr("v-model<t := aint()?".number":"">", "js_<id.name>")];

//maps AType to the matching html input type attribute value
str mapInputType(AType t){ 
  switch(t){
    case aint (): return "number";
    case astr (): return "text";
    case abool(): return "checkbox";
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

//returns default value string for AType
str defaultValue(AType t){
  switch(t){
   //TODO: use 1 as default value for ints to prevent Divison by Zero Exceptions caused by default values?
   case aint() : return "0";
   case astr() : return "\"\"";
   case abool() : return "false";
  }
}

//evaluate AEXpr to javascript string
str expr2js(AExpr expr){
  switch(expr){
    case ref(AId x): return "data.js_<x.name>";
    case parenthesis(AExpr e):   return "(<expr2js(e)>)";
    case not (AExpr e):          return "!<expr2js(e)>";
    case mult(AExpr l, AExpr r): return "<expr2js(l)> * <expr2js(r)>";
    //use integer division, since only integers are supported
    //division by zero is not explicetly handled since it doesnt throw an error
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
    case string(str s):  return "\"<s>\"";
    case integer(int i): return "<i>";
    case boolean(bool b):return "<b>"; 
  }
}

//collect questions and produce data object for Vue
str jsDataObject(AForm f){
  data_content = "";
  //TODO: can you use a deep match here as well?
  visit(f){
      case question(AId id, str label, AType typ): 
        data_content += "  js_<id.name> : <defaultValue(typ)>,\n";
   }
        
  return
    "var data = {
    '<data_content>}";
}

//collect computedQuestions and produce computed object for Vue
str jsComputedObject(AForm f){
  computed_content = "";
  visit(f){
      case computedQuestion(AId id, str label, AType typ, AExpr expr): 
      //QL computed questions map to JS computed functions
        computed_content += 
          "  js_<id.name> : function(){
          '    return <expr2js(expr)>
          '  },
          '";
      //QL conditions map to JS functions that compute the condition
      case ifThen(AExpr cond, _, src=x): 
        computed_content +=
          "  js_<x.offset> : function(){
          '    return <expr2js(cond)>
          '  },
          '";
      case ifThenElse(AExpr cond, _, _, src=x): 
        computed_content +=
          "  js_<x.offset> : function(){
          '    return <expr2js(cond)>
          '  },
          '";
   }
    
  return
    "var computed = {
    '<computed_content>}";
}