module Run_examples

import Syntax;
import ParseTree;
import CST2AST;
import Resolve; 
import Check;
import Eval;
import Compile;
import Transform;

import IO;


void runExamples(bool applyFlatten){
  examples_folder = |project://QL/examples|;
  entries = listEntries(examples_folder);
  errorList = [];
  //TODO: try catch
  for(e <- entries, /.*\.myql/ := e){
    src_file = find(e, [examples_folder]);
    parseTree = parse(#start[Form], src_file);
	ast = cst2ast(parseTree);
	refGraph = resolve(ast);
	tEnv = collect(ast);
	errorMsgs = check(ast, tEnv, refGraph.useDef);
	if(msg <- errorMsgs, error(_,_) := msg){
	println("did not compile <e> due to errors/warnings: <errorMsgs>\n");
	}else{
	if(applyFlatten) ast = flatten(ast);
	compile(ast);
	println("compiled <e>");
    } 
  }
  
}