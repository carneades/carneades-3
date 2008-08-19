/*
Carneades Argumentation Library and Tools.
Copyright (C) 2008 Matthias Grabmair

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License version 3 (GPL-3)
as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for details.
 
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

package Carneades.Argument;

// General imports
import javafx.xml.*;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.FileInputStream;
import java.lang.System;
import java.lang.Float;
import java.net.URI;

// other Argument imports
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

// import Constants
import Carneades.Graph.GC.*;

// helper issue class for file loading in older LKIF format
class Issue {
	attribute statementId: String;
	attribute value: String;
	attribute assumption: Boolean;
	attribute standard: String;
}

public var builder: DocumentBuilder = DocumentBuilder {
	namespaceAware: true
	validating: true
	ignoringComments: false
}

/**
 * Saves a set of graphs to a file in the current LKIF format.
 */
public var saveGraphToFile = function(argumentGraphs: ArgumentGraph[], file: File): Void {

		var document = builder.createDocument();
		
		var lkifElement = document.createElement("lkif");

		lkifElement.addAttribute("version", "2.0.4");

		document.documentElement = lkifElement;

		document.documentElement.children = [
			Element {
				name: "argument-graphs"
				document: document
				children: [
					for (argumentGraph in argumentGraphs) {

			Element {
				name: "argument-graph"
				document: document
				attributes: [
					Attribute {
						name: "id"
						value: argumentGraph.id
					},
					Attribute {
						name: "title"
						value: argumentGraph.title
					}
				] // attributes
				children: [
					Element {
						name: "statements"
						document: document
						children: [
							for (s in argumentGraph.statements) {
								Element {
									name: "statement"
									document: document
									attributes: [
										Attribute {
											name: "id"
											value: s.id
										},
										Attribute {
											name: "value"
											value: s.value
										},
										Attribute {
											name: "assumption"
											value: {if (s.assumption) "true" else "false"}
										},
										Attribute {
											name: "standard"
											value: { 
												if (s.standard instanceof BestArgument) "BA"
												else if (s.standard instanceof Scintilla) "SE"
												else if (s.standard instanceof DialecticalValidity) "DV"
												else if (s.standard instanceof Preponderance) "PE"
												else if (s.standard instanceof BeyondReasonableDoubt) "BRD"
												else if (s.standard instanceof ClearAndConvincingEvidence) "CCE" 
												else ""
											}
										}
									]
									children: [
										Element { // atom element
											name: "s"
											document: document
											children: [
												Text {
													value: s.wff
												}
											]
										}
									] // children
								} // <s>
							} // statements
						] // children
					} // <statements>
					, Element {
						name: "arguments"
						document: document
						children: [
							for (a in argumentGraph.arguments) {
								Element {
									name: "argument"
									document: document
									attributes: [
										Attribute {
											name: "id"
											value: a.id
										}, 
										Attribute {
											name: "weight"
											value: a.weight.toString()
										},
										Attribute {
											name: "direction"
											value: { if (a.pro) "pro" else "con" }
										},
										Attribute {
											name: "scheme"
											value: a.scheme.id
										}
									] // attributes
									children: [
										Element {
											name: "conclusion"
											document: document
											attributes: [
												Attribute {
													name: "statement"
													value: a.conclusion.id
												}
											]
										},  // conclusion
										Element {
											name: "premises"
											document: document
											children: [
												for (p in a.premises) {
													Element {
														name: "premise"
														document: document
														attributes: [
															Attribute {
																name: "statement"
																value: p.statement.id
															}, 
															Attribute {
																name: "exception"
																value: { if (p.exception) "true" else "false"}
															},
															Attribute {
																name: "polarity"
																value: { if (p.negative) "negative" else "positive"}
															},
															Attribute {
																name: "role"
																value: p.role
															}
														] // attributes
													} // element
												} // for
											] // children
										} // premises
									] // children
								} // arument
							} // for
						] // children
					} // <arguments>
				] // children
			} // <argument-graph>
			} // for 
			] // children
			} // <argument-graphs>
		];

		// create a new file of the specified name
		file.createNewFile();

		// toDo: test canwrite()

		var writer: FileWriter = new FileWriter(file);

		var output: String = document.toString();

		writer.write(output);

		writer.close();

		System.out.println(document);
	}


/**
 * Load an array of model argument graphs from a file.
 */
public var getGraphFromFile = function(file: File): ArgumentGraph[] {

		// Preliminary: Parse file and do some safety checking. Currently none yet.
		var document = builder.parseFile(file);

		// I. Fork according to older LKIF formats
		if (sizeof document.getElementsByTagName("issue") > 0) return getGraphFromLkifV1Document(document);

		// II. Load the Argument Graphs according to the current format.
		var argumentGraphs: ArgumentGraph[] = [];

		var agraphs: Element[] = document.getElementsByTagName("argument-graph");

		for (agraph in agraphs) {
			var argumentGraph: ArgumentGraph = ArgumentGraph {};
			var statements: Element[];
			var arguments: Element[];
			var issues: Element[];
			var loadedIssues: Issue[];
			
			for (c: Node in agraph.children) {
				if (c.name == "statements") {
					statements = for (i in c.children where i.name == "statement") { i as Element };
				}
				if (c.name == "arguments") {
					arguments = for (i in c.children where i.name == "argument") { i as Element };
				}
			}
			
			// 1. Fetch Statements
			
			for (s in statements) {
				// declare variables
				var id: String;
				var wff: String;
				var assumption: Boolean;
				var value: String;
				var standard: ProofStandard;
				// toDo: Term component missing, but also not in current version of Argument.fx
				// also: predicates
				
				// set variables
				
				
				for (a in s.attributes) {
					if (a.name == "id") { id = a.value; }
					else if (a.name == "assumption") { assumption = { if (a.value == "true") true else false }}
					else if (a.name == "value") { value = a.value }
					else if (a.name == "standard") {
						standard = {
							if (a.value == "BA") BestArgument {}
							else if (a.value == "SE") Scintilla {}
							else if (a.value == "PE") Preponderance {}
							else if (a.value == "BRD") BeyondReasonableDoubt {}
							else if (a.value == "DV") DialecticalValidity {}
							else /*if (a.value == "CCE")*/ ClearAndConvincingEvidence {}
						}
					}
				}

				for (c in s.children) {
					if (c.name == "s") {
						wff = c.value
					}
				}
				
				var newStatement: Statement = Statement {
					graph: argumentGraph
					id: id
					wff: wff
					value: value
					assumption: assumption
					standard: standard
				}

				insert newStatement into argumentGraph.statements;
			}

			// 2. Fetch Arguments
			
			for (a in arguments) {
				// declare variables
				var id: String;
				var scheme: Scheme;
				var premises: Premise[];  
				var pro: Boolean = true;
				var conclusion: Statement;
				var weight: Number = 0.0;
				
				// set variables from attributes
				
				for (i in a.attributes) {
					if (i.name == "id") id = { i.value; }
					if (i.name == "direction") {
						if (i.value == "pro") { pro = true; }
						else /*(i.value == "con")*/ { pro = false; }
					}
					if (i.name == "scheme") 
						scheme = Scheme {
							id: i.value
						}
					if (i.name == "weight") { weight = Float.valueOf(i.value); }
					// correct weight for older files
					if (weight > 1.0) { weight = weight / 100; }
				}
				
				// fetch premises
				for (c in a.children) {
					if (c.name == "premises") {
						for (p in c.children) {
							if (p.name == "premise") {
								var statement: Statement;
								var negative: Boolean = false;
								var role: String = "";
								var exception: Boolean = false;
								
								for (i in (p as Element).attributes) {
									if (i.name == "polarity") {
										if (i.value == "positive") { negative = false; }
										else /*(i.value == "negative")*/  { negative = true; }
									}
									if (i.name == "role") { role = i.value; }
									if (i.name == "statement") {
										var temp = for (s in argumentGraph.statements 
															where s.id == i.value) { s };
										statement = temp[0];
									}
									if (i.name == "exception") { 
										if (i.value == "true") { exception = true } 
										else { exception = false; } }
								} // attributes
								
								var newPremise: Premise;
								
								newPremise = Premise {
									statement: statement
									negative: negative
									role: role
									exception: exception
								}
								
								insert newPremise into premises;
							} // premise
						} // children
					} // premises
					if (c.name == "conclusion") {
						for (i in (c as Element).attributes) {
							if (i.name == "statement") {
								var temp = for (s in argumentGraph.statements 
													where s.id == i.value) { s };
								conclusion = temp[0];
							}
						} // attributes
					}
				} // children
				
				var newArgument: Argument = Argument {
					id: id
					graph: argumentGraph
					scheme: scheme
					premises: premises  
					pro: pro
					conclusion: conclusion
					weight: weight
				}
				
				insert newArgument into argumentGraph.arguments;
				for (p in newArgument.premises) {
					insert newArgument into p.statement.arguments;
				}
			} // fetch arguments
			
			insert argumentGraph into argumentGraphs;
		} // for-loop iterating over the graphs in the file
		
		return argumentGraphs;
	}

/**
 * Load graph from old-format, issue-based LKIF document.
 */
var getGraphFromLkifV1Document = function(document: Document): ArgumentGraph[] { 
		var argumentGraph: ArgumentGraph = ArgumentGraph {};
		var agraph: Element = document.getElementsByTagName("argument-graph") [0];
		var statements: Element[];
		var arguments: Element[];
		var issues: Element[];
		var loadedIssues: Issue[];

		for (c: Node in agraph.children) {
			if (c.name == "statements") {
				statements = for (i in c.children where i.name == "s") { i as Element };
			}
			if (c.name == "arguments") {
				arguments = for (i in c.children where i.name == "argument") { i as Element };
			}
			if (c.name == "issues") {
				issues = for (i in c.children where i.name == "issue") { i as Element };
			}
		}

		// 1. Fetch Issues
		for (i in issues) {
			var id: String;
			var value: String;
			var assumption: Boolean;
			var standard: String;
			
			for (a in i.attributes) {
				if (a.name == "statement") { id = a.value; }
				if (a.name == "standard") { standard = a.value; }
				if (a.name == "assumption") { assumption = { if (a.value == "true") true else false }; }
				if (a.name == "value") { value = a.value; }
			}

			var newIssue: Issue = Issue {
				statementId: id
				value: value
				assumption: assumption
				standard: standard
			}

			insert newIssue into loadedIssues;
		}
		

		// 2. Fetch Statements

		for (s in statements) {
			// declare variables
			var id: String;
			var wff: String;
			// toDo: Term component missing, but also not in current version of Argument.fx
			// also: predicates

			// set variables
			wff = s.value;

			for (a in s.attributes) {
				if (a.name == "id") { id = a.value; }
			}

			var newStatement: Statement = Statement {
				graph: argumentGraph
				id: id
				wff: wff
			}

			// check whether there is an issue to override the defaults
			for (i in loadedIssues where i.statementId == newStatement.id) {
				newStatement.value = i.value;
				newStatement.assumption = i.assumption;

				var negated: Boolean = false;
				var complement: Boolean = false;
				var extractedStandard: String = i.standard;

				newStatement.standard = {
					if (extractedStandard == "SE") Scintilla { statement: newStatement }
					else if (extractedStandard == "DV") DialecticalValidity { statement: newStatement }
					else if (extractedStandard == "PE") Preponderance { statement: newStatement }
					else if (extractedStandard == "BRD") BeyondReasonableDoubt { statement: newStatement }
					else BestArgument { statement: newStatement } 
				};
			}
			insert newStatement into argumentGraph.statements;
		}

		// 3. Fetch Arguments

		for (a in arguments) {
			// declare variables
			var id: String;
			var scheme: Scheme;
			var premises: Premise[];  
			var pro: Boolean = true;
			var conclusion: Statement;
			var weight: Number = 0.0;

			// set variables from attributes

			for (i in a.attributes) {
				if (i.name == "id") id = { i.value; }
				if (i.name == "direction") {
					if (i.value == "pro") { pro = true; }
					else /*(i.value == "con")*/ { pro = false; }
				}
				if (i.name == "scheme") 
					scheme = Scheme {
						id: i.value
					}
				if (i.name == "weight") { weight = Float.valueOf(i.value); }
				// correct weight for older files
				if (weight > 1.0) { weight = weight / 100; }
			}
			
			// fetch premises
			for (c in a.children) {
				if (c.name == "premises") {
					for (p in c.children) {
						if (p.name == "premise") {
							var statement: Statement;
							var negative: Boolean = false;
							var role: String = "";
							var exception: Boolean = false;

							for (i in (p as Element).attributes) {
								if (i.name == "polarity") {
									if (i.value == "positive") { negative = false; }
									else /*(i.value == "negative")*/  { negative = true; }
								}
								if (i.name == "role") { role = i.value; }
								if (i.name == "statement") {
									var temp = for (s in argumentGraph.statements where s.id == i.value) { s };
									statement = temp[0];
								}
								if (i.name == "exception") { if (i.value == "true") { exception = true } else { exception = false; } }
							} // attributes
							
							var newPremise: Premise;

							newPremise = Premise {
								statement: statement
								negative: negative
								role: role
								exception: exception
							}

							insert newPremise into premises;
						} // premise
					} // children
				} // premises
				if (c.name == "conclusion") {
					for (i in (c as Element).attributes) {
						if (i.name == "statement") {
							var temp = for (s in argumentGraph.statements where s.id == i.value) { s };
							conclusion = temp[0];
						}
					} // attributes
				}
			} // children

			var newArgument: Argument = Argument {
				id: id
				graph: argumentGraph
				scheme: scheme
				premises: premises  
				pro: pro
				conclusion: conclusion
				weight: weight
			}

			insert newArgument into argumentGraph.arguments;
			for (p in newArgument.premises) {
				insert newArgument into p.statement.arguments;
			}
		} // fetch arguments
		
		return [argumentGraph];
	}


