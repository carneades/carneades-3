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

package carneadesgui.control;

import carneadesgui.model.Argument;
import carneadesgui.model.Argument.*;
import carneadesgui.view.CarneadesGraph;
import carneadesgui.control.XWDocumentBuilder;
import carneadesgui.control.XWDocumentBuilder.*;
import java.io.File;
import java.io.FileWriter;
import java.lang.System;


// helper issue class for file loading in older LKIF format
class Issue {
	var statementId: String;
	var value: String;
	var assumption: Boolean;
	var standard: String;
}

public var builder: XWDocumentBuilder = XWDocumentBuilder {}

/**
 * Saves a set of graphs to a file in the current LKIF format.
 */
public var saveGraphToFile = function(argumentGraphs: ArgumentGraph[], file: File): Void {

		var document: XWDocument = XWDocument {
			version: '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
			schema: '<?oxygen RNGSchema="../../schemas/LKIF2.rnc" type="compact"?>'
		};

		document.documentElement = XWElement {
			name: "lkif"
			parent: document
			document: document
			attributes: [
				XWAttribute {
					document: document
					name: "version"
					value: "2.0.4"
				}
			]
		}

		document.documentElement.children = [
			XWElement {
				name: "argument-graphs"
				document: document
				children: [
					for (argumentGraph in argumentGraphs) {

			XWElement {
				name: "argument-graph"
				document: document
				attributes: [
					XWAttribute {
						name: "id"
						value: argumentGraph.id
					},
					XWAttribute {
						name: "title"
						value: argumentGraph.title
					}
				] // attributes
				children: [
					XWElement {
						name: "statements"
						document: document
						children: [
							for (s in argumentGraph.statements) {
								XWElement {
									name: "statement"
									document: document
									attributes: [
										XWAttribute {
											name: "id"
											value: s.id
										},
										XWAttribute {
											name: "value"
											value: s.value
										},
										XWAttribute {
											name: "assumption"
											value: {if (s.assumption) "true" else "false"}
										},
										XWAttribute {
											name: "standard"
											value: {
												if (s.standard instanceof Preponderance) "PE"
												else if (s.standard instanceof Scintilla) "SE"
												else if (s.standard instanceof DialecticalValidity) "DV"
												else if (s.standard instanceof BeyondReasonableDoubt) "BRD"
												else if (s.standard instanceof ClearAndConvincingEvidence) "CCE"
												else ""
											}
										}
									]
									children: [
										XWElement { // atom element
											name: "s"
											document: document
											children: [
												XWText {
													value: s.wff
												}
											]
										}
									] // children
								} // <s>
							} // statements
						] // children
					} // <statements>
					, XWElement {
						name: "arguments"
						document: document
						children: [
							for (a in argumentGraph.arguments) {
								XWElement {
									name: "argument"
									document: document
									attributes: [
										XWAttribute {
											name: "id"
											value: a.id
										},
										XWAttribute {
											name: "weight"
											value: a.weight.toString()
										},
										XWAttribute {
											name: "direction"
											value: { if (a.pro) "pro" else "con" }
										},
										XWAttribute {
											name: "scheme"
											value: a.scheme.id
										}
									] // attributes
									children: [
										XWElement {
											name: "conclusion"
											document: document
											attributes: [
												XWAttribute {
													name: "statement"
													value: a.conclusion.id
												}
											]
										},  // conclusion
										XWElement {
											name: "premises"
											document: document
											children: [
												for (p in a.premises) {
													XWElement {
														name: "premise"
														document: document
														attributes: [
															XWAttribute {
																name: "statement"
																value: p.statement.id
															},
															XWAttribute {
																name: "exception"
																value: { if (p.exception) "true" else "false"}
															},
															XWAttribute {
																name: "polarity"
																value: { if (p.negative) "negative" else "positive"}
															},
															XWAttribute {
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
	}


/**
 * Load an array of model argument graphs from a file.
 */
public var getGraphFromFile = function(file: File): ArgumentGraph[] {

		// Preliminary: Parse file and do some safety checking. Currently none yet.
		var document = builder.parseFile(file);

		// I. Fork according to older LKIF formats
		if (sizeof document.getElementsByTagName("issue") > 0) {
			return getGraphFromLkifV1Document(document);
		}

		// II. Load the Argument Graphs according to the current format.
		var argumentGraphs: ArgumentGraph[] = [];

		var agraphs: XWElement[] = document.getElementsByTagName("argument-graph");

		for (agraph in agraphs) {
			var graphId: String = "NewGraph";
			var graphTitle: String = "New Graph";
			
			for (a in agraph.attributes) {
				if (a.name == "id") graphId = a.value;
				if (a.name == "title") graphTitle = a.value;
			}

			var argumentGraph: ArgumentGraph = ArgumentGraph {
				id: graphId
				title: graphTitle
			}

			var statements: XWElement[];
			var arguments: XWElement[];
			var issues: XWElement[];
			var loadedIssues: Issue[];

			for (c: XWNode in agraph.children) {
				if (c.name == "statements") {
					statements = for (i in c.children where i.name == "statement") { i as XWElement };
				}
				if (c.name == "arguments") {
					arguments = for (i in c.children where i.name == "argument") { i as XWElement };
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

				for (a in s.attributes) {
					if (a.name == "id") { id = a.value; }
					else if (a.name == "assumption") { assumption = { if (a.value == "true") true else false }}
					else if (a.name == "value") { value = a.value }
					else if (a.name == "standard") {
						standard = {
							if (a.value == "BA") Preponderance {} // Preponderance and Best Argument are functionally equivalent
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
						// extract text
						wff = c.children[0].value;
						System.out.println(wff);

						// remove all linebreaks and tabs
						wff = wff.replaceAll("\n", "");
						wff = wff.replaceAll("\r", "");
						wff = wff.replaceAll("\t", "");

						System.out.println(wff);
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

								for (i in (p as XWElement).attributes) {
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
						for (i in (c as XWElement).attributes) {
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
var getGraphFromLkifV1Document = function(document: XWDocument): ArgumentGraph[] {
		var argumentGraph: ArgumentGraph = ArgumentGraph {};
		var agraph: XWElement = document.getElementsByTagName("argument-graph") [0];
		var statements: XWElement[];
		var arguments: XWElement[];
		var issues: XWElement[];
		var loadedIssues: Issue[];

		for (c: XWNode in agraph.children) {
			if (c.name == "statements") {
				statements = for (i in c.children where i.name == "s") { i as XWElement };
			}
			if (c.name == "arguments") {
				arguments = for (i in c.children where i.name == "argument") { i as XWElement };
			}
			if (c.name == "issues") {
				issues = for (i in c.children where i.name == "issue") { i as XWElement };
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
			wff = s.children[0].value; // get text node
			// remove all linebreaks and tabs
			wff = wff.replaceAll("\n", "");
			wff = wff.replaceAll("\r", "");
			wff = wff.replaceAll("\t", "");

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
					else Preponderance { statement: newStatement }
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

			// set variables from XWAttributes

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

							for (i in (p as XWElement).attributes) {
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
							} // XWAttributes

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
					for (i in (c as XWElement).attributes) {
						if (i.name == "statement") {
							var temp = for (s in argumentGraph.statements where s.id == i.value) { s };
							conclusion = temp[0];
						}
					} // XWAttributes
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

public function printGraphAsSVG(g: CarneadesGraph): String {
	g.toSVG()
}

public var saveAsImage = function(g: CarneadesGraph, fileName: String): Void {

	// System.out.println("{printGraphAsSVG(g)}");

	var writer: FileWriter = new FileWriter("{fileName}{if (not fileName.endsWith(".svg")) ".svg" else ""}");
	writer.write(printGraphAsSVG(g));
	writer.close();
}

