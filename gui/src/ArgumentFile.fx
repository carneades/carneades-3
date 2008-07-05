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

package GraphSketch1.Argument;

// General imports
import javafx.xml.*;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.FileInputStream;
import java.lang.System;
import java.net.URI;

// other Argument imports
import GraphSketch1.Argument.Argument;
import GraphSketch1.Argument.Argument.*;

// helper issue class for file loading
class Issue {
	attribute statementId: String;
	attribute value: String;
	attribute assumption: Boolean;
	attribute standard: String;
}

public class ArgumentFile {

	static attribute builder: DocumentBuilder = DocumentBuilder {
		namespaceAware: true
		validating: true
		ignoringComments: false
	}

	public static function saveGraphToFile(argumentGraph: ArgumentGraph, file: File): Void {

		var document = builder.createDocument();

		var lkifElement = document.createElement("lkif");

		document.documentElement = lkifElement;

		document.documentElement.children = [
			Element {
				name: "argument-graph"
				document: document
				attributes: [
					Attribute {
						name: "id"
						value: argumentGraph.id
					}
				] // attributes
				children: [
					Element {
						name: "issues"
						document: document
						children: [
							for (s in argumentGraph.statements where ((s.value != "unknown") or 
																		(s.assumption) or 
																		not (s.standard instanceof DialecticalValidity))) {
								Element {
									name: "issue"
									document: document
									attributes: [
										Attribute {
											name: "statement"
											value: s.id
										}
										, Attribute {
											name: "value"
											value: s.value
										}
										, Attribute {
											name: "assumption"
											value: { if (s.assumption) "true" else "false"}
										}
										, Attribute {
											name: "standard"
											value: { if (s.standard instanceof Scintilla) "SE" 
													else if (s.standard instanceof BestArgument) "BA"
													else /*if (s.standard instanceof DialecticalValidity)*/ "DV"}
										}
									] // attributes
								} // element
							} // for
						] // children
					} // issues
					, Element {
						name: "statements"
						document: document
						children: [
							for (s in argumentGraph.statements) {
								Element {
									name: "s"
									document: document
									attributes: [
										Attribute {
											name: "id"
											value: s.id
										}
									]
									children: [
										Text { // actual text of the tag
											value: s.wff
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
		];

		// create a new file of the specified name
		file.createNewFile();

		// toDo: test canwrite()

		var writer: FileWriter = new FileWriter(file);

		var output: String = document.toString();

		writer.write(output);

		writer.close();

		System.out.println(document);
		

		/*
		output += 	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
					<?oxygen RNGSchema=\"LKIF2.rnc\" type=\"compact\"?>
					<?xml-stylesheet type=\"text/css\" href=\"LKIF2.css\"?>
					<lkif xmlns:family=\"http:://fokus.fraunhofer.de/elan/estrella/family.owl\"
    				xmlns:foods=\"http:://fokus.fraunhofer.de/elan/estrella/food.owl\"
    				xmlns:smith=\"http:://fokus.fraunhofer.de/elan/estrella/smith.owl\">";
		*/

	}

	public static function getGraphFromFile(file: File): ArgumentGraph {

		/*
		Currently, the loades does NOT do grammatical validation as well as NOT the following checks:
		- multiple statements/arguments having the same id
		- cycles

		*/

		// toDo: test canRead()

		// I. Validate the File against the grammar
		var noError: Boolean = true;
		
		//var factory: SchemaFactory = new SchemaFactory();
		//var schema: Schema = factory.createSchema(new InputSource(new FileInputStream("data/LKIF2.rnc")));



		// II. Load the Argument Graph
		var argumentGraph: ArgumentGraph = ArgumentGraph {};
		var document = builder.parseFile(file);

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
				if (a.name == "assumption") { if (a.value == "true") true else false; }
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
				id: id
				wff: wff
			}

			// check whether there is an issue to override the defaults
			for (i in loadedIssues where i.statementId == newStatement.id) {
				newStatement.value = i.value;
				newStatement.assumption = i.assumption;
				newStatement.standard = { 
											if (i.standard == "SE") Scintilla {} 
											else if (i.standard == "DV") DialecticalValidity {}
											else /*(i.standard == "BA")*/ BestArgument {} };
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

			// set variables from attributes

			for (i in a.attributes) {
				if (i.name == "id") { id = i.value; }
				if (i.name == "direction") {
					if (i.value == "pro") { pro = true; }
					else /*(i.value == "con")*/ { pro = false; }
				}
				if (i.name == "scheme") {
					scheme = Scheme {
						id: i.value
					}
				}
			}
			
			// fetch premises
			for (c in a.children) {
				if (c.name == "premises") {
					for (p in c.children) {
						if (p.name == "premise") {
							var statement: Statement;
							var negative: Boolean = false;
							var role: String = "";
							var type: String = "ordinary";

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
								if (i.name == "type") { type = i.name; }
							} // attributes
							
							var newPremise: Premise;

							newPremise = Premise {
								statement: statement
								negative: negative
								role: role
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
				scheme: scheme
				premises: premises  
				pro: pro
				conclusion: conclusion
			}

			insert newArgument into argumentGraph.arguments;
		} // fetch arguments
		
		return argumentGraph;
	}
}
