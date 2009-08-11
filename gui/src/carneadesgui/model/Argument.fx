/*
Carneades Argumentation Library and Tools.
Copyright (C) 2008 Thomas Gordon and Matthias Grabmair

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

package carneadesgui.model;

// import object for cycle checking
import java.lang.Object;

// import constants
import carneadesgui.GC.*;
import java.lang.System;

// Changed: List of supported proof standards for the GUI
public var proofStandards: String[] = [proofStandardSE, proofStandardDV, proofStandardBA, proofStandardPE, proofStandardCCE, proofStandardBRD];

abstract public class ProofStandard {
	public var statement : Statement;  // backwards reference

	public var negated: Boolean = false
		on replace {
			statement.update();
		};

	public var complement: Boolean = false
		on replace {
			statement.update();
		};

	abstract function test (ag: ArgumentGraph,
	               pro: Argument[],
	               con: Argument[]): Boolean;

	function satisfied (ag: ArgumentGraph,
	                    pro: Argument[],
	                    con: Argument[]) : Boolean {
		var r = if (not complement)
					test(ag, pro, con)
		        else
					test(ag, con, pro);
		if (negated) not r else r;
	}
}

public class Statement {

	public var id: String;  // xsd:ID
	public var wff: String; // The content model of the wff XML element
	public var graph: ArgumentGraph; // backwards reference

	// arguments in which this statement is a premise
	public var arguments: Argument[];

	// read-only: value, assumption
	// use the methods below to alter state

	// value = "true", "false" or "unknown
    public var value: String = "unknown";
	public var assumption: Boolean = true;

	public var standard: ProofStandard = BestArgument {}
		on replace {
			standard.statement = this;
			update();
		}

	/**
	* Function that returns the proof standard as a string according as set in the constants file.
	*/
	public function getStandard(): String {
	    var s: String = "";
	    if (standard instanceof Scintilla) s = proofStandardSE
	    else if (standard instanceof DialecticalValidity) s = proofStandardDV
	    else if (standard instanceof BestArgument) s = proofStandardBA
	    else if (standard instanceof Preponderance) s = proofStandardPE
	    else if (standard instanceof ClearAndConvincingEvidence) s = proofStandardCCE
	    else if (standard instanceof BeyondReasonableDoubt) s = proofStandardBRD;
	    return s;
	}

	// ok should be read only for other objects.  But we can't
	// make it  because views need to bind to this variable
	// and track changes to its value.
	public var ok: Boolean = false
		on replace {
			for (arg in arguments) arg.update(); // propogate changes
		}

	// read-only
	public var complementOk: Boolean = true
		on replace {
			for (arg in arguments) arg.update();
		}


    function update () : Void {
    	var pro = graph.arguments[arg | arg.conclusion == this and arg.pro];
	var con = graph.arguments[arg | arg.conclusion == this and not arg.pro];
    	ok = standard.satisfied(graph,pro,con);
    	complementOk = standard.satisfied(graph,con,pro);
    	for (arg in arguments) arg.update();
	status = getStatus();
    }

    public function stated () : Boolean {
	value == "unknown" and assumption == true;
    }

    public function questioned () : Boolean {
	value == "unknown" and assumption == false;
    }

    public function assumedTrue () : Boolean {
    	value == "true" and assumption == true;
    }

    public function assumedFalse () : Boolean {
    	value == "false" and assumption == true;
    }

    public function accepted () : Boolean {
    	value == "true" and assumption == false;
    }

    public function rejected () : Boolean {
    	value == "false" and assumption == false;
    }

    // this function is a workaround as there is a bug
    // in the compiler with regard to calling bound
    // functions from non-bound contexts
    public function getStatus(): String {
	if (stated()) "stated"
	else if (questioned()) "questioned"
	else if (assumedTrue()) "assumed true"
	else if (assumedFalse()) "assumed false"
	else if (rejected()) "rejected"
	else if (accepted()) "accepted"
	else "";
    }

    public var status: String = getStatus();

    public function state ():  Void {
	value = "unknown";
	assumption = true;
	update();
    }

    public function question () : Void {
    	value = "unknown";
    	assumption = false;
    	update();
    }

    public function assume (v: Boolean) : Void {
    	value = if (v) "true" else "false";
    	assumption = true;
    	update();
    }

    public function decide (v: Boolean) : Void {
    	value = if (v) "true" else "false";
    	assumption = false;
    	update();
    }

    public function accept () : Void {
		decide(true);
	}

	public function reject () : Void {
		decide(false);
    }
}

public class Premise {
	public var statement: Statement
		on replace {
			statement.update();
		};

	public var role: String = "";

	public var negative: Boolean = false
		on replace {
			statement.update();
		};

	public var exception: Boolean = false
		on replace {
			statement.update();
		};

	public function holds (): Boolean {
		var statementOk = statement.value == "true" or
    	                  statement.ok;
    	var complementOk = statement.value == "false" or
			               statement.complementOk;
		if (not exception)
			if (not negative)  // positive ordinary premise
				statementOk
			else               // negative ordinary premise
				complementOk
		else
			if (not negative)  // positive exception
				not statementOk
			else               // negative exception
				not complementOk
	}
}

public class Scheme {
	public var id: String;   // could be URI
	// to do: possibly other attributes
}

public class Argument {
	public var id: String;
	public var title: String;
	public var graph : ArgumentGraph;

	public var weight: Number = 0.5 // range: 0.0 to 1.0 coming as soon as JavaFX does it right
		on replace {
			conclusion.update();
		}

	public var scheme: Scheme;

    public var premises: Premise[]
    	on replace {
    		update();
    		conclusion.update();
    	};

	public var pro: Boolean = true
		on replace {
			conclusion.update();
		};

	public var conclusion: Statement
		on replace {
			conclusion.update();
		}

	// ok should be read only for other objects.  But we can't
	// make it  because views need to bind to this variable
	// and track changes to its value.
	public var ok: Boolean = false
		on replace {
			conclusion.update();  // propogate changes
		}

	public function allPremisesHold () : Boolean {
		0 == sizeof(premises[p | not p.holds()]);
	}

	 function update () : Void {
		ok = allPremisesHold();
	}

	public function defensible () : Boolean { allPremisesHold() }

    public function switchDirection (): Void {
		pro = not pro;
	}

	public function addPremise (p: Premise) : Number {
		insert p into premises;
		insert p.statement into graph.statements;
		insert this into p.statement.arguments;
		return AG_OK;
	}

	public function appendPremise (p: Premise) : Number {
		insert p into premises;
		insert this into p.statement.arguments;
		return AG_OK;
	}

	public function deletePremise (p: Premise) : Number {
		delete p from premises;
		delete this from p.statement.arguments;
		return AG_OK;
	}
}

public class ArgumentGraph {
	public var id: String = "NewGraph";
	public var title: String = "New Graph";
	public var mainIssue: Statement;

	// statements and arguments should be "read-only"
	public var statements: Statement[];
	public var arguments: Argument[];

	// Issues of XML format not needed, since the
	// statements here have all the attributes of issues.

	public function stronger (a1: Argument, a2: Argument) : Boolean {
		a1.weight > a2.weight;
	}

	public function asStrong (a1: Argument, a2: Argument) : Boolean {
		a1.weight >= a2.weight;
	}

	public function insertStatement (s: Statement): Number {
		var result: Number = AG_OK;

		// check for double ids
		if ((idTaken(s.id)) == false) {
			insert s into statements;
		} else {
			result = AG_DOUBLE_ID;
		}

		s.graph = this;
		return result;
	}

	public function deleteStatement (s: Statement) : Void {
		delete s from statements;

		// todo: remove possible premises leading to the statement.
		// todo: put the statement into the "trash" and allow a final deletion later.
	}

	public function insertArgument (arg: Argument) : Number {
		var result: Number = AG_OK;

		// check for and prohibit cycles.
		if (noCycles()) {
			insert arg into arguments;

			// add the statement of each premise to the
			// the statements of the graph, if not already present
			for (p in arg.premises) {
				insert arg into p.statement.arguments;
				if (0 == sizeof(statements[s | s == p.statement])) {
					insert p.statement into statements;
				}
			}
			// add the conclusion of the argument to statements of
			// the graph, if not already present
			if (0 == sizeof(statements[s | s == arg.conclusion])) {
				insert arg.conclusion into statements;
			}

			arg.graph = this;
			// arg.update();

		} else {
			result = AG_CYCLE;
		}

		return result;
    }

	public function deleteArgument (arg: Argument) : Void {
		delete arg from arguments;
		arg.conclusion.update();
		for (p in arg.premises) { delete arg from p.statement.arguments; }
	}


	function pro (s: Statement) : Argument[] {
		arguments[arg | arg.conclusion == s and arg.pro];
	}

	function con (s: Statement) : Argument[] {
		arguments[arg | arg.conclusion == s and not arg.pro];
	}


	// INFORMATION GATHERING FUNCTIONS

	// function to tell whether a statement has been brought forth in an argument
	public function broughtForth(s: Statement): Boolean {
		return (isConclusion(s) or isPremise(s));
	}

	// function returning true if the statement is used as a conclusion
	public function isConclusion(s: Statement): Boolean {
		var argued = false;
		for (a in arguments)
			if (a.conclusion == s)
				argued = true;
		return argued;
	}

	// function to tell whether a statement is a premise of an argument
	public function isPremise(s: Statement): Boolean {
		var premised: Boolean = false;
		for (a in arguments)
			for (p in a.premises)
				if (p.statement == s)
					premised = true;
		return premised;
	}

	// CYCLE CHECKING

	public function noCycles(): Boolean {
		var result: Boolean = true;
		// checking is done for all statement nodes
		var roots: Statement[] = [statements];
		for (r in roots)
			if (noCyclesRec(r, [r]) == false) result = false;
		return result
	}

	// Recursive  function for cycle checking
	 function noCyclesRec(root: Object, marked: Object[]): Boolean {
		var result = true;

		if (root instanceof Argument) {
			//System.out.println("premises to check: " + sizeof (root as Argument).premises);
			for (p in (root as Argument).premises) {
				//p("checking " + p.statement.id);
				// check whether the premise statement has been marked before
				if (isMemberOf(marked, p.statement)) {
					//p("returning false");
					result = false;
				}
				else {
					// if not, do the recursive call
					//p("Adding " + p.statement.id);
					if (noCyclesRec(p.statement, [marked, p.statement]) == false ) { result = false; } ;
				}
			}
		} else if (root instanceof Statement) {
			// check whether there is an argument with a matching conclusion
			for (a in arguments) {
				// does the conclusion match
				if (a.conclusion == root) {
					// has the argument already been marked
					//p("checking " + a.id);
					if (isMemberOf(marked, a)) {
						//p("returning false");
						result = false;
					} else {
					// do the recursive call
						//p("Adding " + a.id);
						if (noCyclesRec(a, [marked, a]) == false) { result = false; };
					}
				}
			}
		}

		// base case: no premises and no further arguments
		return result;
	}

	// DOUBLE ID CHECKING
	 function idTaken(id: String): Boolean {
		for (s in statements) {
			if (s.id == id) { return true; }
		}
		for (a in arguments) {
			if (a.id == id) { return true; }
		}
		return false;
	}

	public function getNewStatementId(): String {
		var admissible: Boolean = true;
		var id: String = "s";
		var number: Integer = 1;
		while ( idTaken("{id}{number.toString()}") ) { number ++; }
		return "{id}{number.toString()}";
	}

	public function getNewArgumentId(): String {
		var admissible: Boolean = true;
		var id: String = "a";
		var number: Integer = 1;
		while ( idTaken("{id}{number.toString()}") ) { number ++; }
		return "{id}{number.toString()}";
	}

	public function noDoubleIDs(newId: String): Boolean {
		var ids: String[];

		for (s in statements) {
			if (isMemberOf(ids, s.id)) { return false; } else { insert s.id into ids; }
		}
		for (a in arguments) {
			if (isMemberOf(ids, a.id)) { return false; } else { insert a.id into ids; }
		}
		if (isMemberOf(ids, newId)) { return false; }

		return true;
	}
}

// Proof Standards

// The proof standard assigned to each statement in an argument
// graph is an instance of some proof standard class, to allow
// the "negated" and "complement" attributes of the standard to
// be modified on a statement by statement basis, if desired.
// Alternatively, several statements could share an instance of some
// proof standard.  The user interfaces needs to make clear whether
// a proof standard is being modified for only a single statement or a
// set of statements.

public class Scintilla extends ProofStandard {
	override function test (ag: ArgumentGraph,
	               pro: Argument[],
	               con: Argument[]): Boolean {
	       sizeof(pro[p | p.allPremisesHold()]) > 0;
	}
}

public class DialecticalValidity extends ProofStandard {
	override function test (ag: ArgumentGraph,
	               pro: Argument[],
	               con: Argument[]): Boolean {
	       sizeof(pro[arg | arg.allPremisesHold()]) > 0 and
	       sizeof(con[arg | arg.allPremisesHold()]) == 0;
	}
}


// BestArgument and Preponderance are now equivalent standards, i.e.
// synonyms.

public class BestArgument extends ProofStandard {
	override function test (ag: ArgumentGraph,
	               pro: Argument[],
	               con: Argument[]): Boolean {
	       var okPro = pro [ arg | arg.allPremisesHold() ];
	       var okCon = con [ arg | arg.allPremisesHold() ];

	       var maxPro = 0.0;
	       for (arg in okPro) if (arg.weight > maxPro) maxPro = arg.weight;

	       var maxCon = 0.0;
	       for (arg in okCon) if (arg.weight > maxCon) maxCon = arg.weight;

	       return sizeof(okPro) > 0 and maxPro > maxCon;
	}
}


public class Preponderance extends ProofStandard {
	override function test (ag: ArgumentGraph,
	               pro: Argument[],
	               con: Argument[]): Boolean {
	       var okPro = pro [ arg | arg.allPremisesHold() ];
	       var okCon = con [ arg | arg.allPremisesHold() ];

	       var maxPro = 0.0;
	       for (arg in okPro) if (arg.weight > maxPro) maxPro = arg.weight;

	       var maxCon = 0.0;
	       for (arg in okCon) if (arg.weight > maxCon) maxCon = arg.weight;

	       return sizeof(okPro) > 0 and maxPro > maxCon;
	}
}


// Clear and Convincing Evidence: met if preponderance is met
// and the difference between the average weight of the pro arguments
// and the average weight of the con arguments is greater than
// some threshold.
public class ClearAndConvincingEvidence extends ProofStandard {
	override function test (ag: ArgumentGraph,
	               pro: Argument[],
	               con: Argument[]): Boolean {

		   // thresholds
		   var alpha = 0.5;
		   var beta = 0.3;

	       var okPro = pro [ arg | arg.allPremisesHold() ];
	       var okCon = con [ arg | arg.allPremisesHold() ];

	       var maxPro = 0.0;
	       for (arg in okPro) if (arg.weight > maxPro) maxPro = arg.weight;

	       var maxCon = 0.0;
	       for (arg in okCon) if (arg.weight > maxCon) maxCon = arg.weight;

	       return sizeof(okPro) > 0 and
	              maxPro > maxCon and
	              maxPro > alpha and
	              maxPro - maxCon > beta;
	}
}

// Beyond a Reasonable Doubt: met if the clear and convincing evidece
// test is met and the weight of the strongest con argument is below
// the threshold.
public class BeyondReasonableDoubt extends ProofStandard {
	override function test (ag: ArgumentGraph,
	               pro: Argument[],
	               con: Argument[]): Boolean {

		   // thresholds
		   var alpha = 0.5;
		   var beta = 0.3;
		   var gamma = 0.2;

	       var okPro = pro [ arg | arg.allPremisesHold() ];
	       var okCon = con [ arg | arg.allPremisesHold() ];

	       var maxPro = 0.0;
	       for (arg in okPro) if (arg.weight > maxPro) maxPro = arg.weight;

	       var maxCon = 0.0;
	       for (arg in okCon) if (arg.weight > maxCon) maxCon = arg.weight;

	       return sizeof(okPro) > 0 and
	              maxPro > maxCon and
	              maxPro > alpha and
	              maxPro - maxCon > beta and
	              maxCon < gamma;
	}
}

