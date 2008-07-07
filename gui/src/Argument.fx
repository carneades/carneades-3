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

package GraphSketch1.Argument;

// import object for cycle checking
import java.lang.Object;
// import constants
import GraphSketch1.Graph.GC;
import java.lang.System;

abstract public class ProofStandard {
	public attribute statement : Statement;  // backwards reference

	public attribute negated: Boolean = false 
		on replace {
			statement.graph.update();
		};

	public attribute complement: Boolean = false
		on replace {
			statement.graph.update();
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
	public attribute id: String;  // xsd:ID
	public attribute wff: String; // The content model of the wff XML element
	public attribute graph: ArgumentGraph; // backwards reference
	
	// value = "true", "false" or "unknown
    public attribute value: String = "unknown"  
    	on replace {
    		graph.update();
    	};
    	
	public attribute assumption: Boolean = false
		on replace {
			graph.update();
		}
		
	public attribute standard: ProofStandard = BestArgument {}
		on replace {
			standard.statement = this;
			graph.update();
		}
	
	// ok should be read only for other objects.  But we can't
	// make it private because views need to bind to this variable
	// and track changes to its value.
	public attribute ok: Boolean = false;
	
	public attribute updated : Boolean = false;

	public function acceptable () : Boolean { ok };

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
    
	public function status(): String {
		if (stated()) "stated"
		else if (questioned()) "questioned"
		else if (assumedTrue()) "assumed true"
		else if (assumedFalse()) "assumed false"
		else if (rejected()) "rejected"
		else if (accepted()) "accepted"
		else "";
	}
	
	public function state ():  Void {
		value = "unknown";
		assumption = true;
	}
	
	public function question () : Void {
    	value = "unknown";
    	assumption = false;
    }
    
    public function assume (v: Boolean) : Void {
    	value = if (v) "true" else "false";
    	assumption = true;
    }
    
    public function decide (v: Boolean) : Void {
    	value = if (v) "true" else "false";
    	assumption = false;
    }
    
    public function accept () : Void {
		decide(true);
	}
	
	public function reject () : Void {
		decide(false);
    }
}

public class Premise {
	public attribute statement: Statement 
		on replace {
			statement.graph.update();
		};
		
	public attribute role: String = "";
	
	public attribute negative: Boolean = false 
		on replace {
			statement.graph.update();
		};
		
	public attribute exception: Boolean = false
		on replace {
			statement.graph.update();
		};
}

public class Scheme {
	attribute id: String;   // could be URI
	// to do: possibly other attributes
}

public class Argument {
	public attribute id: String;
	public attribute graph : ArgumentGraph;  
	public attribute scheme: Scheme;
    public attribute premises: Premise[] 
    	on replace {
    		graph.update();
    	};
    	
	public attribute pro: Boolean = true 
		on replace {
			graph.update();
		};
	
	public attribute conclusion: Statement 
		on replace {	
			graph.update();
		}
	
	// ok should be read only for other objects.  But we can't
	// make it private because views need to bind to this variable
	// and track changes to its value.
	public attribute ok: Boolean = false;  
	
	public attribute updated : Boolean = false;
	
	public function allPremisesHold () : Boolean { ok } 
	public function defensible () : Boolean { allPremisesHold(); }
	
    public function switchDirection (): Void {
		pro = not pro;
	}
	
	public function addPremise (p: Premise) : Number {
		insert p into premises;
		insert p.statement into graph.statements;
		return GC.AG_OK;
	}

	public function appendPremise (p: Premise) : Number {
		insert p into premises;
		return GC.AG_OK;
	}

	public function deletePremise (p: Premise) : Number {
		delete p from premises;
		return GC.AG_OK;
	}
}

public class ArgumentGraph {
	 public attribute id: String = "NewGraph";
	 public attribute mainIssue: Statement; 
	 
	 public attribute statements: Statement[] 
	 	on replace {
	 		update();
	 	};

     public attribute arguments: Argument[]
     	on replace {
     		update();
     	};

    // Issues of XML format not needed, since the 
    // statements here have all the attributes of issues.

	// to do: attribute for some data structure representing
	// the relative strength of arguments, with the following API
	
	public function putStronger (a1: Argument, a2: Argument) : Void {
		// to do
    }

	public function putAsStrong (a1: Argument, a2: Argument) : Void {
		// to do
	}
	
	public function stronger (a1: Argument, a2: Argument) : Boolean {
		// to do
		false
	}
	
	public function atLeastAsStrong (a1: Argument, a2: Argument) : Boolean {
		// to do
		false
	}
	
	public function insertStatement (s: Statement): Number {
		var result: Number = GC.AG_OK;

		// check for double ids
		if ((idTaken(s.id)) == false) {
			insert s into statements;
		} else {
			result = GC.AG_DOUBLE_ID;
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
		var result: Number = GC.AG_OK;
		
		// check for and prohibit cycles.
		if (noCycles()) {	
			insert arg into arguments;
			// add the statement of each premise to the
			// the statements of the graph, if not already present
			for (p in arg.premises) {
				if (0 == sizeof(statements[s | s == p.statement])) {
					insert p.statement into statements;
				}
			}
			// add the conclusion of the argument to statements of
			// the graph, if not already present
			if (0 == sizeof(statements[s | s == arg.conclusion])) {
				insert arg.conclusion into statements;
			}
		} else {
			result = GC.AG_CYCLE;
		}

		arg.graph = this;
		return result;
    }

	public function deleteArgument (arg: Argument) : Void {
		delete arg from arguments;
	}
	
	// update the acceptability and defensibility of statements
	// and arguments respectively.
	private function update () : Void {
		var n = 2; // number of passes
		// to do: not clear why multiple passes are needed
		// one should be enough, but isn't
		for (i in [1..n]) {
	    	// first set all statements and arguments to not updated
			for (s in statements) {	s.updated = false; }
			for (arg in arguments) { arg.updated = false; }
		
			// update the statements and arguments
			for (s in statements) { acceptable(s); }
			for (arg in arguments) { allPremisesHold(arg); }
		}
	}
	
	function pro (s: Statement) : Argument[] {
		arguments[arg | arg.conclusion == s and arg.pro];
	}
	
	function con (s: Statement) : Argument[] {
		arguments[arg | arg.conclusion == s and not arg.pro];
	}
	
	public function acceptable (s: Statement) : Boolean {
		if (s.updated) {
			s.ok;
		} else {
			if (s.standard.satisfied(this,pro(s),con(s))) {
				s.ok = true;
				s.updated = true;
				true;
			} else {
				s.ok = false;
				s.updated = true;
				false;
			}
		}
	}
	
	// Check whether the complement of a statement would be acceptable,
	// which is the case if its proof standard is satisfied when 
	// the roles of the pro and con arguments are reversed.  Unlike 
	// acceptable(), this function is side-effect free.  It doesn't
	// update the statement.
	public function complementAcceptable (s: Statement) : Boolean {
		s.standard.satisfied(this,con(s),pro(s)) 
	}

	public function allPremisesHold (a: Argument): Boolean {
		if (a.updated) {
			a.ok;
		} else {
			if (sizeof(a.premises[p | not holds(p)]) == 0) {
				a.updated = true;
				a.ok = true;
				true;
			} else {
				a.updated = true;
				a.ok = false;
				false;
			}
		}
	}

	public function holds (p: Premise): Boolean {
		if (not p.exception) {    // ordinary premise
			if (not p.negative) { // positive premise
				p.statement.value == "true" or
				(p.statement.value == "unknown" and
				acceptable(p.statement))
			} else {             // negative premise
				p.statement.value == "false" or
				(p.statement.value == "unknown" and 
				complementAcceptable(p.statement))
			}
		} else {
			// exception
			if (not p.negative) { // positive exception
				p.statement.value == "false" or
				(p.statement.value == "unknown" and
				 not acceptable(p.statement))
			} else {              // negative exception
				p.statement.value == "true" or
				(p.statement.value == "unknown" and
				 not complementAcceptable(p.statement))
			}
		}
	}

	// INFORMATION GATHERING FUNCTIONS

	// function to tell whether a statement has been brought forth in an argument
	public function broughtForth(s: Statement): Boolean {
		// return true if the statement has been brought forth in an 
		// argument or connected in a premise
		var argued = false;
		for (a in arguments) {
			if (a.conclusion == s) {
				argued = true;
			}
		}
		return argued;
	}

	// function to tell whether a statement is a premise of an argument
	public function isPremise(s: Statement): Boolean {
		var premised: Boolean = false;
		for (a in arguments) {
			for (p in a.premises) {
				if (p.statement == s) {
					premised = true;
				}
			}
		}
		return premised;
	}

	// CYCLE CHECKING

	public function noCycles(): Boolean {
		var result: Boolean = true;
		// checking is done for all statement nodes
		var roots: Statement[] = [statements];
		for (r in roots) {
			if (noCyclesRec(r, [r]) == false) { result = false; }
		}
		return result
	}
	
	// Recursive private function for cycle checking
	private function noCyclesRec(root: Object, marked: Object[]): Boolean {
		var result = true;
		
		if (root instanceof Argument) {
			//System.out.println("premises to check: " + sizeof (root as Argument).premises);
			for (p in (root as Argument).premises) {
				//GC.p("checking " + p.statement.id);
				// check whether the premise statement has been marked before
				if (contains(marked, p.statement)) { 
					//GC.p("returning false");
					result = false;
				} 
				else {
					// if not, do the recursive call
					//GC.p("Adding " + p.statement.id);
					if (noCyclesRec(p.statement, [marked, p.statement]) == false ) { result = false; } ;
				}
			}
		} else if (root instanceof Statement) {
			// check whether there is an argument with a matching conclusion
			for (a in arguments) {
				// does the conclusion match
				if (a.conclusion == root) {
					// has the argument already been marked
					//GC.p("checking " + a.id);
					if (contains(marked, a)) {
						//GC.p("returning false");
						result = false;
					} else {
					// do the recursive call
						//GC.p("Adding " + a.id);
						if (noCyclesRec(a, [marked, a]) == false) { result = false; };
					}
				}
			}
		}

		// base case: no premises and no further arguments
		return result;
	}

	// DOUBLE ID CHECKING
	private function idTaken(id: String): Boolean {
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

		while ( idTaken(id + number.toString()) ) { number ++; }

		return id + number.toString();
	}

	public function getNewArgumentId(): String {
		var admissible: Boolean = true;
		var id: String = "a";
		var number: Integer = 1;

		while ( idTaken(id + number.toString()) ) { number ++; }

		return id + number.toString();
	}

	public function noDoubleIDs(newId: String): Boolean {
		var ids: String[];

		for (s in statements) {
			if (contains(ids, s.id)) { return false; } else { insert s.id into ids; }
		}
		for (a in arguments) {
			if (contains(ids, a.id)) { return false; } else { insert a.id into ids; }
		}
		if (contains(ids, newId)) { return false; } 

		return true;
	}

	// HELPER FUNCTIONS
	private function contains(list: Object[], element: Object): Boolean {
		for (i in list) {
			if (i == element) { return true; }
		}
		return false;
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
	function test (ag: ArgumentGraph, 
	               pro: Argument[], 
	               con: Argument[]): Boolean {
	       sizeof(pro[p | p.allPremisesHold()]) > 0;               
	}
}

public class DialecticalValidity extends ProofStandard {
	function test (ag: ArgumentGraph, 
	               pro: Argument[], 
	               con: Argument[]): Boolean {
	       sizeof(pro[arg | arg.allPremisesHold()]) > 0 and
	       sizeof(con[arg | arg.allPremisesHold()]) == 0;
	}
}

public class BestArgument extends ProofStandard {
	function test (ag: ArgumentGraph, 
	               pro: Argument[], 
	               con: Argument[]): Boolean {
	       var okPro = pro [ arg | arg.allPremisesHold() ];
	       var okCon = con [ arg | arg.allPremisesHold() ];
	       // there is at least one ok pro argument, and
	       sizeof(okPro) > 0 and
	       // there is no ok con argument which is at least
	       // as strong as every ok pro argument
	       0 == sizeof(okCon [ conArg | 
	       			0 < sizeof(okPro [ proArg | 
	       				ag.atLeastAsStrong(conArg,proArg) ]) ]);
	}
}
