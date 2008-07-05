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

abstract public class ProofStandard {
	/*private*/ public attribute negated: Boolean = false;
	/*private*/ public attribute complement: Boolean = false;
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
	public attribute id: String;  // key or label
	public attribute wff: String; // is a URI
   // Is URI referring inside the document, then do not resolve link
   // If it refers to something inside the document, otherwise give 
   // the wff in xml form. Label is always the ID!
	/*private*/ public attribute value: String = "unknown";  // "true", "false", "unknown"
	/*private*/ public attribute assumption: Boolean = false;
	/*private*/ public attribute standard: ProofStandard = BestArgument {};
	/*private*/ public attribute ok: Boolean = false;
	/*private*/ public attribute updated : Boolean = false;

	public function acceptable () : Boolean { ok };

	public function stated () : Boolean {
		value == "unknown" and assumption == true;
	}
    
	public function questioned () : Boolean {
		value == "unknown" and assumption == false;
    }
    
    public function accepted () : Boolean {
    	(value == "true") and (assumption == false);
    }
    
    public function rejected () : Boolean {
    	(value == "false") and (assumption == false);
    }
    
    public function truthValue () : String { value }
    public function truthValueAssumed () : Boolean { assumption }
    public function proofStandard () : ProofStandard { standard }
    
	public function status(): String {
		return {	if (stated()) "stated"
					else if (questioned()) "questioned"
					else if (rejected()) "rejected"
					else if (accepted()) "accepted"
					else ""};
	}
}

public class Premise {
	public attribute statement: Statement;
	public attribute role: String = "";
	public attribute negative: Boolean = false;
	public attribute exception: Boolean = false;
}

public class Scheme {
	attribute id: String;   // could be URI
	// to do: possibly other attributes
}

public class Argument {
	public attribute id: String;
	public attribute scheme: Scheme;
	/*private*/ public attribute premises: Premise[];  
	/*private*/ public attribute pro: Boolean = true;
	/*private*/ public attribute conclusion: Statement;
	/*private*/ public attribute ok: Boolean = false;  
	/*private*/ public attribute updated : Boolean = false;
	public function allPremisesHold () : Boolean { ok }  // i.e "defensible"

}

public class ArgumentGraph {
	public attribute id: String = "NewGraph";
	public attribute mainIssue: Statement; 
	/*private*/ public attribute statements: Statement[];
    /*private*/ public attribute arguments: Argument[];
 
 	public function state (s: Statement) :  Void {
		s.value = "unknown";
		s.assumption = true;
		update();
	}
	
	public function question (s: Statement) : Void {
    	s.value = "unknown";
    	s.assumption = false;
    	update();
    }
    
    public function accept (s: Statement) : Void {
		s.value = "true";
		s.assumption = false;
		update();
	}
	
	public function reject (s: Statement) : Void {
    	s.value = "false";
    	s.assumption = false;
    	update();
    }
    
	// set the id of statements and arguments
	public function setStatementId(s: Statement, id: String): Void {
		s.id = id;
	}
	
	public function setArgumentId(a: Argument, id: String): Void {
		a.id = id;
	}

	// set the wff of statements and arguments
	public function setStatementWff(s: Statement, wff: String): Void {
		s.wff = wff;
	}

	// set the role of premises
	public function setPremiseRole(p: Premise, role: String): Void {
		p.role = role;
	}

	// switch a premise's type
	public function switchPremiseType(p: Premise): Void {
		if (p.exception) { p.exception = false; } else { p.exception = true; }
		update();
	}

	// negate a premise
	public function negatePremise(p: Premise): Void {
		if (p.negative) { p.negative = false; } else { p.negative = true; }
		update();
	}
	
    // set the truth value of a statement to "true", "false" or "unknown"
    public function setTruthValue (s: Statement, v: String) : Void {
    	s.value = v;
    	update();
    }
    
    public function setTruthValueAssumed (s: Statement, v: Boolean) : Void {
    	s.assumption = v;
    	update();
    }
    
    // assume the truth value of a statement to be "true", "false" or "unknown"
    public function assume (s: Statement, v: String) : Void {
    	s.value = v;
    	s.assumption = true;
    	update();
    }
    
    public function setProofStandard (s: Statement, p: ProofStandard) : Void {
    	s.standard = p;
    	update();
    }
    
    
    public function negateProofStandard (s: Statement) : Void {
    	s.standard.negated = not s.standard.negated;
    	update();
    }
    
    public function complementProofStandard (s: Statement) : Void {
    	s.standard.complement = not s.standard.complement;
    	update();
    }
    

	
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

		update();
		return result;
	}

	public function deleteStatement (s: Statement) : Void {
		delete s from statements;

		// todo: remove possible premises leading to the statement.
		// todo: put the statement into the "trash" and allow a final deletion later.
		update();
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
			update();
		} else {
			result = GC.AG_CYCLE;
		}

		return result;
    }

	public function deleteArgument (arg: Argument) : Void {
		delete arg from arguments;
		// to do: what should happen to statements not used 
		// in other arguments?  Should they be deleted from
		// the graph?  How can this be efficiently determined?
		// Reference counting?
		update();
	}

    public function switchDirection (arg: Argument): Void {
		arg.pro = not arg.pro;
		update();
	}
	
	public function setConclusion (arg: Argument, s: Statement) : Void {
		arg.conclusion = s;
		update();
	}
	
	public function addPremise (p: Premise, a: Argument) : Number {
		insert p into a.premises;
		insert p.statement into statements;
		update();
		return GC.AG_OK;
	}

	public function appendPremise (p: Premise, a: Argument) : Number {
		insert p into a.premises;
		update();
		return GC.AG_OK;
	}

	public function deletePremise (p: Premise, a: Argument) : Number {
		delete p from a.premises;
		update();
		return GC.AG_OK;
	}
	
	// update the acceptability and defensibility of statements
	// and arguments respectively.
	private function update () : Void {
	    // first set all statements and arguments to not updated
		for (s in statements) {	s.updated = false; }
		for (arg in arguments) { arg.updated = false; }
		
		// update the statements and arguments
		for (s in statements) { acceptable(s); }
		for (arg in arguments) { allPremisesHold(arg); }
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
		acceptable(p.statement);  // check and update if necessary
		if (not p.exception) {
			// ordinary premise
			(p.statement.value == "true" and not p.negative) or
			(p.statement.value == "false" and p.negative)
		} else {
			// exception
			(p.statement.value != "true" and not p.negative) or
			(p.statement.value != "false" and p.negative)
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
