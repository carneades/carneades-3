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

package Carneades.Argument;

// import object for cycle checking
import java.lang.Object;
// import constants
import Carneades.Graph.GC;
import java.lang.System;

abstract public class ProofStandard {
	public attribute statement : Statement;  // backwards reference

	public attribute negated: Boolean = false 
		on replace {
			statement.update();
		};

	public attribute complement: Boolean = false
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
	public attribute id: String;  // xsd:ID
	public attribute wff: String; // The content model of the wff XML element
	public attribute graph: ArgumentGraph; // backwards reference
	
	// arguments in which this statement is a premise
	public attribute arguments: Argument[]; 
	
	// read-only: value, assumption
	// use the methods below to alter state

	// value = "true", "false" or "unknown
    public attribute value: String = "unknown";      	
	public attribute assumption: Boolean = true;
		
	public attribute standard: ProofStandard = BestArgument {}
		on replace {
			standard.statement = this;
			update();
		}
	
	// ok should be read only for other objects.  But we can't
	// make it private because views need to bind to this variable
	// and track changes to its value.
	public attribute ok: Boolean = false 
		on replace {
			for (arg in arguments) arg.update(); // propogate changes
		}
		
	// read-only
	public attribute complementOk: Boolean = true 
		on replace {
			for (arg in arguments) arg.update();
		}
	

    private function update () : Void {
    	var pro = graph.arguments[arg | arg.conclusion == this and arg.pro];
		var con = graph.arguments[arg | arg.conclusion == this and not arg.pro];
    	ok = standard.satisfied(graph,pro,con);
    	complementOk = standard.satisfied(graph,con,pro);
    	for (arg in arguments) arg.update();
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
	public bound function getBoundStatus(): String {
		if (stated()) "stated"
		else if (questioned()) "questioned"
		else if (assumedTrue()) "assumed true"
		else if (assumedFalse()) "assumed false"
		else if (rejected()) "rejected"
		else if (accepted()) "accepted"
		else "";
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
	public attribute statement: Statement 
		on replace {
			statement.update();
		};
		
	public attribute role: String = "";
	
	public attribute negative: Boolean = false 
		on replace {
			statement.update();
		};
		
	public attribute exception: Boolean = false
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
	public attribute id: String;   // could be URI
	// to do: possibly other attributes
}

public class Argument {
	public attribute id: String;
	public attribute title: String;
	public attribute graph : ArgumentGraph;  

	public attribute weight: Number = 0.5 // range: 0.0 to 1.0 coming as soon as JavaFX does it right
		on replace {
			conclusion.update();
		}
		
	public attribute scheme: Scheme;
	
    public attribute premises: Premise[] 
    	on replace {
    		update();
    		conclusion.update();
    	};
    	
	public attribute pro: Boolean = true 
		on replace {
			conclusion.update();
		};
	
	public attribute conclusion: Statement 
		on replace {	
			conclusion.update();
		}
	
	// ok should be read only for other objects.  But we can't
	// make it private because views need to bind to this variable
	// and track changes to its value.
	public attribute ok: Boolean = false 
		on replace {
			conclusion.update();  // propogate changes
		}
	
	public function allPremisesHold () : Boolean { 
		0 == sizeof(premises[p | not p.holds()]);
	} 
	
	private function update () : Void {
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
		return GC.AG_OK;
	}

	public function appendPremise (p: Premise) : Number {
		insert p into premises;
		insert this into p.statement.arguments;
		return GC.AG_OK;
	}

	public function deletePremise (p: Premise) : Number {
		delete p from premises;
		delete this from p.statement.arguments;
		return GC.AG_OK;
	}
}

public class ArgumentGraph {
	public attribute id: String = "NewGraph";
	public attribute title: String = "New Graph";
	public attribute mainIssue: Statement; 

	// statements and arguments should be "read-only"
	public attribute statements: Statement[];
    public attribute arguments: Argument[];

    // Issues of XML format not needed, since the 
    // statements here have all the attributes of issues.
	
	public function stronger (a1: Argument, a2: Argument) : Boolean {
		a1.weight > a2.weight;
	}
	
	public function asStrong (a1: Argument, a2: Argument) : Boolean {
		a1.weight >= a2.weight;
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
			result = GC.AG_CYCLE;
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
	
	// Recursive private function for cycle checking
	private function noCyclesRec(root: Object, marked: Object[]): Boolean {
		var result = true;
		
		if (root instanceof Argument) {
			//System.out.println("premises to check: " + sizeof (root as Argument).premises);
			for (p in (root as Argument).premises) {
				//GC.p("checking " + p.statement.id);
				// check whether the premise statement has been marked before
				if (GC.contains(marked, p.statement)) { 
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
					if (GC.contains(marked, a)) {
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
			if (GC.contains(ids, s.id)) { return false; } else { insert s.id into ids; }
		}
		for (a in arguments) {
			if (GC.contains(ids, a.id)) { return false; } else { insert a.id into ids; }
		}
		if (GC.contains(ids, newId)) { return false; } 

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
	       // as strong as every ok pro argument, that is
	       // there is an ok pro argument which is strictly 
	       // stronger than every ok con argument.
	       0 < sizeof(okPro [ proArg |	
	       		0 == sizeof (okCon [ conArg | 	
	       				ag.asStrong(conArg,proArg) ]) ]);

	}
}

// According to Clermont and Sherman, in the US "Everyone agrees .. the
// establishment of the truth of facts in adjudication is typically a 
// a matter of probabilities, falling short of absolute certainty".  The 
// different proof standards are interpreted as setting different 
// probability thresholds.  preponderance: > 0.5,  clear and convincing
// evidence: > 0.7 (my estimate, no number stated in at the article), 
// beyond a reasonable doubt:  0.95 (again, my estimate.  Described
// in the article only as "virtual certainty.)

// The alternative model of these standards below is not based on 
// probability theory.  They will thus need to be justified carefully
// in an article comparing argumentation theory with probability theory
// and explaining the inapplicability of probability theory to the
// problem of aggregating evidence in legal cases.

public class Preponderance extends ProofStandard {
	function test (ag: ArgumentGraph, 
	               pro: Argument[], 
	               con: Argument[]): Boolean {
	       var okPro = pro [ arg | arg.allPremisesHold() ];
	       var okCon = con [ arg | arg.allPremisesHold() ];
	       
	       var sumPro = 0.0; 
	       for (arg in okPro) sumPro = sumPro + arg.weight;
	       var avgPro = sumPro / sizeof(okPro);
	       
	       var sumCon = 0.0;
	       for (arg in okCon) sumCon = sumCon + arg.weight;
	       var avgCon = sumCon / sizeof(okCon);
	       
	       return avgPro > avgCon;
	}
}


// Clear and Convincing Evidence: met if preponderance is met 
// and the difference between the average weight of the pro arguments
// and the average weight of the con arguments is greater than
// some threshold.
public class ClearAndConvincingEvidence extends ProofStandard {
	function test (ag: ArgumentGraph, 
	               pro: Argument[], 
	               con: Argument[]): Boolean {

		   var threshold = 0.3;
	       var okPro = pro [ arg | arg.allPremisesHold() ];
	       var okCon = con [ arg | arg.allPremisesHold() ];
	       
	       var sumPro = 0.0; 
	       for (arg in okPro) sumPro = sumPro + arg.weight;
	       var avgPro = sumPro / sizeof(okPro);
	       
	       var sumCon = 0.0;
	       for (arg in okCon) sumCon = sumCon + arg.weight;
	       var avgCon = sumCon / sizeof(okCon);
	       
	       return (avgPro > avgCon) and 
	              (avgPro - avgCon > threshold);
	}
}

// Beyond a Reasonable Doubt: met if the clear and convincing evidece
// test is met and the weight of the strongest con argument is below
// the threshold.
public class BeyondReasonableDoubt extends ProofStandard {
	function test (ag: ArgumentGraph, 
	               pro: Argument[], 
	               con: Argument[]): Boolean {
	      var threshold = 0.3;

	      var okPro = pro [ arg | arg.allPremisesHold() ];
	      var okCon = con [ arg | arg.allPremisesHold() ];

   	      var strongestCon = 0.0;
	      for (arg in okCon) 
	      	if (arg.weight > strongestCon) 
	      		strongestCon = arg.weight;
	       
	      var sumPro = 0.0; 
	      for (arg in okPro) sumPro = sumPro + arg.weight;
	      var avgPro = sumPro / sizeof(okPro);
	       
	      var sumCon = 0.0;
	      for (arg in okCon) sumCon = sumCon + arg.weight;
	      var avgCon = sumCon / sizeof(okCon);
	     
	      return (avgPro > avgCon) and 
	             (avgPro - avgCon > threshold) and
	             strongestCon < threshold;
	}
}



