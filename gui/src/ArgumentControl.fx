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

// General Imports
import java.lang.System;
import java.lang.Object;

// Constants imports
import GraphSketch1.Graph.GC;

// Model imports
import GraphSketch1.Argument.Argument;
import GraphSketch1.Argument.Argument.*;


// This file contains all model editing functionality that is not present in Argument.fx. As soon as Argument.fx contains all necessary functions, this class/file can be abolished.

public class ArgumentControl {

	// MODIFICATION FUNCTIONS

	public static function newGraph(argumentGraph: ArgumentGraph): Number {
		var result = GC.AG_OK;

		argumentGraph.statements = [];
		argumentGraph.arguments = [];
	
		var conclusion = Statement {
			id: argumentGraph.getNewStatementId()
			wff: "New Conclusion"
		};

		insert conclusion into argumentGraph.statements;

		return result;

	}

	public static function addPremise(argumentGraph: ArgumentGraph, argument: Argument, premise: Premise, statement: Statement): Number {
		var result = GC.AG_OK;

		insert premise into argument.premises;

		return result;
	}
	

	// DELETION FUNCTIONS

	public static function deleteStatement(argumentGraph: ArgumentGraph, s: Statement): Number {
		var result = GC.AG_OK;
		
		for (a in argumentGraph.arguments) {
			if (a.conclusion == s) {
				a.conclusion = null;
			}
			for (p in a.premises) {
				if (p.statement == s) {
					delete p from a.premises;
				}
			}
		}
		delete s from argumentGraph.statements;
		
		return result;
	}

	public static function unLinkArgument(argumentGraph: ArgumentGraph, a: Argument): Number {
		var result = GC.AG_OK;
		
		a.conclusion = null;

		return result;
	}

	public static function deletePremise(a: Argument, p: Premise): Number {
		var result = GC.AG_OK;
		
		delete p from a.premises;

		return result;
	}

	public static function deleteEntirePremise(argumentGraph: ArgumentGraph, a: Argument, p: Premise): Number {
		var result = GC.AG_OK;
		
		delete p from a.premises;
		delete p.statement from argumentGraph.statements;

		return result;
	}

	// ATTRIBUTE MODIFICATION FUNCTIONS

	// for statements ...

	public static function changeStatementId(s: Statement, id: String): Number {
		var result = GC.AG_OK;
	
		s.id = id;

		return result;
	}

	public static function changeStatementWff(s: Statement, c: String): Number {
		var result = GC.AG_OK;
	
		s.wff = c;

		return result;
	}

	public static function changeStatementStandard(s: Statement, st: ProofStandard): Number {
		var result = GC.AG_OK;
	
		s.standard = st;

		return result;
	}

	// for arguments ...

	public static function changeArgumentId(s: Argument, id: String): Number {
		var result = GC.AG_OK;
	
		s.id = id;

		return result;
	}

	public static function changeArgumentDirection(argument: Argument): Number {
		var result = GC.AG_OK;

		if (argument.pro) {
			argument.pro = false;
		} else {
			argument.pro = true;
		}	

		return result;
	}

	// and for premises

	public static function negatePremise(argumentGraph: ArgumentGraph, premise: Premise): Number {
		var result = GC.AG_OK;

		if (premise.negative) {
			premise.negative = false;
		} else {
			premise.negative = true;
		}

		return result;
	}

	public static function changePremiseRole(p: Premise, r: String): Number {
		var result = GC.AG_OK;

		p.role = r;

		return result;
	}

}
