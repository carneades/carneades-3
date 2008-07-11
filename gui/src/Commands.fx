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


package Carneades.Control;

// General Imports
import java.lang.System;
import javafx.animation.*;
import java.io.File;

// Model imports
import Carneades.Argument.*;
import Carneades.Argument.Argument;
import Carneades.Argument.Argument.*;

// View imports
import Carneades.Graph.*;

// GENERAL CLASSES

public abstract class Command {

	public attribute argumentGraph: ArgumentGraph;
	public attribute commandControl: CommandControl;
	public attribute merges: Boolean = false;

	public function do(): Number {
		return GC.C_OK;
	}
	
	public function mergeable(c: Command): Boolean {false}

	public function merge(c: Command): Number { GC.C_OK }
}

public class UndoableCommand extends Command {

	public function undo(): Number {
		return GC.C_OK;
	}
}

public class CommandControl {

	public attribute control: GraphControl;
	private attribute commands: Command[] = [];
	private attribute size: Integer = bind sizeof commands;

	// The bookmark determines where we are in the undo-stack. It usually is 0, meaning
	// that we have undone 0 commands. Whenever an undo is called, it increases by one.
	// Hence, a redo decreases it by one.
	private attribute bookmark: Integer = 0;
	
	public function possibleToUndo(): Boolean { return (size - bookmark > 0); }

	public function possibleToRedo(): Boolean { return (bookmark > 0); }
	
	public function pop(): Void {
		// pops off the topmost elements until the bookmark.
		// The function is void and serves the purpose to allow for an auto-undo in case of e.g. cyclic graphs

		// throw away everything beyond the bookmark
		for (i in [1 .. bookmark]) {
			delete commands[size-1];
		}
		bookmark = 0;
	}

	public function do(c: Command): Number {
		// tell control the file has changed
		control.fileChanged = true;

		// throw away everything beyond the bookmark
		for (i in [1 .. bookmark]) {
			delete commands[size-1];
		}

		// do the command
		bookmark = 0;
		c.commandControl = this;

		// get the instance of the top command in case it works
		var topCommand: Command;
		if (sizeof commands > 0) { topCommand = commands [size-1]; }

		if ( c.merges and (c.getClass() == topCommand.getClass()) and c.mergeable(topCommand)) {
			return c.merge(topCommand);
		}
		else { 
			insert c into commands; 
			return c.do();
		}
	}

	public function undo(): Number {
		var result: Number = GC.C_NO_UNDO;

		if (commands[size - bookmark - 1] instanceof UndoableCommand) {
			result = (commands[size - bookmark - 1] as UndoableCommand).undo();
			bookmark += 1;
		}

		return result;
	}

	public function redo(): Number {
		var result: Number = GC.C_LATEST_COMMAND;

		if (bookmark > 0 and bookmark <= size) {
			result = commands[size - bookmark].do();
			bookmark -= 1;
		}
		return result;
	}

	public function reset(): Void {
		commands = [];
		bookmark = 0;
	}

}

// SPECIFIC CLASSES

public class AddArgumentCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute argument: Argument;

	postinit {
		argument = Argument {
			graph: argumentGraph;
			id: argumentGraph.getNewArgumentId();
			conclusion: statement
		}
	}

	public function do(): Number {
		argumentGraph.insertArgument(argument);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.deleteArgument(argument);
		return GC.C_OK;
	}
}

public class AddStatementCommand extends UndoableCommand {

	public attribute statement: Statement;

	public function do(): Number {
		// set a default value in case no statement has been passed
		if (statement == null){
			statement = Statement {
				id: argumentGraph.getNewStatementId();
				wff: "New Statement";
				graph: argumentGraph
			}
		}
		argumentGraph.insertStatement(statement);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.deleteStatement(statement);
		return GC.C_OK;
	}
}

public class AddPremiseCommand extends UndoableCommand {

	public attribute argument: Argument;
	public attribute premise: Premise;
	public attribute statement: Statement;

	postinit {
		statement = Statement {
			id: argumentGraph.getNewStatementId();
			wff: "Premise Statement";
			graph: argumentGraph
		}
		premise = Premise {
			statement: statement
		}
	}

	public function do(): Number {
		argument.addPremise(premise);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.deleteStatement(premise.statement);
		argument.deletePremise(premise);
		return GC.C_OK;
	}
}

public class AddArgumentAndPremiseCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute argument: Argument;
	public attribute premise: Premise;
	public attribute pstatement: Statement;

	postinit {
		pstatement = Statement {
			id: argumentGraph.getNewStatementId();
			wff: "Premise Statement";
			graph: argumentGraph
		}
		premise = Premise {
			statement: pstatement
		}
		argument = Argument {
			conclusion: statement
			id: argumentGraph.getNewArgumentId();
			graph: argumentGraph
		}
	}

	public function do(): Number {
		argument.addPremise(premise);
		argumentGraph.insertArgument(argument);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.deleteStatement(premise.statement);
		argument.deletePremise(premise);
		argumentGraph.deleteArgument(argument);
		return GC.C_OK;
	}
}

public class DeleteArgumentCommand extends UndoableCommand {
	
	public attribute argument: Argument;
	
	public function do(): Number {
		argumentGraph.deleteArgument(argument);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.insertArgument(argument);
		return GC.C_OK;
	}
}

public class DeleteStatementCommand extends UndoableCommand {
	public attribute statement: Statement;
	
	public function do(): Number {
		argumentGraph.deleteStatement(statement);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.insertStatement(statement);
		return GC.C_OK;
	}
}

public class DeleteConclusionCommand extends UndoableCommand {
	// Is called whenever a statement is deleted from the graph that has arguments attached to it.
	public attribute conclusion: Statement;
	public attribute motherArgument: Argument;
	public attribute premise: Premise;
	public attribute childArguments: Argument[];
	
	public function do(): Number {
		motherArgument.deletePremise(premise);
		for (c in childArguments) argumentGraph.deleteArgument(c);
		return GC.C_OK;
	}

	public function undo(): Number {
		for (c in childArguments) argumentGraph.insertArgument(c);
		motherArgument.addPremise(premise);
		return GC.C_OK;
	}
}

public class DeletePremiseCommand extends UndoableCommand {
	public attribute premise: Premise;
	public attribute argument: Argument;

	public function do(): Number {	
		argument.deletePremise(premise);
		return GC.C_OK;
	}

	public function undo(): Number {
		argument.addPremise(premise);
		return GC.C_OK;
	}
}

public class MovePremiseCommand extends UndoableCommand {
	public attribute premise: Premise;
	public attribute oldArgument: Argument;
	public attribute newArgument: Argument;

	public function do(): Number {
		// 1. remove premise from argument
		oldArgument.deletePremise(premise);
		// 2. add premise to new node
		newArgument.appendPremise(premise);
		return GC.C_OK;
	}

	public function undo(): Number {
		// 1. remove premise from argument
		newArgument.deletePremise(premise);
		// 2. add premise to old node
		oldArgument.appendPremise(premise);
		return GC.C_OK;
	}
}

public class MoveArgumentCommand extends UndoableCommand {
	public attribute argument: Argument;
	public attribute oldStatement: Statement;
	public attribute newStatement: Statement;

	public function do(): Number {
		argument.conclusion = newStatement;
		return GC.C_OK;
	}

	public function undo(): Number {
		argument.conclusion = oldStatement;
		return GC.C_OK;
	}
}

// ATTRIBUTE MODIFICATION COMMANDS

// for statements

/*
// The two commands below are potentially obsolete ...

public class NegateStatementAssumptionCommand extends UndoableCommand {

	public attribute statement: Statement;

	public function do(): Number {
		argumentGraph.setTruthValueAssumed
			(statement, 
			 not statement.truthValueAssumed());
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.setTruthValueAssumed
			(statement, 
			 not statement.truthValueAssumed());
		return GC.C_OK;
	}
}

public class ChangeStatementValueCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute oldValue: String;
	public attribute newValue: String;

	public function do(): Number {
		oldValue = statement.value;
		argumentGraph.setTruthValue(statement,newValue);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.setTruthValue(statement,oldValue3);
		return GC.C_OK;
	}
}

*/

public class ChangeStatementStatusCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute oldStatus: String;
	public attribute newStatus: String;

	public function do(): Number {
		
		oldStatus = statement.status();
		if (newStatus == "stated") { 
			statement.state();
		} else if (newStatus == "questioned") { 
			statement.question();
		} else if (newStatus == "rejected") { 
			statement.reject();
		} else if (newStatus == "accepted") { 
			statement.accept();
		} else if (newStatus == "assumed true") { 
			statement.assume(true); 
		} else if (newStatus == "assumed false") { 
			statement.assume(false); 
		}
		return GC.C_OK;
	}

	public function undo(): Number {
		if (oldStatus == "stated") { 
			statement.state();
		} else if (oldStatus == "questioned") { 
			statement.question();
		} else if (oldStatus == "rejected") { 
			statement.reject();
		} else if (oldStatus == "accepted") { 
			statement.accept();
		} else if (oldStatus == "assumed true") { 
			statement.assume(true); 
		} else if (oldStatus == "assumed false") { 
			statement.assume(false);
		}
		return GC.C_OK;
	}
}

public class ChangeStatementIdCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute id: String;
	public attribute oldId: String;

	public function do(): Number {
		oldId = statement.id;
		statement.id = id;
		//argumentGraph.setStatementId(statement, id);
		return GC.C_OK;
	}

	public function undo(): Number {
		statement.id = oldId;
		//argumentGraph.setStatementId(statement, oldId);
		return GC.C_OK;
	}
}

public class ChangeStatementWffCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute wff: String;
	public attribute oldWff: String;

	public function do(): Number {
		oldWff = statement.wff;
		statement.wff = wff;
		//argumentGraph.setStatementWff(statement, wff);
		return GC.C_OK;
	}

	public function undo(): Number {
		//argumentGraph.setStatementWff(statement, oldWff);
		statement.wff = oldWff;
		return GC.C_OK;
	}
}

public class ChangeArgumentSchemeCommand extends UndoableCommand {

	public attribute argument: Argument;
	public attribute scheme: String;
	public attribute oldScheme: String;

	public function do(): Number {
		oldScheme = argument.scheme.id;
		argument.scheme.id = scheme;
		return GC.C_OK;
	}

	public function undo(): Number {
		argument.scheme.id = oldScheme;
		return GC.C_OK;
	}
}

public class ChangeStatementStandardCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute standard: ProofStandard;
	public attribute oldStandard: ProofStandard;

	public function do(): Number {
		// backup old standard via deep copy
		if (statement.standard instanceof Scintilla) {
			oldStandard = Scintilla {
				negated: statement.standard.negated
				complement: statement.standard.complement
			};
		} else if (statement.standard instanceof DialecticalValidity) {
			oldStandard = DialecticalValidity {
				negated: statement.standard.negated
				complement: statement.standard.complement
			};
		} else if (statement.standard instanceof BestArgument) {
			oldStandard = BestArgument {
				negated: statement.standard.negated
				complement: statement.standard.complement
			};
		}
		// Set new Standard
		statement.standard = standard;
		standard.statement = statement;
		//argumentGraph.setProofStandard(statement, standard);
		return GC.C_OK;
	}

	public function undo(): Number {
		//argumentGraph.setProofStandard(statement,oldStandard);
		statement.standard = oldStandard;
		return GC.C_OK;
	}
}

// for arguments

public class ChangeArgumentIdCommand extends UndoableCommand {

	public attribute argument: Argument;
	public attribute id: String;
	public attribute oldId: String;

	public function do(): Number {
		oldId = argument.id;
		argument.id = id;
		//argumentGraph.setArgumentId(argument, id);
		return GC.C_OK;
	}

	public function undo(): Number {
		//argumentGraph.setArgumentId(argument, oldId);
		argument.id = oldId;
		return GC.C_OK;
	}
}

public class ChangeArgumentWeightCommand extends UndoableCommand {

	public attribute argument: Argument;
	public attribute weight: Number;
	public attribute oldWeight: Number;
	override attribute merges = true;

	public function do(): Number {
		oldWeight = argument.weight;
		argument.weight = weight;
		return GC.C_OK;
	}

	public function mergeable(c: Command): Boolean {
		return argument == (c as ChangeArgumentWeightCommand).argument;
	}

	public function merge(c: Command): Number {
		(c as ChangeArgumentWeightCommand).weight = weight;
		argument.weight = weight;
		return GC.C_OK;
	}

	public function undo(): Number {
		argument.weight = oldWeight;
		return GC.C_OK;
	}
}

public class ChangeArgumentDirectionCommand extends UndoableCommand {

	public attribute argument: Argument;

	public function do(): Number {
		argument.switchDirection();
		return GC.C_OK;
	}

	public function undo(): Number {
		argument.switchDirection();
		return GC.C_OK;
	}
}

// ... and for premises

public class ChangePremiseRoleCommand extends UndoableCommand {

	public attribute premise: Premise;
	public attribute role: String;
	public attribute oldRole: String;

	public function do(): Number {
		oldRole = premise.role;
		premise.role = role;
		//argumentGraph.setPremiseRole(premise, role);
		return GC.C_OK;
	}

	public function undo(): Number {
		//argumentGraph.setPremiseRole(premise, oldRole);
		premise.role = oldRole;
		return GC.C_OK;
	}
}

public class NegatePremiseCommand extends UndoableCommand {
	
	public attribute premise: Premise;

	public function do(): Number {
		//argumentGraph.negatePremise(premise);
		if (premise.negative) { premise.negative = false } else { premise.negative = true }
		return GC.C_OK;
	}

	public function undo(): Number {
		//argumentGraph.negatePremise(premise);
		if (premise.negative) { premise.negative = false } else { premise.negative = true }
		return GC.C_OK;
	}
}

public class ChangePremiseTypeCommand extends UndoableCommand {
	
	public attribute premise: Premise;

	public function do(): Number {
		if (premise.exception) { premise.exception = false } else { premise.exception = true }
		//argumentGraph.switchPremiseType(premise);
		return GC.C_OK;
	}

	public function undo(): Number {
		//argumentGraph.switchPremiseType(premise);
		return GC.C_OK;
	}
}
