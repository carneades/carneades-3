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


package GraphSketch1.Control;

// General Imports
import java.lang.System;
import javafx.animation.*;
import java.io.File;

// Model imports
import GraphSketch1.Argument.*;
import GraphSketch1.Argument.Argument;
import GraphSketch1.Argument.Argument.*;

// View imports
import GraphSketch1.Graph.*;

// GENERAL CLASSES

public abstract class Command {

	public attribute argumentGraph: ArgumentGraph;

	public function do(): Number {
		return GC.C_OK;
	}

}

public class UndoableCommand extends Command {

	public function undo(): Number {
		return GC.C_OK;
	}
}

public class CommandControl {

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
		// throw away everything beyond the bookmark
		for (i in [1 .. bookmark]) {
			delete commands[size-1];
		}

		// do the command
		bookmark = 0;
		insert c into commands;
		return c.do();
	}

	public function undo(): Number {
		var result: Number = GC.C_NO_UNDO;

		//System.out.println("Attempting to undo " +commands[size - bookmark - 1] );
		if (commands[size - bookmark - 1] instanceof UndoableCommand) {
			//GC.p("undoing ...");
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
			id: argumentGraph.getNewArgumentId();
			conclusion: statement
		}
	}

	public function do(): Number {
		argumentGraph.insertArgument(argument);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.deleteArgument(argument);
		// argumentGraph.update();
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
				wff: "New Statement"
			}
		}
		argumentGraph.insertStatement(statement);
		argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.deleteStatement(statement);
		argumentGraph.update();
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
			wff: "Premise Statement"
		}
		premise = Premise {
			statement: statement
		}
	}

	public function do(): Number {
		argumentGraph.addPremise(premise, argument);
		argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		ArgumentControl.deleteEntirePremise(argumentGraph, argument, premise);
		argumentGraph.update();
		return GC.C_OK;
	}
}

public class DeleteArgumentCommand extends UndoableCommand {
	
	public attribute argument: Argument;
	
	public function do(): Number {
		argumentGraph.deleteArgument(argument);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.insertArgument(argument);
		// argumentGraph.update();
		return GC.C_OK;
	}
}

public class DeleteStatementCommand extends UndoableCommand {
	public attribute statement: Statement;
	
	public function do(): Number {
		argumentGraph.deleteStatement(statement);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.insertStatement(statement);
		// argumentGraph.update();
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
		ArgumentControl.deletePremise(motherArgument, premise);
		for (c in childArguments) argumentGraph.deleteArgument(c);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		for (c in childArguments) argumentGraph.insertArgument(c);
		ArgumentControl.addPremise(argumentGraph, motherArgument, premise, premise.statement);
		// argumentGraph.update();
		return GC.C_OK;
	}
}

public class DeletePremiseCommand extends UndoableCommand {
	public attribute premise: Premise;
	public attribute argument: Argument;

	public function do(): Number {	
		ArgumentControl.deletePremise(argument, premise);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		ArgumentControl.addPremise(argumentGraph, argument, premise, premise.statement);
		// argumentGraph.update();
		return GC.C_OK;
	}
}

public class MovePremiseCommand extends UndoableCommand {
	public attribute premise: Premise;
	public attribute oldArgument: Argument;
	public attribute newArgument: Argument;

	public function do(): Number {
		// 1. remove premise from argument
		argumentGraph.deletePremise(premise, oldArgument);
		// 2. add premise to new node
		argumentGraph.appendPremise(premise, newArgument);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		// 1. remove premise from argument
		argumentGraph.deletePremise(premise, newArgument);
		// 2. add premise to old node
		argumentGraph.appendPremise(premise, oldArgument);
		// argumentGraph.update();
		return GC.C_OK;
	}
}

public class MoveArgumentCommand extends UndoableCommand {
	public attribute argument: Argument;
	public attribute oldStatement: Statement;
	public attribute newStatement: Statement;

	public function do(): Number {
		// todo: put this into the model
		argument.conclusion = newStatement;
		//argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		argument.conclusion = oldStatement;
		//argumentGraph.update();
		return GC.C_OK;
	}
}




// ATTRIBUTE MODIFICATION COMMANDS

// for statements

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
		argumentGraph.setTruthValue(statement,oldValue);
		return GC.C_OK;
	}
}

public class ChangeStatementStatusCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute oldStatus: String;
	public attribute newStatus: String;

	public function do(): Number {
		
		oldStatus = statement.status();
		if (newStatus == "stated") { 
			argumentGraph.state(statement); 
		} else if (newStatus == "questioned") { 
			argumentGraph.question(statement); 
		} else if (newStatus == "rejected") { 
			argumentGraph.reject(statement); 
		} else if (newStatus == "accepted") { 
			argumentGraph.accept(statement); 
		}
		return GC.C_OK;
	}

	public function undo(): Number {
		if (oldStatus == "stated") { 
			argumentGraph.state(statement); 
		} else if (oldStatus == "questioned") { 
			argumentGraph.question(statement); 
		} else if (oldStatus == "rejected") { 
			argumentGraph.reject(statement); 
		} else if (oldStatus == "accepted") { 
			argumentGraph.accept(statement); 
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
		ArgumentControl.changeStatementId(statement, id);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		ArgumentControl.changeStatementId(statement, oldId);
		// argumentGraph.update();
		return GC.C_OK;
	}
}

public class ChangeStatementWffCommand extends UndoableCommand {

	public attribute statement: Statement;
	public attribute wff: String;
	public attribute oldWff: String;

	public function do(): Number {
		oldWff = statement.wff;
		ArgumentControl.changeStatementWff(statement, wff);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		ArgumentControl.changeStatementWff(statement, oldWff);
		// argumentGraph.update();
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
		argumentGraph.setProofStandard(statement, standard);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.setProofStandard(statement,oldStandard);
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
		ArgumentControl.changeArgumentId(argument, id);
		// argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		ArgumentControl.changeArgumentId(argument, oldId);
		// argumentGraph.update();
		return GC.C_OK;
	}
}

public class ChangeArgumentDirectionCommand extends UndoableCommand {

	public attribute argument: Argument;

	public function do(): Number {
		argumentGraph.switchDirection(argument);
		return GC.C_OK;
	}

	public function undo(): Number {
		argumentGraph.switchDirection(argument);
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
		ArgumentControl.changePremiseRole(premise, role);
		argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		ArgumentControl.changePremiseRole(premise, oldRole);
		argumentGraph.update();
		return GC.C_OK;
	}
}

public class NegatePremiseCommand extends UndoableCommand {
	
	public attribute premise: Premise;

	public function do(): Number {
		ArgumentControl.negatePremise(argumentGraph, premise);
		argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		ArgumentControl.negatePremise(argumentGraph, premise);
		argumentGraph.update();
		return GC.C_OK;
	}
}

public class ChangePremiseTypeCommand extends UndoableCommand {
	
	public attribute premise: Premise;

	public function do(): Number {
		premise.switchType();
		argumentGraph.update();
		return GC.C_OK;
	}

	public function undo(): Number {
		premise.switchType();
		argumentGraph.update();
		return GC.C_OK;
	}
}
