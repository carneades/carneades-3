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

// Control Imports
import carneadesgui.control.CarneadesControl;

// Model imports
import carneadesgui.model.Argument.*;
import carneadesgui.model.Argument;

// View imports
import carneadesgui.GC.*;

// GENERAL CLASSES

/**
 * Base class for a command.
 */
public abstract class Command {

	/**
	 * The model argument graph associated with the administering CommandControl object.
	 */
	public var argumentGraph: ArgumentGraph;

	/**
	 * The administering CommandControl object.
	 */
	public var commandControl: CommandControl;

	/**
	 * Does this command have the ability to merge with other commands?
	 */
	public var merges: Boolean = false;

	/**
	 * Execute the command.
	 */
	public function do(): Number {
		return C_OK;
	}

	/**
	 * Function computing whether a certain command can be merged with the previous command to a single one.
	 */
	public function mergeable(c: Command): Boolean {false}

	/**
	 * Merge the command with the previous one, that is the one below it on the command stack.
	 */
	public function merge(c: Command): Number { C_OK }
}

/**
 * Base class for undoable commands.
 */
public class UndoableCommand extends Command {

	/**
	 * Undo the command.
	 */
	public function undo(): Number {
		return C_OK;
	}
}

/**
 * The control object administering a command stack.
 */
public class CommandControl {

	/**
	 * The application's control object.
	 */
	public var control: CarneadesControl;

	/**
	 * The command stack.
	 */
	var commands: Command[] = [];

	/**
	 * The size of the command stack for easy handling. Bound and read-only.
	 */
	var size: Integer = bind sizeof commands;

	/**
	 * The bookmark determines where we are in the undo-stack. It usually is 0, meaning that we have undone 0 commands. Whenever an undo is called, it increases by one. Hence, a redo decreases it by one.
	 */
	var bookmark: Integer = 0;

	public function possibleToUndo(): Boolean { return (size - bookmark > 0); }

	public function possibleToRedo(): Boolean { return (bookmark > 0); }

	/**
	 * Pops off the topmost elements until the bookmark. The function is void and serves the purpose to allow for an auto-undo in case of e.g. cyclic graphs
	 */
	public function pop(): Void {
		// throw away everything beyond the bookmark
		for (i in [1 .. bookmark]) {
			delete commands[size-1];
		}
		bookmark = 0;
	}

	/**
	 * Execute the passed command.
	 */
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

	/**
	 * Undo the last command before the bookmark.
	 */
	public function undo(): Number {
		var result: Number = C_NO_UNDO;

		if (commands[size - bookmark - 1] instanceof UndoableCommand) {
			result = (commands[size - bookmark - 1] as UndoableCommand).undo();
			bookmark += 1;
		}

		return result;
	}

	/**
	 * Redo the command after the bookmark.
	 */
	public function redo(): Number {
		var result: Number = C_LATEST_COMMAND;

		if (bookmark > 0 and bookmark <= size) {
			result = commands[size - bookmark].do();
			bookmark -= 1;
		}
		return result;
	}

	/**
	 * Empty the command stack and reset the bookmark.
	 */
	public function reset(): Void {
		commands = [];
		bookmark = 0;
	}

}

// SPECIFIC COMMANDS
// Hopefully self-explanatory. A detailed description will be coming.

public class AddArgumentCommand extends UndoableCommand {

	public var statement: Statement;
	public var argument: Argument;

	postinit {
		argument = Argument {
			graph: argumentGraph;
			id: argumentGraph.getNewArgumentId();
			conclusion: statement
		}
	}

	override function do(): Number {
		argumentGraph.insertArgument(argument);
		return C_OK;
	}

	override function undo(): Number {
		argumentGraph.deleteArgument(argument);
		return C_OK;
	}
}

public class AddStatementCommand extends UndoableCommand {

	public var statement: Statement;

	override function do(): Number {
		// set a default value in case no statement has been passed
		if (statement == null){
			statement = Statement {
				id: argumentGraph.getNewStatementId();
				wff: "New Statement";
				graph: argumentGraph
			}
		}
		argumentGraph.insertStatement(statement);
		return C_OK;
	}

	override function undo(): Number {
		argumentGraph.deleteStatement(statement);
		return C_OK;
	}
}

public class CopyPremiseCommand extends UndoableCommand {
	public var premise: Premise;
	public var newArgument: Argument;

	override function do(): Number {
		// 1. add premise to new node
		newArgument.appendPremise(premise);
		return C_OK;
	}

	override function undo(): Number {
		// 1. remove premise from argument
		newArgument.deletePremise(premise);
		return C_OK;
	}
}

public class AddPremiseCommand extends UndoableCommand {

	public var argument: Argument;
	public var premise: Premise;
	public var statement: Statement;

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

	override function do(): Number {
		argument.addPremise(premise);
		return C_OK;
	}

	override function undo(): Number {
		argumentGraph.deleteStatement(premise.statement);
		argument.deletePremise(premise);
		return C_OK;
	}
}

public class AddArgumentAndPremiseCommand extends UndoableCommand {

	public var statement: Statement;
	public var argument: Argument;
	public var premise: Premise;
	public var pstatement: Statement;

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

	override function do(): Number {
		argument.addPremise(premise);
		argumentGraph.insertArgument(argument);
		return C_OK;
	}

	override function undo(): Number {
		argumentGraph.deleteStatement(premise.statement);
		argument.deletePremise(premise);
		argumentGraph.deleteArgument(argument);
		return C_OK;
	}
}

public class RemoveArgumentCommand extends UndoableCommand {

	public var argument: Argument;

	override function do(): Number {
		argumentGraph.deleteArgument(argument);
		return C_OK;
	}

	override function undo(): Number {
		argumentGraph.insertArgument(argument);
		return C_OK;
	}
}

public class DeleteStatementCommand extends UndoableCommand {
	public var statement: Statement;
	var premises: Premise[];

	override function do(): Number {
		argumentGraph.deleteStatement(statement);
		return C_OK;
	}

	override function undo(): Number {
		argumentGraph.insertStatement(statement);
		return C_OK;
	}
}

public class DeleteConclusionCommand extends UndoableCommand {
	// Is called whenever a statement is deleted from the graph that has arguments attached to it.
	public var conclusion: Statement;
	public var motherArgument: Argument;
	var premise: Premise;
	public var childArguments: Argument[];

	override function do(): Number {
		if (motherArgument != null) {
			// if there is a mother argument, get the corresponding premise
			premise = motherArgument.premises[p | p.statement == conclusion][0];
			motherArgument.deletePremise(premise);
			if (motherArgument.premises == []) argumentGraph.deleteArgument(motherArgument);
		}
		for (c in childArguments) argumentGraph.deleteArgument(c);

		if (conclusion.arguments == [] and not argumentGraph.isConclusion(conclusion))
			argumentGraph.deleteStatement(conclusion);
		return C_OK;
	}

	override function undo(): Number {
		if (motherArgument != null) {
			if (motherArgument.premises == []) argumentGraph.insertArgument(motherArgument);
			motherArgument.appendPremise(premise);
		}
		for (c in childArguments) { insert c into argumentGraph.arguments; }

		if (not isMemberOf(conclusion, argumentGraph.statements)) argumentGraph.insertStatement(conclusion);
		
		return C_OK;
	}
}

public class DeletePremiseCommand extends UndoableCommand {
	public var premise: Premise;
	public var argument: Argument;

	override function do(): Number {
		argument.deletePremise(premise);
		return C_OK;
	}

	override function undo(): Number {
		argument.appendPremise(premise);
		return C_OK;
	}
}

public class DeletePremiseStatementCommand extends UndoableCommand {
	public var premise: Premise;
	public var argument: Argument;

	override function do(): Number {
		argument.deletePremise(premise);
		if (premise.statement.arguments == [] and not argumentGraph.isConclusion(premise.statement))
			argumentGraph.deleteStatement(premise.statement);
		return C_OK;
	}

	override function undo(): Number {
		argument.appendPremise(premise);
		if (not isMemberOf(premise.statement, argumentGraph.statements)) argumentGraph.insertStatement(premise.statement);
		return C_OK;
	}
}

public class MovePremiseCommand extends UndoableCommand {
	public var premise: Premise;
	public var oldArgument: Argument;
	public var newArgument: Argument;

	override function do(): Number {
		// 1. remove premise from argument
		oldArgument.deletePremise(premise);
		// 2. add premise to new node
		newArgument.appendPremise(premise);
		return C_OK;
	}

	override function undo(): Number {
		// 1. remove premise from argument
		newArgument.deletePremise(premise);
		// 2. add premise to old node
		oldArgument.appendPremise(premise);
		return C_OK;
	}
}

public class MoveArgumentCommand extends UndoableCommand {
	public var argument: Argument;
	public var oldStatement: Statement;
	public var newStatement: Statement;

	override function do(): Number {
		argument.conclusion = newStatement;
		return C_OK;
	}

	override function undo(): Number {
		argument.conclusion = oldStatement;
		return C_OK;
	}
}

public class ChangeStatementStatusCommand extends UndoableCommand {

	public var statement: Statement;
	public var oldStatus: String;
	public var newStatus: String;

	override function do(): Number {

	    oldStatus = statement.status;
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
	    return C_OK;
	}

	override function undo(): Number {
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
	    return C_OK;
	}
}

public class ChangeStatementIdCommand extends UndoableCommand {

	public var statement: Statement;
	public var id: String;
	public var oldId: String;

	override function do(): Number {
		oldId = statement.id;
		statement.id = id;
		//argumentGraph.setStatementId(statement, id);
		return C_OK;
	}

	override function undo(): Number {
		statement.id = oldId;
		//argumentGraph.setStatementId(statement, oldId);
		return C_OK;
	}
}

public class ChangeStatementWffCommand extends UndoableCommand {

	public var statement: Statement;
	public var wff: String;
	public var oldWff: String;

	override function do(): Number {
		oldWff = statement.wff;
		statement.wff = wff;
		return C_OK;
	}

	override function undo(): Number {
		statement.wff = oldWff;
		return C_OK;
	}
}

public class ChangeGraphTitleCommand extends UndoableCommand {

	public var title: String;
	public var oldTitle: String;

	override function do(): Number {
		oldTitle = argumentGraph.title;
		argumentGraph.title = title;
		return C_OK;
	}

	override function undo(): Number {
		argumentGraph.title = oldTitle;
		return C_OK;
	}
}

public class ChangeArgumentSchemeCommand extends UndoableCommand {

	public var argument: Argument;
	public var scheme: String;
	public var oldScheme: String;

	override function do(): Number {
		oldScheme = argument.scheme.id;
		argument.scheme.id = scheme;
		return C_OK;
	}

	override function undo(): Number {
		argument.scheme.id = oldScheme;
		return C_OK;
	}
}

public class ChangeStatementStandardCommand extends UndoableCommand {

	public var statement: Statement;
	public var standard: ProofStandard;
	public var oldStandard: ProofStandard;

	override function do(): Number {
		// backup old standard via deep copy
		if (statement.standard instanceof Scintilla) {
			oldStandard = Scintilla {};
		} else if (statement.standard instanceof DialecticalValidity) {
			oldStandard = DialecticalValidity {};
		} else if (statement.standard instanceof Preponderance) {
			oldStandard = Preponderance {};
		} else if (statement.standard instanceof ClearAndConvincingEvidence) {
			oldStandard = ClearAndConvincingEvidence {};
		} else if (statement.standard instanceof BeyondReasonableDoubt) {
			oldStandard = BeyondReasonableDoubt {};
		}
		// Set new Standard
		statement.standard = standard;
		standard.statement = statement;
		return C_OK;
	}

	override function undo(): Number {
		statement.standard = oldStandard;
		return C_OK;
	}
}

// for arguments

public class ChangeArgumentIdCommand extends UndoableCommand {

	public var argument: Argument;
	public var id: String;
	public var oldId: String;

	override function do(): Number {
		oldId = argument.id;
		argument.id = id;
		return C_OK;
	}

	override function undo(): Number {
		argument.id = oldId;
		return C_OK;
	}
}

public class ChangeArgumentTitleCommand extends UndoableCommand {

	public var argument: Argument;
	public var title: String;
	public var oldTitle: String;

	override function do(): Number {
		oldTitle = argument.title;
		argument.title = title;
		return C_OK;
	}

	override function undo(): Number {
		argument.title = oldTitle;
		return C_OK;
	}
}

public class ChangeArgumentWeightCommand extends UndoableCommand {

	public var argument: Argument;
	public var weight: Number;
	public var oldWeight: Number;
	override var merges = true;

	override function do(): Number {
		oldWeight = argument.weight;
		argument.weight = weight;
		return C_OK;
	}

	override function mergeable(c: Command): Boolean {
		return argument == (c as ChangeArgumentWeightCommand).argument;
	}

	override function merge(c: Command): Number {
		(c as ChangeArgumentWeightCommand).weight = weight;
		argument.weight = weight;
		return C_OK;
	}

	override function undo(): Number {
		argument.weight = oldWeight;
		return C_OK;
	}
}

public class ChangeArgumentDirectionCommand extends UndoableCommand {

	public var argument: Argument;

	override function do(): Number {
		argument.switchDirection();
		return C_OK;
	}

	override function undo(): Number {
		argument.switchDirection();
		return C_OK;
	}
}

// ... and for premises

public class ChangePremiseRoleCommand extends UndoableCommand {

	public var premise: Premise;
	public var role: String;
	public var oldRole: String;

	override function do(): Number {
		oldRole = premise.role;
		premise.role = role;
		//argumentGraph.setPremiseRole(premise, role);
		return C_OK;
	}

	override function undo(): Number {
		//argumentGraph.setPremiseRole(premise, oldRole);
		premise.role = oldRole;
		return C_OK;
	}
}

public class NegatePremiseCommand extends UndoableCommand {

	public var premise: Premise;

	override function do(): Number {
		//argumentGraph.negatePremise(premise);
		if (premise.negative) { premise.negative = false } else { premise.negative = true }
		return C_OK;
	}

	override function undo(): Number {
		//argumentGraph.negatePremise(premise);
		if (premise.negative) { premise.negative = false } else { premise.negative = true }
		return C_OK;
	}
}

public class ChangePremiseTypeCommand extends UndoableCommand {

	public var premise: Premise;

	override function do(): Number {
		if (premise.exception) { premise.exception = false } else { premise.exception = true }
		//argumentGraph.switchPremiseType(premise);
		return C_OK;
	}

	override function undo(): Number {
		if (premise.exception) { premise.exception = false } else { premise.exception = true }
		//argumentGraph.switchPremiseType(premise);
		return C_OK;
	}
}
