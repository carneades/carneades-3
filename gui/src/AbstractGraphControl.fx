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
import java.io.File;
import java.lang.Object;

// Model imports
import GraphSketch1.Argument.Argument;
import GraphSketch1.Argument.Argument.*;

// Constants import
import GraphSketch1.Graph.GC;

public class AbstractGraphControl {

	// View configuration attributes
	public attribute possibleToAddArgument: Boolean = false;
	public attribute possibleToAddPremise: Boolean = false;
	public attribute possibleToAddConclusion: Boolean = true;

	public attribute possibleToInverseArgument: Boolean = false;

	public attribute possibleToDelete: Boolean = false ;
	
	public attribute possibleToUndo: Boolean = false;
	public attribute possibleToRedo: Boolean = false;
	
	public attribute dragging: Boolean = false;

	public attribute selectedArgumentEditable: Boolean = false;
	public attribute selectedStatementEditable: Boolean = false;
	public attribute selectedPremiseEditable: Boolean = false;

	// Drawing Functions
	public function updateAll(): Void {};

	// Model modification functions
	public function addArgumentToSelected(): Void {}
	public function addPremiseToSelected(): Void {}
	public function addStatement(): Void {}

	public function negatePremise(p: Premise): Void {}
	public function changePremiseRole(p: Premise, r: String): Void {}
	public function changePremiseType(p: Premise, v: String): Void {}

	public function changeStatementAssumption(s: Statement, v: String): Void {}
	public function changeStatementValue(s: Statement, v: String): Void {}
	public function changeStatementStatus(s: Statement, v: String): Void {}
	public function changeStatementId(s: Statement, i: String): Void {}
	public function changeStatementWff(s: Statement, c: String): Void {}
	public function changeStatementProofStandard(s: Statement, st: String, n: Boolean, c: Boolean): Void {}

	public function changeArgumentId(a: Argument, i: String): Void {}
	public function changeArgumentDirection(a: Argument, v: String): Void {}

	// Selection Functions
	public function deleteSelected(): Void {}

	public function unSelectAll(): Void {}
	public function unSelectGraph(): Void {}
	public function unSelectModels(): Void {}
	public function processSelection(): Void {}
	public function getSelected(): Object[] { return null; }
	public function getSelectedModel(): Object[] { return null; }
	public function selectModel(o: Object): Void {};

	// Dragging Function
	public function startDrag(): Void {}
	public function endDrag(): Void {}
	public function dragEndsAt(argumentElement): Void {}
	public attribute draggingOver = null;
	public function setDraggingOver(thing): Void { draggingOver = thing; };

	// Other editing functions
	public function undo(): Number { GC.C_OK }
	public function redo(): Number { GC.C_OK }

	// File functions
	public function loadGraphFromFile(f: File): Void {}
	public function saveGraphToFile(f: File): Void {}
	public function newGraph(): Void {}

	// Debug functions
	public function printSelected(): Void {}

}
