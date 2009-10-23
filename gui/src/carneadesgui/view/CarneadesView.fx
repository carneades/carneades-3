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

package carneadesgui.view;


// general imports
import javafx.scene.Scene;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;

// control imports
import carneadesgui.GC.*;
import carneadesgui.control.CarneadesControl;

// model imports
import carneadesgui.model.Argument;
import carneadesgui.model.Argument.*;

// view imports
import carneadesgui.view.GraphUpdate;

import javafx.scene.input.MouseEvent;

/**
* The base class for view components to the Carneades GUI. A new view component should subclass it and override attributes and methods as needed.
*/
public abstract class CarneadesView {
	// The array of graph objects that correspond to the model graphs.
	public var graphs: CarneadesGraph[] = [];

	// The view graph object currently displayed.
	public var currentGraph: CarneadesGraph;

	public var control: CarneadesControl = null;

	public var mode: Integer = inspectorDefaultMode;

	public var displayTitle: String = "Carneades GUI";
	protected var aboutInformationDisplayed: Boolean = false;

	public-read var active: Boolean = false;
	public function activate(): Void {
		active = true;
	}
	public function deactivate(): Void {
		active = false;
	}

    public var view: Stage = Stage {
	title: bind displayTitle
	width: bind APP_WIDTH
	height: bind APP_HEIGHT
	scene: Scene {
		content: [
			Text {
				font : Font {
					size : 16
				}
				x: 10
				y: 30
				content: "This is the default view of the Carneades GUI"
			}
			]
		}
    }

    public function update(u: GraphUpdate): Void {}
    public function focusOn(e: GraphElement): Void {}
	public function changeZoom(s: Number): Void {}
	public function resetZoom(): Void {}
    public function editNothing(): Void {}
    public function editStatement(s: Statement): Void {}
    public function editArgument(a: Argument): Void {}
    public function editPremise(p: Premise): Void {}
    public function editGraph(a: ArgumentGraph): Void {}
	public function displayGraphListView(): Void {}
    public function isVisible(e: GraphElement): Boolean { false }
    public function alert(t: String): Void {}
    public function unSelectAll(): Void {}
	public function displayAboutInformation(): Void {}
    public function quit(): Void { view.close(); }
}


