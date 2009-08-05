/*
 * CarneadesView.fx
 *
 * Created on 24.06.2009, 12:37:59
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

	public-read var active: Boolean = false;
	public function activate(): Void {
		active = true;
	}
	public function deactivate(): Void {
		active = false;
		//currentGraph = null;
	}

    public var view: Stage = Stage {
	title: bind displayTitle
	width: bind appWidth
	height: bind appHeight
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
    public function editNothing(): Void {}
    public function editStatement(s: Statement): Void {}
    public function editArgument(a: Argument): Void {}
    public function editPremise(p: Premise): Void {}
    public function editGraph(a: ArgumentGraph): Void {}
    public function isVisible(e: GraphElement): Boolean { false }
    public function alert(t: String): Void {}
    public function unSelectAll(): Void {}
    public function quit(): Void { view.close(); }
}


