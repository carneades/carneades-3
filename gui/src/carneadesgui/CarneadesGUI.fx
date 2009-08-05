/*
 * CarneadesGUI.fx
 *
 * Created on 24.06.2009, 12:22:01
 */

package carneadesgui;

/**
 * @author matthiasgrabmair
 */

import carneadesgui.control.CarneadesControl;
import carneadesgui.view.CarneadesView;
import carneadesgui.model.CarneadesModel;
import carneadesgui.view.StandardView;
import carneadesgui.view.RichView;
import carneadesgui.GC.*;

public class CarneadesGUI {
    var control: CarneadesControl = CarneadesControl {
	application: this
    };
    var standardView: CarneadesView = StandardView {};
    //var richView: CarneadesView = RichView {};
    var model: CarneadesModel = CarneadesModel {};
	
	postinit {
		control.setModel(model);
		control.addView(standardView);
		//control.addView(richView);
		control.setActiveView(standardView);
		control.newGraph();
	}

	public function display() {
		return control.view.view; // yes I know this reads weird ...
	}

	public function quit() {
	    //richView.quit();
	    standardView.quit();
	}
}




