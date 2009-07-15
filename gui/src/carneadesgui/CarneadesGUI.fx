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
import carneadesgui.view.CarneadesView.StandardView;
import carneadesgui.view.CarneadesView;
import carneadesgui.model.CarneadesModel;

import java.io.File;

public class CarneadesGUI {
    var control: CarneadesControl = CarneadesControl {};
    var view: CarneadesView = StandardView {};
    var model: CarneadesModel = CarneadesModel {};

	postinit {
		control.setView(view);
		control.setModel(model);
		control.newGraph();
		//control.loadGraphFromFile(new File("Tweety.xml"));
	}

	public function display() {
		return view.view;
	}
}




