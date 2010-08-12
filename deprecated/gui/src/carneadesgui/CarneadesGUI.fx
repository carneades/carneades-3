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

package carneadesgui;

import carneadesgui.control.CarneadesControl;
import carneadesgui.view.CarneadesView;
import carneadesgui.model.CarneadesModel;
import carneadesgui.view.StandardView;
import carneadesgui.GC.*;

public class CarneadesGUI {
    var control: CarneadesControl = CarneadesControl {
	application: this
    };
    var standardView: CarneadesView = StandardView {};
    var model: CarneadesModel = CarneadesModel {};
	
	postinit {
		control.setModel(model);
		control.addView(standardView);
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




