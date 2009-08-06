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

/**
 * @author matthiasgrabmair
 */

import carneadesgui.GC.*;
import carneadesgui.control.CarneadesControl;
import carneadesgui.view.GraphUpdate;
import javafx.scene.input.MouseEvent;
import javafx.scene.control.Label;
import javafx.scene.control.TextBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Slider;
import javafx.scene.layout.LayoutInfo;
import javafx.scene.layout.Panel;

public class InspectorLabel extends Label {
	override var layoutInfo = LayoutInfo {
		minWidth: inspectorLabelWidth
		width: inspectorLabelWidth
	}
}

var rightColumnLayoutInfo: LayoutInfo = LayoutInfo {
	minWidth: inspectorPanelWidth - inspectorLabelWidth
	width: inspectorPanelWidth - inspectorLabelWidth - 10
}

public class InspectorTextBox extends TextBox {
	override var layoutInfo = rightColumnLayoutInfo;
}

public class InspectorRadioButton extends RadioButton {
	override var layoutInfo = rightColumnLayoutInfo;
}

/**
* Custom CheckBox class to make up for the missing action-attribute.
*/
public class InspectorCheckBox extends CheckBox {
	override var layoutInfo = rightColumnLayoutInfo;
	override var defined = true;
	public var action: function(): Void = null;
	override var onMouseReleased = function(e: MouseEvent) {
		if (not controlsLocked) action();
	}
}

/**
* Custom slider class to make up for the missing action-attribute.
*/
public class InspectorSlider extends Slider {
	override var layoutInfo = rightColumnLayoutInfo;
	var fires: Boolean = true;
	public var val: Number = 0.0;
	public var action: function(): Void = null;
	override var value = bind val on replace { if (fires) action(); }
	public function setValue(v: Number): Void {
		fires = false;
		val = v;
		fires = true;
	}
}

public abstract class Inspector extends Panel {
	public var control: CarneadesControl = null;
	public function update(u: GraphUpdate): Void {}
	public function reset(s: String): Void {
		p("{s} Inspector shall be shown without a model being loaded. Check for bug!")
	}
}
