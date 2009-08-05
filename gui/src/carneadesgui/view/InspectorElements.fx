/*
 * InspectorElements.fx
 *
 * Created on 06.07.2009, 18:26:13
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
