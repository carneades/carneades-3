/*
 * GraphPanel.fx
 *
 * Created on 02.07.2009, 01:31:36
 */

package carneadesgui.view;

import javafx.animation.Interpolator;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.scene.Group;
import javafx.scene.shape.Circle;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.LayoutInfo;
import javafx.scene.layout.Panel;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.transform.Scale;
import javafx.scene.transform.Translate;
import carneadesgui.control.CarneadesControl;
import carneadesgui.GC.*;

/**
* Panel displaying the graph in the standard way. Its layout elements are set to work with the StandardView. If used elsewhere, override them accordingly.
*/
public class GraphPanel extends Panel {
	public var graph: Graph = null;
	public var control: CarneadesControl = null;
	public var constraintX: Number = 0;
	public var constraintY: Number = 0;

	var centerX: Number = bind this.width / 2;
	var centerY: Number = bind this.height / 2;

	var shiftX: Number = 0;
	var shiftY: Number = 0; // x- and y-shift due to element focusing

	var zoom: Number = 1.0; // Zoom factor

	override var layoutInfo = LayoutInfo {
		width: bind appWidth - constraintX - horizontalWindowMismatch;
		height: bind appHeight - constraintY - verticalWindowMismatch;
	}

	var centerCircle = Circle {
		centerX: this.centerX
		centerY: this.centerY
		radius: 3
		fill: Color.RED
	}

	var detectorRect: Rectangle = Rectangle {
		x: 0
		y: 0
		width: bind this.width
		height: bind this.height
		fill: Color.TRANSPARENT
		blocksMouse: false

		onMouseWheelMoved: function(e: MouseEvent) {
			if (not controlsLocked) {
				// zoom on mousewheel movement
				var newZoom: Number = zoom;
				newZoom -= e.wheelRotation / 10;
				if (newZoom >= zoomLimits[0] and newZoom <= zoomLimits[1]) zoom = newZoom;
			}
		}

		onMouseClicked: function(e: MouseEvent) {
			if (not controlsLocked) {
				// unselect on rightclick
				if (e.button == MouseButton.SECONDARY)  {
					control.unSelectAll();
				}
			}
		}
	}

	/**
	* The function that centers the view on a given graph element.
	*/
	public function focusOn(e: GraphElement): Void {
		if (e instanceof Vertex) {
			var oldShiftX: Number = shiftX;
			var oldShiftY: Number = shiftY;
			Timeline {
				repeatCount: 1
				keyFrames: [
					KeyFrame {
						time: 0s
						action: function(): Void {
							controlsLocked = true;
						}
						values: [
							shiftX => oldShiftX,
							shiftY => oldShiftY
						]
					},
					KeyFrame {
						time: 1s
						action: function(): Void {
							controlsLocked = false;
						}
						values: [
							shiftX => (e as Vertex).x tween Interpolator.EASEBOTH,
							shiftY => (e as Vertex).y tween Interpolator.EASEBOTH
						]
					}
				]
			}.play();
		}
	}

	override var clip = Rectangle {
		width: bind this.width
		height: bind this.height
	}

	override var content = bind [
		LayoutRect {fill: viewBackground},
		Filler { content: bind "{this.width}\n{this.centerX}\n{this.centerY}" },
		Group {
			content: bind [
				Group {
					transforms: bind [
						Translate {
							x: bind centerX - shiftX
							y: bind centerY - shiftY
						},
						Scale {
							x: zoom
							y: zoom
							pivotX: shiftX
							pivotY: shiftY
						}
					]
					content: bind graph
				},
				detectorRect
			]
		}
	]
}
