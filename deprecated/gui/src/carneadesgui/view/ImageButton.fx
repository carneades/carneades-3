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

import javafx.scene.CustomNode;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.effect.Effect;
import javafx.scene.effect.Glow;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

public class ImageButton extends CustomNode {
    public var action: function();

	public var image: Image;

	public var x: Integer;
	public var y: Integer;

	public var width: Integer;
	public var height: Integer;

	public var padding: Integer = 2;

	public var backColor: Color = Color.GREY;
	public var borderColor: Color = Color.WHITE;

	public var hoverGlow: Number = 0.3;

	public var hoverEffect: Effect = Glow { level: hoverGlow };

	public-read var armed: Boolean = false;
	

	override var onMouseClicked = function(e: MouseEvent) { armed = false; action(); }
	override var onMousePressed = function(e: MouseEvent) { armed = true }
	override var onMouseExited = function(e: MouseEvent) { armed = false }

	var imageView: ImageView = ImageView {
		x: bind x + padding
		y: bind y + padding
		fitWidth: bind width - padding*2
		fitHeight: bind height - padding*2
		smooth: true
		image: bind image
		effect: bind { if (hover and not armed) hoverEffect else null }
	}

	var backRect = Rectangle {
		x: bind x
		y: bind y
		width: bind width
		height: bind height
		fill: bind backColor
		stroke: bind borderColor
		effect: bind { if (hover and not armed) hoverEffect else null }
	}

	var coverRect = Rectangle {
		x: bind x
		y: bind y
		width: bind width
		height: bind height
		fill: bind Color.BLACK
		opacity: 0.3
	}

	override function create(): Node {
		Group {
			opacity: bind { if (disabled) 0.5 else 1.0 }
			content: bind [backRect, imageView, if (armed) coverRect else null]
		}
	}
}
