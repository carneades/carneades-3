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


package Carneades.Graph;

import javafx.scene.CustomNode;
import Carneades.Graph.*;

/**
 * Static variable counting all GraphElement instances so there are unique indices distributed.
 */
public var currentIndex: Integer = 0;

/**
 * Base class for every view element that is involved in drawing the view argument graph.
 */
public abstract class GraphElement extends CustomNode {

	/**
	 * Has the element been selected by the user?
	 */
	public attribute selected: Boolean = false;

	/**
	 * Unique index of the element.
	 */
	public attribute index: Integer = newIndex();

	/**
	 * Produce a new index value for the component. Should be made static.
	 */
	protected function newIndex(): Integer {
		currentIndex++;
		return currentIndex;
	}
}
