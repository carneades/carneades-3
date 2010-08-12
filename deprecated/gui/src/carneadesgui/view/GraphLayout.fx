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


package carneadesgui.view;

import carneadesgui.view.*;
import carneadesgui.GC.*;

public class GraphLayout {
	// This would normally be a Graph object, but the inverse bind forces the classes to exactly match.
	public var graph: Graph;

	public var width: Integer;
	public var height: Integer;
	var xOffset: Integer = 0;
	var yOffset: Integer = 0;

	public function compose():Graph { graph }

}
