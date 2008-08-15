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

import Carneades.Graph.*;
import Carneades.Graph.GC.*;
import java.lang.System;

public abstract class GraphLayout {
	// This would normally be a Graph object, but the inverse bind forces the classes to exactly match.
	public attribute graph: Graph;
	
	public attribute width: Integer;
	public attribute height: Integer;
	attribute xOffset: Integer = xOffset;
	attribute yOffset: Integer = yOffset;
	attribute d: Boolean = bind debug; // debug messages?

	public function compose():Graph { graph }

}
