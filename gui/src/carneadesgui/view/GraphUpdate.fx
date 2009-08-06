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

public class GraphUpdate {
	public var changedAttribute: Boolean = false;
	public var graphSelection: Boolean = false;
	public var listSelection: Boolean = false;
	public var selection: Boolean = bind graphSelection and listSelection;
	public var layout: Boolean = false;
	public var listView: Boolean = false;
}

