/*
 * GraphUpdate.fx
 *
 * Created on 10.07.2009, 12:45:20
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

