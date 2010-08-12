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

package carneadesgui.control;

import java.io.File;
import java.lang.Exception;
import java.lang.System;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;

/**
* Wrapper class for org.w3c.dom.Node
*/
public class XWNode {
	function beParent() {
		for (c in children) c.parent = this;
	}

	postinit {
		beParent();
	}

	public var children: XWNode[] = [];
	public var document: XWDocument;
	public var parent: XWNode;
	public var name: String;
	public var value: String;
}

/**
* Wrapper class for Attributes.
*/
public class XWAttribute extends XWNode {
	override function toString() {
		' {name}="{value}"'
	}
}

/**
* Wrapper class for org.w3c.dom.Element
*/
public class XWElement extends XWNode {
	public var attributes: XWAttribute[] = [];

	public function getAllChildrenElements(): XWElement[] {
		[ for (e in children where e instanceof XWElement)
			{[e as XWElement, (e as XWElement).getAllChildrenElements()]} ]
	}

	override function toString() {
		"<{name}{attributes}>{children}</{name}>\n"
	}
}

/**
* Wrapper class for text nodes
*/
public class XWText extends XWNode {
	override function toString() {
		"{value}"
	}
}

/**
* Wrapper class for org.w3c.dom.Document
*/
public class XWDocument extends XWNode{
	public var documentElement: XWElement = null;

	public var version: String = '';
	public var schema: String = '';

	public function getAllElements(): XWElement[] {
		[documentElement, documentElement.getAllChildrenElements()]
	}

	public function getElementsByTagName(tag: String): XWElement[] {
		[for (e in getAllElements() where e.name == tag) {e}]
	}

	override function toString() {
		"{version}\n{schema}\n{documentElement}"
	}
}

public class XWDocumentBuilder {

	function toXWAttributes(n: NamedNodeMap): XWAttribute[] {
		var attributes: XWAttribute[] = [];

		for (i: Integer in [0 .. n.getLength()-1]) {
			insert XWAttribute {
				name: n.item(i).getNodeName()
				value: n.item(i).getNodeValue()
			} into attributes;
		}
		return attributes;
	}

	function toXWElement(element: Node, parent: XWNode, document: XWDocument): XWElement {
		var xw: XWElement;

		xw = XWElement {
			name: element.getNodeName()
			value: element.getNodeValue()
			document: document
			parent: parent
			attributes: toXWAttributes(element.getAttributes())
		};

		for (i: Integer in [0 .. element.getChildNodes().getLength()-1]) {
			var e: Node = element.getChildNodes().item(i);
			var ev: String = e.getNodeValue();
			if (e.getNodeName() == "#text" and ev != "") {
				//System.out.println("text node added: {e.getNodeValue()}");
				insert XWText {
					name: "#text"
					value: e.getNodeValue()
					document: document
					parent: parent
				} into xw.children;
				parent.value = e.getNodeValue();
			} else {
				//System.out.println("child added");
				insert toXWElement(e, xw, document) into xw.children;
			}
		}

		return xw;
	}

	public function parseFile(file: File): XWDocument {
		var xwDocument: XWDocument;
		try {

		var factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance();
		var builder: DocumentBuilder = factory.newDocumentBuilder();
		var document: Document = builder.parse(file);

		if (document != null) {
			//System.out.println("document parsed");

			// parse the file into the XWDocument object
			xwDocument = XWDocument {};
			xwDocument.documentElement = toXWElement(document.getDocumentElement(), xwDocument, xwDocument);
		} else {
			System.out.println("Document not parsed!")
		}

		} catch (e: Exception) {
			System.out.println("Exception occured during parsing!")
		}

		return xwDocument;
	}
}
