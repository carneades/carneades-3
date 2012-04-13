/********************************************************************************
 *                                                                              *
 *  (c) Copyright 2011 University of Leeds, UK                                  *
 *  (c) Copyright 2010 The Open University UK                                   *
 *                                                                              *
 *  This software is freely distributed in accordance with                      *
 *  the GNU Lesser General Public (LGPL) license, version 3 or later            *
 *  as published by the Free Software Foundation.                               *
 *  For details see LGPL: http://www.fsf.org/licensing/licenses/lgpl.html       *
 *               and GPL: http://www.fsf.org/licensing/licenses/gpl-3.0.html    *
 *                                                                              *
 *  This software is provided by the copyright holders and contributors "as is" *
 *  and any express or implied warranties, including, but not limited to, the   *
 *  implied warranties of merchantability and fitness for a particular purpose  *
 *  are disclaimed. In no event shall the copyright owner or contributors be    *
 *  liable for any direct, indirect, incidental, special, exemplary, or         *
 *  consequential damages (including, but not limited to, procurement of        *
 *  substitute goods or services; loss of use, data, or profits; or business    *
 *  interruption) however caused and on any theory of liability, whether in     *
 *  contract, strict liability, or tort (including negligence or otherwise)     *
 *  arising in any way out of the use of this software, even if advised of the  *
 *  possibility of such damage.                                                 *
 *                                                                              *
 ********************************************************************************/

function convertCohereJsonToD3 (cohereJson) {
		var d3Json = {
				nodes: [],
				links: []
		};

		var nodePositions = {};
		var connections = cohereJson.connectionset[0].connections;

		function nodeExists(theNode) {
				for (var i=0, len=d3Json.nodes.length; i<len; i++) {
						if (theNode.nodeid == d3Json.nodes[i].nodeid) {
								return true;
						}
				}

				return false;
		}

		for (var i=0, len=connections.length; i<len; i++) {
				// First deal with the nodes
				//var fromCnode =				Object.clone(connections[i].connection.from[0].cnode);
				var fromCnode = jQuery.extend(true, {}, connections[i].connection.from[0].cnode);

				//var toCnode =					Object.clone(connections[i].connection.to[0].cnode);
				var toCnode = jQuery.extend(true, {}, connections[i].connection.to[0].cnode);
				if (!nodeExists(fromCnode)) {
						var position = d3Json.nodes.push(fromCnode) - 1;
						nodePositions[fromCnode.nodeid] = position;
				}

				if (!nodeExists(toCnode)) {
						var position = d3Json.nodes.push(toCnode) - 1;
						nodePositions[toCnode.nodeid] = position;
				}

				// Now deal with the links
				var newLink = {};
				newLink.connid = connections[i].connection.connid;
				newLink.source = nodePositions[fromCnode.nodeid];
				newLink.target = nodePositions[toCnode.nodeid];
				newLink.label =
						connections[i].connection.linktype[0].linktype.label;
				newLink.polarity =
						connections[i].connection.linktype[0].linktype.grouplabel
						.toLowerCase();
				d3Json.links.push(newLink);
		}

		return d3Json;
}

/**
 * 
 * @param data Json Data
 */
function drawNetwork(data) {
	
		//converts Json Data to D3 data
		var data = convertCohereJsonToD3(data);
		
	
		// Set width & height for SVG
		//var networkDiv = new Element("div", {"id":"network-div"});
		$("#graph").html("<div id='network-div'></div>");
		//alert("works");
		//document.getElementById("tab-content-conn").innerHTML = "hello again";
		
		//document.getElementById("tab-content-conn").insert(networkDiv);

		var w = document.getElementById('graph').offsetWidth - 30;
		var h = getWindowHeight()-200;

		var vis = d3.select("#network-div")
				.append("svg:svg")
				.attr("width", w)
				.attr("height", h);

		vis.style("opacity", 1e-6)
				.transition()
				.duration(1000)
				.style("opacity", 1);

		var defs = vis.append("svg:defs");

		// Run the force directed layout algorithm
		var force = d3.layout.force()
				.charge(-2500)
				.linkDistance(250)
				.nodes(data.nodes)
				.links(data.links)
				.size([w, h])
				.start();

		// First draw the links
		var linkColors = new Object();
		linkColors.positive = "#2ca02c";
		linkColors.negative = "#d62728";
		linkColors.neutral = "#c7c7c7";

		var link = vis.selectAll("g.link")
				.data(data.links)
				.enter().append("svg:g")
				.attr("class", "link");

		link.append("svg:path")
				.attr("id",
							function(d) {
									return "path"+d.source.index+"_"+d.target.index;
							})
				.attr("label", function(d) { return d.label; })
				.attr("stroke", function(d) {return linkColors[d.polarity]; })
				.attr("d",
							function(d) {
									return moveto(d) + lineto(d);
							})
				.attr("marker-end", "url(#arrowhead)");

		defs.append("svg:marker")
				.attr("id", "arrowhead")
				.attr("viewBox","0 0 20 20")
				.attr("refX","30")
				.attr("refY","10")
				.attr("markerUnits","strokeWidth")
				.attr("markerWidth","11")
				.attr("markerHeight","7")
				.attr("orient","auto")
				.append("svg:path")
				.attr("d","M 0 0 L 20 10 L 0 20 z");

		function moveto (d) {
				// Locate the node where the path will start
				var node = d3.select("#node"+d.source.index);

				// Retrieve the width and height attributes...
				var w = parseFloat(node.attr("width"));
				var h = parseFloat(node.attr("height"));

				// ...so we can change the x,y coordinates of the node to be
				// at its center rather than the top-left corner
				d.source.newX = d.source.x + (w/2);
				d.source.newY = d.source.y + (h/2);
				
				return "M" + d.source.newX + "," + d.source.newY;
		};

		function lineto (d) {
				// Locate the node where the path will end
				var node = d3.select("#node"+d.target.index);

				// Retrieve the width and height attributes...
				var w = parseFloat(node.attr("width"));
				var h = parseFloat(node.attr("height"));

				// ...so we can locate the x,y coordinates of the center of
				// the node...
				d.target.centerX = d.target.x + (w/2);
				d.target.centerY = d.target.y + (h/2);

				// ...which we will use to calculate the x,y coordinates of
				// the point on the perimeter of the node where the path will
				// end -- the idea is that the arrowhead at the end of the
				// path is "smart" enough to move around the perimeter of the
				// rectangular node as the node moves around the screen.
				smartPathEnd(d, w, h);
				
				return " L" + d.target.newX + "," + d.target.newY;
		};

		/* We want to the end of the path to be able to move around the
			 perimeter of the rectangular node as the node moves around the
			 screen.
			 We achieve this by using trigonometry to work out where an
			 imaginary path to the center of the target node would intersect
			 the perimeter of the node, and then drawing the actual path
			 from source node to this intersection point. */
		function smartPathEnd(d, w, h) {

				// We need to work out the (tan of the) angle between the
				// imaginary horizontal line running through the center of the
				// target node and the imaginary line connecting the center of
				// the target node with the top-left corner of the same
				// node. Of course, this angle is fixed.
				var tanRatioFixed =
						(d.target.centerY - d.target.y)
						/
						(d.target.centerX - d.target.x);

				// We also need to work out the (tan of the) angle between the
				// imaginary horizontal line running through the center of the
				// target node and the imaginary line connecting the center of
				// the target node with the center of the source node. This
				// angle changes as the nodes move around the screen.
				var tanRatioMoveable =
						Math.abs(d.target.centerY - d.source.newY)
						/
						Math.abs(d.target.centerX - d.source.newX); // Note,
						// JavaScript handles division-by-zero by returning
						// Infinity, which in this case is useful, especially
						// since it handles the subsequent Infinity arithmetic
						// correctly.

				// Now work out the intersection point
				
				if (tanRatioMoveable == tanRatioFixed) {
						// Then path is intersecting at corner of textbox so draw
						// path to that point

						// By default assume path intersects a left-side corner
						d.target.newX = d.target.x;

						// But...
						if (d.target.centerX < d.source.newX) {
								// i.e. if target node is to left of the source node
								// then path intersects a right-side corner
								d.target.newX = d.target.x + w;
						}

						// By default assume path intersects a top corner
						d.target.newY = d.target.y;

						// But...
						if (d.target.centerY < d.source.newY) {
								// i.e. if target node is above the source node
								// then path intersects a bottom corner
								d.target.newY = d.target.y + h;
						}
				}

				if (tanRatioMoveable < tanRatioFixed) {
						// Then path is intersecting on a vertical side of the
						// textbox, which means we know the x-coordinate of the
						// path endpoint but we need to work out the y-coordinate

						// By default assume path intersects left vertical side
						d.target.newX = d.target.x;

						// But...
						if (d.target.centerX < d.source.newX) {
								// i.e. if target node is to left of the source node
								// then path intersects right vertical side
								d.target.newX = d.target.x + w;
						}

						// Now use a bit of trigonometry to work out the y-coord.

						// By default assume path intersects towards top of node								
						d.target.newY =
								d.target.centerY - ((d.target.centerX - d.target.x)
																		*
																		tanRatioMoveable);

						// But...
						if (d.target.centerY < d.source.newY) {
								// i.e. if target node is above the source node
								// then path intersects towards bottom of the node
								d.target.newY = (2 * d.target.y) - d.target.newY + h;
						}
				}

				if (tanRatioMoveable > tanRatioFixed) {
						// Then path is intersecting on a horizontal side of the
						// textbox, which means we know the y-coordinate of the
						// path endpoint but we need to work out the x-coordinate

						// By default assume path intersects top horizontal side
						d.target.newY = d.target.y;

						// But...
						if (d.target.centerY < d.source.newY) {
								// i.e. if target node is above the source node
								// then path intersects bottom horizontal side
								d.target.newY = d.target.y + h;
						}

						// Now use a bit of trigonometry to work out the x-coord.

						// By default assume path intersects towards lefthand side
						d.target.newX =
								d.target.centerX - ((d.target.centerY - d.target.y)
																		/
																		tanRatioMoveable);

						// But...
						if (d.target.centerX < d.source.newX) {
								// i.e. if target node is to left of the source node
								// then path intersects towards the righthand side
								d.target.newX = (2 * d.target.x) - d.target.newX + w;
						}
				}
		}

		link.append("svg:text")
				.attr("font-size", 10)
		
		    //Put label in the middle of the line
				.attr("x",
							function(d){
									return (d.target.newX + d.source.newX) / 2;
							})
				.attr("y",
							function(d){
									return (d.target.newY + d.source.newY) / 2;
							})
				.text(function(d){return d.label;});

		// Now draw the nodes
		var node = vis.selectAll("g.node")
				.data(data.nodes)
				.enter().append("svg:g")
				.attr("class", "node")
				.attr("id",
							function(d) {
									return "node"+d.index;
							})
				.attr("transform",
							function(d) {
									return "translate(" + d.x + "," + d.y + ")";
							})
				.call(force.drag);

		node.append("svg:title")
				.text(function(d) { return d.name; });

		node.append("svg:rect")
				.attr("rx", 3)
				.attr("filter", "url(#drop-shadow)")
				.style("stroke", "black")
				.style("fill", "aliceblue");

		node.append("svg:text")
				.attr("font-size", 10)
				.attr("y", 10)
				.attr("text-anchor", "start")
				.each(function(d) {
						// textFlow(myText,textToAppend,maxWidth,x,ddy,justified)
						var dy = textFlow(d.name, this, 225, 5, 10, false);

						// Get the bounding box of the text element so that we can
						// adjust the rectangle to suit

						var bb = this.getBBox();

						this.parentNode.setAttribute("height", bb.height+5);
						this.parentNode.setAttribute("width", bb.width+10);


				});

		node.selectAll("rect")
				.attr("height",
							function() {
									return this.parentNode.getAttribute("height")})
				.attr("width",
							function() {
									return this.parentNode.getAttribute("width")});

		// Add a dropshadow to the textbox
	  var filter = defs.append("svg:filter")
				.attr("id", "drop-shadow")
				.attr("filterUnits", "userSpaceOnUse");


		filter.append("svg:feGaussianBlur")
				.attr("in", "SourceAlpha")
				.attr("stdDeviation", 1)
				.attr("result", "blur-output");

		filter.append("svg:feOffset")
				.attr("in", "blur-output")
				.attr("result", "the-shadow")
				.attr("dx", 1.5)
				.attr("dy", 1.5);

		filter.append("svg:feBlend")
				.attr("in", "SourceGraphic")
				.attr("in2", "the-shadow")
				.attr("mode", "normal");

		force.on("tick",
						 function() {
								 link.select("path")
										 .attr("d",
													 function(d) {
															 return moveto(d) + lineto(d);
													 })
										 .attr("stroke",
													 function(d) {
															 return linkColors[d.polarity];
													 });

								 link.select("text")
										 .attr("x",
													 function(d){
															 return (d.target.newX + d.source.newX) / 2;
													 })
										 .attr("y",
													 function(d){
															 return (d.target.newY + d.source.newY) / 2;
													 })

								 node.attr("transform",
													 function(d) {
															 return "translate(" + d.x + "," + d.y + ")";
													 });

						 });
}

function getWindowHeight(){
  	var viewportHeight = 500;
	if (self.innerHeight) {
		// all except Explorer
		viewportHeight = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) {
	 	// Explorer 6 Strict Mode
		viewportHeight = document.documentElement.clientHeight;
	} else if (document.body)  {
		// other Explorers
		viewportHeight = document.body.clientHeight;
	}
	return viewportHeight;
}
