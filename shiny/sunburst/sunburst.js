/* 

Copyright 2017 Observational Health Data Sciences and Informatics

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Authors: Christopher Knoll

Adapted from sequnces sunburst (d3 v4) at: https://bl.ocks.org/kerryrodden/766f8f6d31f645c39f488a0befa1e3c8

*/

define(["d3", "./chart"], function (d3, Chart) {
	"use strict";

	class Sunburst extends Chart {

		getTipDirection(d) {
			return("n");
		}
		
		getTipOffset(d, arc) {
			const bbox = event.target.getBBox();
			const arcCenter = arc.centroid(d);
			let tipOffsetX = Math.abs(bbox.x - arcCenter[0]) - (bbox.width/2)
			let tipOffsetY = Math.abs(bbox.y - arcCenter[1]);
			return([tipOffsetY-10,tipOffsetX]);
		}

		
		
drawLegend(colors) {

  // Dimensions of legend item: width, height, spacing, radius of rounded rect.
  var li = {
    w: 300, h: 30, s: 3, r: 3
  };

  var legend = d3.select("#legend").append("svg:svg")
      .attr("width", li.w)
      .attr("height", d3.keys(colors).length * (li.h + li.s));

  var g = legend.selectAll("g")
      .data(d3.entries(colors))
      .enter().append("svg:g")
      .attr("transform", function(d, i) {
              return "translate(0," + i * (li.h + li.s) + ")";
           });

  g.append("svg:rect")
      .attr("rx", li.r)
      .attr("ry", li.r)
      .attr("width", li.w)
      .attr("height", li.h)
      .style("fill", function(d) { return d.value; });
      
  g.append("svg:text")
      .attr("x", li.w / 2)
      .attr("y", li.h / 2)
      .attr("dy", "0.35em")
      .attr("text-anchor", "middle")
      .text(function(d) { return d.key; });
}
		
		render(data, target, width, height, chartOptions, lookup) {
			
			super.render(data, target, width, height, chartOptions);

			const defaultOptions = {
				tooltip: (d) => {
					return '' //`<div>No Tooltip Set</div>`
				}

			};

			// options
			const options = this.getOptions(defaultOptions, chartOptions);

			// container
			const svg = this.createSvg(target, width, height);
			svg.attr('class', 'sunburst')

	    // this must be done after createSvg()
			this.useTip((tip, options) => {
				tip.attr('class', `d3-tip ${options.tipClass || ""}`)
					.offset(d => d.tipOffset || [-10,0])
					.direction(d => d.tipDirection || "n")
					.html(d => options.tooltip(d))
			}, options);

			const vis = svg.append("svg:g")
				.attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

			const radius = Math.min(width, height) / 2;

			const partition = d3.partition()
				.size([2 * Math.PI, radius]);

			const arc = d3.arc()
				.startAngle(function (d) {
					return d.x0;
				})
				.endAngle(function (d) {
					return d.x1;
				})
				.innerRadius(function (d) {
					return d.y0
				})
				.outerRadius(function (d) {
					return d.y1
				});

			vis.append("svg:circle")
				.attr("r", radius)
				.style("opacity", 0);

			// Turn the data into a d3 hierarchy and calculate the sums.
			const root = d3.hierarchy(data)
				.sum(function (d) {
					return d.size;
				})
				.sort(function (a, b) {
					return b.value - a.value;
				});

			let nodes = partition(root).descendants().filter(d => (d.x1 - d.x0 > 0.005)).reverse(); // 0.005 radians = 0.29 degrees

function search(nameKey){
    for (var i=0; i < lookup.length; i++) {
        if (lookup[i].key === nameKey) {
            return lookup[i].value;
        }
    }
}

var colors = {};
  for (var i=0; i < lookup.length; i++) {
        if (lookup[i].value == 'End') {
          colors[lookup[i].value] = "#FFFFFF" ;
        } else {
          colors[lookup[i].value] = options.colors(lookup[i].value);
        }
}

			if (options.split) {
				const multiNodes = nodes.reduce((result, node) => {
					let splitNodes = options.split(node);
					if (splitNodes.length > 1) {
						node.isSplit = true;
						result = result.concat(splitNodes.map(n => Object.assign(n, {
							isPartialNode: true
						})));
					}
					return result;
				}, []);

				// append arcs, but do not apply tooltips, and only look for 'partial' nodes to select
				vis.data([data]).selectAll("partialnode")
					.data(multiNodes)
					.enter()
					.append("svg:path")
					.attr("d", arc)
					.attr("fill-rule", "evenodd")
					.attr("class", "partial")
					.style("fill", function(d) { return colors[search(d.data.name)]; })
			}

			const self = this;

			// append arcs and tooltips
			vis.data([data]).selectAll("pathnode")
				.data(nodes)
				.enter()
				.append("svg:path")
				.attr("display", function (d) {
					return d.depth ? null : "none";
				})
				.attr("d", arc)
				.attr("fill-rule", "evenodd")
				.attr("class", d => (options.nodeClass && options.nodeClass(d)) || "node")
				.style("fill", d => d.isSplit ? "#000" : colors[search(d.data.name)]) 
				.style("opacity", d => d.isSplit ? 0 : 1)
				.on('mouseover', d => self.tip.show(Object.assign({}, d, { tipDirection: self.getTipDirection(d), tipOffset: self.getTipOffset(d, arc)}), event.target))
				.on('mouseout', d => self.tip.hide(d, event.target))
				.on('click', (d) => options.onclick && options.onclick(d));

this.drawLegend(colors);

		}
	}
	return Sunburst;
});
