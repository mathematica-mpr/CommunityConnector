import {curveTypes} from "@palewire/d3-curves-inputs";

function radarChart(params) {
  var { data, v, title, maxValue, selectedCurve, xOffset, yOffset, labels } = params; 
  
  var chartSpace = d3.select(v);
  
  var radarLine = d3.lineRadial()
    .curve(d3[selectedCurve])
    .radius(d => rScale(d))
    .angle((d, i) => i * angleSlice);
  
  var axesLength =  data.length;
  var axesDomain = data.map(d => d.axis);
  var angleSlice = Math.PI * 2 / axesLength;
  
  var rScale = d3.scaleLinear()
    .domain([0, maxValue])
    .range([0, radius]);
  
  var containerWidth = width-(margin*2);
  var containerHeight = height-(margin*2);
  
  var container = chartSpace.append('g')
    .attr("width", containerWidth)
    .attr("height", containerHeight)
    .attr('transform', `translate(${xOffset}, ${yOffset})`);
  
	var axisGrid = container.append("g")
    .attr("class", "axisWrapper");
	
	axisGrid.selectAll(".gridCircle")
	   .data(d3.range(1,(axisCircles+1)).reverse())
	   .enter()
      .append("circle")
      .attr("class", "gridCircle")
      .attr("r", (d, i) => radius/axisCircles*d)
      .style("fill", "#CDCDCD")
      .style("stroke", "#CDCDCD")
      .style("fill-opacity", 0.1);
  
	var axis = axisGrid.selectAll(".axis")
		.data(axesDomain)
		.enter()
      .append("g")
      .attr("class", "axis");

	axis.append("line")
		.attr("x1", 0)
		.attr("y1", 0)
		.attr("x2", (d, i) => rScale(maxValue*1.1) * Math.cos(angleSlice*i - Math.PI/2))
		.attr("y2", (d, i) => rScale(maxValue*1.1) * Math.sin(angleSlice*i - Math.PI/2))
		.attr("class", "line")
		.style("stroke", "white")
		.style("stroke-width", "2px");
  
  if (labels) {
    axis.append("text")
      .attr("class", "legend")
      .style("font-size", "11px")
      .attr("text-anchor", "middle")
    .attr("font-family", "monospace")
    .attr("dy", "0.35em")
      .attr("x", (d, i) => rScale(maxValue * axisLabelFactor) * Math.cos(angleSlice*i - Math.PI/2))
      .attr("y", (d, i) => rScale(maxValue * axisLabelFactor) * Math.sin(angleSlice*i - Math.PI/2))
      .text(d => d);
  }
  
  var plots = container.append('g')
    .selectAll('g')
    .data([data])
    .join('g')
      .attr("county-number", (d, i) => i)
      .attr("fill", "none")
      .attr("stroke", "red");
  console.log('plots', plots);
  plots.append('path')
    .attr("d", d => radarLine(d.map(v => v.value)))
    .attr("fill", (d, i) => d3.interpolateViridis(i))
    .attr("fill-opacity", 0.1)
    .attr("stroke", (d, i) => d3.interpolateViridis(i))
    .attr("stroke-width", 2);

	plots.selectAll("circle")
		.data(d => d)
    .join("circle")
		  .attr("r", dotRadius)
		  .attr("cx", (d,i) => rScale(d.value) * Math.cos(angleSlice*i - Math.PI/2))
		  .attr("cy", (d,i) => rScale(d.value) * Math.sin(angleSlice*i - Math.PI/2))
		  .style("fill-opacity", 0.8);

  return 'Rendered chart';
}


var height = 600;
var margin = 30;
var width = height;
var radius = (height-(margin*2)) / 2;
var xOffset = (width/2) + margin;
var yOffset = (height/2) + margin;
var wrapWidth = 40;
var axisLabelFactor = 1.12;
var axisCircles = 2;
var dotRadius = 3; 
var el = DOM.svg(width, height+(margin*2));

var chartParams = ({
  data: data,
  v: el,
  title: 'County',
  maxValue: 1,
  selectedCurve: "curveCardinalClosed",
  xOffset: xOffset,
  yOffset: yOffset,
  labels: false
}); 
radarChart(chartParams);
return el;