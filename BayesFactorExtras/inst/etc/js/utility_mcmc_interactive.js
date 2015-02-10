function revealPlot_interactive( divname, parCode ){

  var mcmcdata = $.parseJSON( $( "#" + divname + "_mcmcdata_" + parCode ).text() );
  
  
  var parName = $( this ).children( "option:selected" ).text();
  var lineDiv = "#" + divname + "_lineplot";
  var histDiv = "#" + divname + "_histplot";
  var width = .48 * $( lineDiv ).parent().width();
  var height = .98 * $( lineDiv ).parent().height();
  
  createLineChart( mcmcdata, parName, lineDiv, width, height, {top: 20, right: 30, bottom: 60, left: 60});
  createHist( mcmcdata, parName, histDiv, 200, width, height, {top: 20, right: 30, bottom: 60, left: 40});

}

createLineChart = function(lineData, parName, lineDiv, plotWidth, plotHeight, margin){
  var tx = 0;
  var scalex = 1;
    
  if( $(lineDiv).data("translatex") !== undefined ){
    tx = $(lineDiv).data("translatex");
    scalex = $(lineDiv).data("scalex");
  }
  
  $(lineDiv).empty();
  var i;

  zipData = [];
  for(i = 0 ; i < lineData.length ; i++){
    zipData.push( { i: i, x: lineData[i]} );
  }
  
  var dataMin = jStat.min(lineData);
  var dataMax = jStat.max(lineData);

  var width = plotWidth - margin.left - margin.right,
    height = plotHeight - margin.top - margin.bottom;

  var x = d3.scale.linear()
    .domain([0, lineData.length])
    .range([0, width]);

  var y = d3.scale.linear()
    .domain([dataMin, dataMax])
    .range([height, 0]);

  var zoom = d3.behavior.zoom()
    .x(x)
    .scaleExtent([1, lineData.length/10])
    .on("zoom", draw)
    .translate([tx, 0])
    .scale(scalex);

  var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");

  var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left");

  var line = d3.svg.line()
    .interpolate("linear")
    .x(function(d) { return x(d.i); })
    .y(function(d) { return y(d.x); });


  var svg = d3.select(lineDiv).append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  svg.append("clipPath")
    .attr("id", "clip")
  .append("rect")
    .attr("x", (x(0) - tx)/scalex)
    .attr("y", y(dataMax))
    .attr("width", (x(lineData.length+1) - x(0))/scalex)
    .attr("height", y(dataMin) - y(dataMax));


  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis)
        .selectAll("text")  
        .style("text-anchor", "end")
        .attr("dx", "-.8em")
        .attr("dy", ".15em")
        .attr("transform", function(d) {
          return "rotate(-65)" 
        });

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
     .append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 0 - margin.left)
        .attr("x",0 - (height / 2))
        .attr("dy", "1em")
        .style("text-anchor", "middle")
        .text(parName);
        
  svg.append("path")
      .datum(zipData)
      .attr("class", "line")
      .attr("d", line)
      .attr("clip-path", "url(#clip)");
  
  svg.append("text")
    .attr("class", "x label")
    .attr("text-anchor", "end")
    .attr("x", width/2)
    .attr("y", height + margin.bottom - 5)
    .style("text-anchor", "middle")
    .text("Iteration");

  svg.append("rect")
    .attr("class", "pane")
    .attr("width", width)
    .attr("height", height)
    .call(zoom);

  function draw() {
      var e = d3.event,
      // now, constrain the x and y components of the translation by the
      // dimensions of the viewport
      tx = Math.min(0, Math.max(e.translate[0], width - width * e.scale));
      
      // then, update the zoom behavior's internal translation, so that
      // it knows how to properly manipulate it on the next movement
      zoom.translate([tx, 0]);
    
    $(lineDiv).data("translatex", tx);
    $(lineDiv).data("scalex", e.scale);
    
    svg.select("g.x.axis").call(xAxis)
      .selectAll("text")  
        .style("text-anchor", "end")
        .attr("dx", "-.8em")
        .attr("dy", ".15em")
        .attr("transform", function(d) {
          return "rotate(-65)" 
        });

    svg.select("path.line").attr("d", line);
  }

}

createHist = function( histData, parName, histDiv, densityPoints, plotWidth, plotHeight, margin ){
  
  $(histDiv).empty();

  var dataMin = jStat.min(histData);
  var dataMax = jStat.max(histData);
  var dataRange = dataMax - dataMin;
  var quart = jStat.quartiles(histData);
  var IQR = quart[2] - quart[0];
  var binWidth =  2 * IQR / Math.pow(histData.length, 1/3);

  var xMin = dataMin - dataRange * .2;
  var xMax = dataMax + dataRange * .2;

  var nBins = ( xMax - xMin ) / binWidth;
  var kernBandwidth = 1.06 * jStat.stdev(histData) * Math.pow(histData.length, -1/5);


  var width = plotWidth - margin.left - margin.right,
    height = plotHeight - margin.top - margin.bottom;

  var x = d3.scale.linear()
    .domain([xMin, xMax])
    .range([0, width]);

  var histogram = d3.layout.histogram()
    .frequency(false)
    .bins(x.ticks(nBins));

    var zoom = d3.behavior.zoom()
      .on("zoom", updateKDE)

  var data = histogram(histData),
      kde = kernelDensityEstimator(gaussianKernel(kernBandwidth), x.ticks(densityPoints));


  var lens = new Array;
  $.map(data, function(x){ lens.push(x.length);} );
  var maxHeight = 1.1 * ( jStat.max(lens) / histData.length ) / data[0].dx;

  var y = d3.scale.linear()
    .domain([0, maxHeight])
    .range([height, 0]);

  var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");


  var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left");

  var line = d3.svg.line()
    .interpolate("linear")
    .x(function(d) { return x(d[0]); })
    .y(function(d) { return y(d[1]); });

  var svg = d3.select(histDiv).append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  svg.append("g")
    .attr("class", "x axis")
    .attr("transform", "translate(0," + height + ")")
    .call(xAxis)
        .selectAll("text")  
        .style("text-anchor", "end")
        .attr("dx", "-.8em")
        .attr("dy", ".15em")
        .attr("transform", function(d) {
          return "rotate(-65)" 
        });

  svg.selectAll(".bar")
      .data(data)
      .enter().insert("rect", ".axis")
      .attr("class", "bar")
      .attr("x", function(d) { return x(d.x) + 1; })
      .attr("y", function(d) { return y(d.y/data[0].dx); })
      .attr("width", x(data[0].dx + data[0].x) - x(data[0].x) - 0)
      .attr("height", function(d) { return (height - y(d.y/data[0].dx)); });

  svg.append("path")
      .datum(kde(histData))
      .attr("class", "line")
      .attr("d", line);

  svg.selectAll(".point")
    .data(histData)
    .enter().insert("rect")
    .attr("class", "point")
    .attr("x", function(d) { return(x(d) - 1); })
    .attr("y", function(d) { return(y(0) - 10); })
    .attr("height", function(d) { return(10); })
    .attr("width", function(d) { return(3); });
    
  svg.append("text")
    .attr("class", "x label")
    .attr("text-anchor", "end")
    .attr("x", width/2)
    .attr("y", height + margin.bottom - 5)
    .style("text-anchor", "middle")
    .text(parName);

  svg.append("rect")
    .attr("class", "pane")
    .attr("width", width)
    .attr("height", height)
    .call(zoom);
  
  function updateKDE(){
    var scale = Math.exp(d3.event.translate[1]);

    kde = kernelDensityEstimator(gaussianKernel(scale*kernBandwidth), x.ticks(densityPoints));
    svg.select("path.line")
      .datum(kde(histData))
      .attr("d", line);
  }


}

function kernelDensityEstimator(kernel, x) {
  return function(sample) {
    return x.map(function(x) {
      return [x, d3.mean(sample, function(v) { return kernel(x - v); })];
    });
  };
}

function epanechnikovKernel(scale) {
  return function(u) {
    return Math.abs(u /= scale) <= 1 ? .75 * (1 - u * u) / scale : 0;
  };
}

function gaussianKernel(scale) {
  return function(u) {
    return 1/Math.sqrt(2*Math.PI) * Math.exp(-.5 * u*u / (scale*scale))/scale;
  };
}
