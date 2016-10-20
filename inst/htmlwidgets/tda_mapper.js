HTMLWidgets.widget({

  name: "tda_mapper",

  type: "output",

  factory: function(el, width, height) {

    var svg = d3.select(el)
                .append("svg")
                  .attr("width", width)
                  .attr("height", height);


    var simulation = d3.forceSimulation()
                       .force("link", d3.forceLink().id(function(d) { return d.id; }))
                       .force("charge", d3.forceManyBody().strength(10))
                       .force("collide", d3.forceCollide(23))
                       .force("center", d3.forceCenter(width / 2, height / 2));

    var tooltip = d3.select("body")
                .append("div")
                  .attr('class', 'tooltip')
                  .style('display', 'none')
                  .style('position', 'absolute')
                  .style("left", "10px")
                  .style("top",  "10px");

    return {
      renderValue: function(x) {
        // alias options
        // var options = x.options;

        // convert links and nodes data frames to d3 friendly format
        console.log(x);

        var links = HTMLWidgets.dataframeToD3(x.links);
        var nodes = HTMLWidgets.dataframeToD3(x.nodes);

        var nodeSize = nodes.map(function(d) {return d.size;});
        var nodeColor = nodes.map(function(d) {return d.color;});

        var colorScale =
          d3.scaleLinear()
            .domain([d3.min(nodeColor), 0.5*d3.min(nodeColor) + 0.5*d3.max(nodeColor), d3.max(nodeColor)])
            .range(["red", "yellow", "blue"]);

        var sizeScale =
          d3.scaleLinear()
            .domain([d3.min(nodeSize), d3.max(nodeSize)])
            .range([5, 20]);

        var link = svg.append("g")
                        .attr("class", "links")
                      .selectAll("g")
                      .data(links)
                      .enter().append("g");

        var path = link.append("path")
                           .attr("stroke-width", 1.5)
                           .attr('fill', 'transparent')
                           .attr('stroke', 1.5);

        var node = svg.append("g")
                        .attr("class", "nodes")
                      .selectAll("circle")
                      .data(nodes)
                      .enter().append("circle")
                        .attr("r", function(d) {return sizeScale(d.size);})
                        .attr("fill", function(d) { return colorScale(d.color); });

        node.call(d3.drag()
            .on("start", dragstarted)
            .on("drag", dragged)
            .on("end", dragended));

        simulation
          .nodes(nodes)
          .on("tick", ticked);
        simulation.force("link")
          .links(links);

        function ticked() {
          path
              .attr("d", function(d, i) {
                var p =
                 "M " + d.source.x + " " + d.source.y + " " +
                 "L " + d.target.x + " " + d.target.y;
                return p;
              });

          node
              .attr("cx", function(d) { return d.x; })
              .attr("cy", function(d) { return d.y; });
        }

        function dragstarted(d) {
          if (!d3.event.active) simulation.alphaTarget(0.3).restart();
          d.fx = d.x;
          d.fy = d.y;
        }

        function dragged(d) {
          d.fx = d3.event.x;
          d.fy = d3.event.y;
        }

        function dragended(d) {
        if (!d3.event.active) simulation.alphaTarget(0);
          d.fx = null;
          d.fy = null;
        }

        node.on('mouseover', function(d) {
               tooltip.style("display", "inline-block");
             })
             .on('mousemove', function(d) {
             //  tooltip.html("<table> " +
              //              "<tr> <td class='col1'>Id: </td>" +
              //                   "<td class='col2'>" + d.id  + "</td> </tr>" +
              //               "<tr> <td class='col1'> Size: </td>" +
              //                   "<td class='col2'>" + Math.round(10000 * d.size)/100  + "%" + "</td> </tr> </table>");
               console.log(d.info);
               tooltip.html(d.info);
             })
             .on('mouseout', function(d) {
               tooltip.style('display', 'none');
             });

      },
      resize: function(width, height) {
        svg
          .attr("width", width)
          .attr("height", height);
        simulation
          .force("center", d3.forceCenter(width / 2, height / 2));

        simulation.alpha(0.3)
                  .restart();
      }
    };
  }


});
