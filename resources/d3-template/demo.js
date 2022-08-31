function makeDemo() {

    var margin = {top: 10, right: 30, bottom: 30, left: 60},
        width = 460 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;

    var svg = d3.select("#demo")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform",
              "translate(" + margin.left + "," + margin.top + ")");


    var x = d3.scaleLinear()
        .domain([0, 4])
        .range([ 0, width ]);
    svg.append("g")
        .attr("transform", "translate(" + 0 + "," + height + ")")
        .call(d3.axisBottom(x));

    var y = d3.scaleLinear()
        .domain([0, 2])
        .range([ height, 0]);
    svg.append("g")
        .call(d3.axisLeft(y));

    d3.csv("blah.csv", function(data) {

        svg.append('g')
            .append("circle")
            .attr("cx", x(Number(data.x)) )
            .attr("cy", y(Number(data.y)) )
            .attr("r", 10)
            .style("fill", "red");

    });
};
