const q = d3.queue()
    .defer(d3.json, "chicago_zipcodes.json")
    .defer(d3.csv, "nightlife_shopping_homeprices.csv")
    .await(ready);

// variable to hold master data in memory
var zipData = {};

// svg
const height = 600;
const width = 800;

const svg = d3.select("#map").append("svg").attr("height", height).attr("width", width);

// create the color-bucket scale
const zScale = d3.scale.quantile()
    .domain([0,1])
    .range(['#F50000','#F63000','#F76201','#F89301','#F9C402','#FAF602','#CEFB03','#9EFC03','#6EFD04','#3EFF05']);
    //.range(['#E0FFFB','#C7E8E4','#AED2CD','#95BCB7','#7CA6A0','#639089','#4A7A73','#31645C','#184E45','#00382F']);

// add event listeners
document.getElementById("mode").addEventListener("change", modeChanged);
document.getElementById("metric").addEventListener("change", metricChanged);
document.getElementById("yearDelta").addEventListener("input", yearChanged);

function ready(error, chicagojson, data) {
    if (error) throw error;

    // populate the master dictionary using raw csv data
    // 1. get a list of distinct zip codes and initialize an empty array for each zip
    const zips = [...new Set(data.map(item => +item.ZipCode))];
    zips.forEach(function(zip) {
        zipData[zip] = [];
    });
    // 2. then populate the data
    data.forEach(function(row) {
        zipData[+row.ZipCode].push(row);
    });

    // get list of distinct metrics and add them to the dropdown
    const metrics = [...new Set(data.map(item => item.Category))];
    const metricDropdown = document.getElementById("metric");
    metrics.forEach(function(metric) {
        const option = document.createElement('option');
        option.value = metric;
        option.innerText = metric;

        metricDropdown.appendChild(option);
    });

    // configure and add the tool-tip
    const tip = d3.tip()
        .attr('class', 'tooltip')
        .html(function(d) {
            return getTooltipHtml(d.properties.ZIP);
        })
        .direction('se');
    svg.call(tip);

    // center and scale values tweaked through trial and error; starting values calculated
    // using the example code at http://bl.ocks.org/cjhin/27e01c636dcc0bfa256c7a225971354d
    //-87.48, 41.80
    var projection = d3.geo.mercator()
        .center([-87.48, 41.80])
        .scale(50000)
        .translate([width / 2, height / 2]);

    var path = d3.geo.path().projection(projection);

    // create the map
    svg.selectAll("path")
        .data(chicagojson.features)
        .enter()
        .append("path")
        .attr("d", path)
        .attr("class", "zipcode")
        .attr("stroke", "black")
        .attr("fill", function(d) {
            return calculateZipColor(d.properties.ZIP);
        })
        .on('mouseover', tip.show)
        .on('mouseout', tip.hide);

    // draw the legend
    const legendDomain = [0,.1,.2,.3,.4,.5,.6,.7,.8,.9];

    const legend = svg.selectAll(".legend")
        .data(legendDomain)
        .enter().append("g")
        .attr("transform", function(d, i) { return "translate(430," + (320 - 25 * i) +  ")"; });

    legend.append("rect")
        .attr("width", 25)
        .attr("height", 25)
        .style("fill", function(d) { return zScale(d + 0.01); });

}

function calculateZipColor(zip) {
    const applicationMode = document.getElementById("mode").value;
    const selectedMetric = document.getElementById("metric").value;
    const yearsAhead = document.getElementById("yearDelta").value;

    // make sure we have data for this zip and year
    if (zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == selectedMetric).length == 0)
        return 'yellow';

    const filteredRow = zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == selectedMetric)[0];
    console.log(applicationMode)
    if (applicationMode == "viewByTotal"){
      console.log(document.getElementById("crimeRate").value)
    }

    return zScale(filteredRow.NormalizedValue);
}

function getTooltipHtml(zip) {
    const yearsAhead = document.getElementById("yearDelta").value;
    const rows = zipData[zip].filter(item => item.YearsAhead == yearsAhead);

    return `
        <div class="tooltip">
            <p>Zip Code: ${zip}</p>
            <p>Median Home Price: $${rows.filter(row => row.Category == "Median Home Price")[0].AbsoluteValue.toLocaleString()}</p>
            <p>Nightlife/Social/Entertainment Score: ${rows.filter(row => row.Category == "Nightlife/Social/Entertainment")[0].AbsoluteValue}</p>
            <p>Everyday Shopping Score: ${rows.filter(row => row.Category == "Everyday Shopping")[0].AbsoluteValue}</p>
            <p>Total Number of Crimes: ${rows.filter(row => row.Category == "Crime")[0].AbsoluteValue}</p>
            <p>Education Score: ${rows.filter(row => row.Category == "Education")[0].AbsoluteValue}</p>
        </div>`;
}

function updateMap() {
    svg.selectAll("path")
        .attr("fill", function(d) {
            return calculateZipColor(d.properties.ZIP);
        });
}

function modeChanged(e) {
    const fieldSetForTotal = document.getElementById("fieldSetForTotal");
    const fieldSetForMetric = document.getElementById("fieldSetForMetric");

    if (e.target.value == "viewByMetric") {
        fieldSetForTotal.setAttribute("style", "display: none;");
        fieldSetForMetric.setAttribute("style", "display: block;");
    }
    else {
        fieldSetForTotal.setAttribute("style", "display: block;");
        fieldSetForMetric.setAttribute("style", "display: none;");
    }

    updateMap();
}

function metricChanged(e) {
    updateMap();
}

function yearChanged(e) {
    document.getElementById("calculatedYear").innerText = 2018 + +e.target.value;

    updateMap();
}
