const q = d3.queue()
    .defer(d3.json, "chicago_zipcodes.json")
    .defer(d3.csv, "all.csv")
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
document.getElementById("refresh").addEventListener("click", updateMap);

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
    console.log(metrics)

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

var crime_arr = 0
var edu_arr = 0
var shopping_arr = 0
var nightlife_arr = 0
var homeprice_arr = 0
var park_arr = 0
var playground_arr = 0
var distance_arr = 0
var total_arr = 0

function calculateZipColor(zip) {
    const applicationMode = document.getElementById("mode").value;
    const selectedMetric = document.getElementById("metric").value;
    const yearsAhead = document.getElementById("yearDelta").value;
    const selectedDest = document.getElementById('workZip').value;
    console.log(selectedDest)

    // make sure we have data for this zip and year
    if (selectedMetric != 'Distance To Work') {
        if (zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == selectedMetric).length == 0){
        console.log(zip)
        return 'yellow';
        }
    }

    const filteredRow = zipData[zip].filter(item => (selectedMetric == 'Distance To Work' && item.Category == selectedMetric) && item.Destination == selectedDest||
    (selectedMetric != 'Distance To Work' && item.YearsAhead == yearsAhead && item.Category == selectedMetric))[0];

    // const filteredRow = zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == selectedMetric)[0];

    //If statement to calculate view by total colors
    if (applicationMode == "viewByTotal"){
      //1. Store each value multiplied by weight
      //2. Add all values together
      //3. Divide by some normalizing constant (trial and error)
      //4. Pass number from 3 into return statement

      console.log(zip)

      //Step 1
      crime_arr = zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Crime')[0].NormalizedValue * document.getElementById("crimeRate").value
      edu_arr=zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Education')[0].NormalizedValue * document.getElementById("education").value
      homeprice_arr=zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Median Home Price')[0].NormalizedValue * document.getElementById("price").value
      park_arr=zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Park')[0].NormalizedValue * document.getElementById("park").value
      playground_arr=zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Playground')[0].NormalizedValue * document.getElementById("playground").value
      distance_arr=zipData[zip].filter(item => item.Destination == selectedDest && item.Category == 'Distance To Work')[0].NormalizedValue * document.getElementById("distance").value

      //Step 2 - Handling case when zip code + category combo not in dataset
      //Multiplying value by 0.25 (open to change)
      if(typeof zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Everyday Shopping')[0] == 'undefined' || typeof zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Nightlife/Social/Entertainment')[0] == 'undefined'){
        shopping_arr = 0.25 * document.getElementById("shopping").value
        nighlife_arr= 0.25 * document.getElementById("social").value
      }else{
        shopping_arr = zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == 'Everyday Shopping')[0].NormalizedValue * document.getElementById("shopping").value
        nightlife_arr = zipData[zip].filter(item => item.YearsAhead == yearsAhead && item.Category == "Nightlife/Social/Entertainment")[0].NormalizedValue * document.getElementById("social").value
      }

      //Step 3. Here normalizing constant is sum of preference values from home screen - 2
      total_arr = (crime_arr + edu_arr + homeprice_arr + shopping_arr + nightlife_arr + park_arr + playground_arr + distance_arr) /
      (parseInt(document.getElementById("crimeRate").value) + parseInt(document.getElementById("education").value) +
      parseInt(document.getElementById("price").value) + parseInt(document.getElementById("shopping").value) +
      parseInt(document.getElementById("social").value) + parseInt(document.getElementById("park").value) +
      parseInt(document.getElementById("playground").value) + parseInt(document.getElementById("distance").value) - 2)

      //Step 4 - return to color scale
      return zScale(total_arr)

    }
    else{
      return zScale(filteredRow.NormalizedValue);
    }


}

function getTooltipHtml(zip) {
    const yearsAhead = document.getElementById("yearDelta").value;
    const selectedDest = document.getElementById('workZip').value;
    const rows = zipData[zip].filter(item => (item.Destination != selectedDest && item.YearsAhead == yearsAhead)||
    (item.Destination == selectedDest));
    console.log(rows)

    return `
        <div class="tooltip">
            <p>Zip Code: ${zip}</p>
            <p>Median Home Price: $${rows.filter(row => row.Category == "Median Home Price")[0].AbsoluteValue.toLocaleString()}</p>
            <p>Nightlife/Social/Entertainment Score: ${rows.filter(row => row.Category == "Nightlife/Social/Entertainment")[0].AbsoluteValue}</p>
            <p>Everyday Shopping Score: ${rows.filter(row => row.Category == "Everyday Shopping")[0].AbsoluteValue}</p>
            <p>Total Number of Crimes: ${rows.filter(row => row.Category == "Crime")[0].AbsoluteValue}</p>
            <p>Education Score: ${rows.filter(row => row.Category == "Education")[0].AbsoluteValue}</p>
            <p>Total Number of Parks: ${rows.filter(row => row.Category == "Park")[0].AbsoluteValue}</p>
            <p>Total Number of Playgrounds: ${rows.filter(row => row.Category == "Playground")[0].AbsoluteValue}</p>
            <p>Distance To Work In Miles: ${rows.filter(row => row.Category == "Distance To Work")[0].AbsoluteValue}</p>
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

    // show "Work at ZipCode" when selected Metric is "Distance To Work"
    const selectedMetric = document.getElementById("metric").value;
    var e = document.getElementById("workAt");
    if(selectedMetric == "Distance To Work") {
        e.style.display = "block";
    } else {
        e.style.display = "none";
    }

    updateMap();
}


function updateWorkAt(someForm) {
    var out = '';
    el = someForm.elements[15]
    console.log(el.value)
    if (['6761','12311','60608','60609','60610','60612','60613','60614','60615','60616','60617','60618','60619','60620','60621','60622',
    '60623','60624','60625','60626','60628','60629','60630','60631','60632','60633','60634','60636','60637','60638','60639','60640',
    '60641','60642','60643','60644','60645','60646','60647','60649','60651','60652','60653','60654','60655','60656','60657','60659',
    '60660','60707','60827'].indexOf(el.value) > -1) {
        updateMap();
    }
}

function yearChanged(e) {
    document.getElementById("calculatedYear").innerText = 2018 + +e.target.value;

    updateMap();
}
