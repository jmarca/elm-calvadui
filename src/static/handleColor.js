var d3 = require('d3')

var formatCount = d3.format(".0s")

function make_histogram(data,cls,cnt){
    // clean out the old.  I can't get updating to work right now
    d3.select("svg g.hist").selectAll("g."+cls).remove()
    if(data.length <10) return null
    // play with it here, then move to elm if necessary
    var width = 480
    var height = 60
    var vmargin = 30
    var hmargin = 10

    var x = d3.scaleLinear() // scalePow().exponent(exponent)//
            .domain(d3.extent(data))
            .rangeRound([0, width]);

    var bins = d3.histogram()
            .domain(x.domain())
            .thresholds(x.ticks(15))
    (data)

    var y = d3.scaleLinear()
            .domain([0,d3.max(bins,function(d){return d.length})])
            .range([height,0])

    // var t = d3.transition()
    //         .duration(750)
    var g = d3.select("svg g.hist")
            .append("g")
            .attr("class",cls)
            .attr("transform", "translate(0,"+cnt*(height+2*vmargin)+")")

    var bars = g.selectAll(".bar")
        .data(bins)

    var entering = bars.enter()
            .append("g")
            .attr("class","bar")

    entering.append("rect")
        .attr("x", 1)
    entering.append("text")
        .attr("text-anchor", "middle")

    bars = entering.merge(bars)

    bars.attr("transform",function(d){ return "translate("+x(d.x0)+","+y(d.length)+")"})
    bars.selectAll("rect")
        .attr("width", x(bins[0].x1) - x(bins[0].x0) - 1)
        .attr("height", function(d) { return height - y(d.length); })

    bars.selectAll("text")
        .attr("dy",".75em")
        .attr("y",6)
        .attr("x", (x(bins[0].x1) - x(bins[0].x0)) / 2)
        .text(function(d) { return formatCount(d.length); });

    g.selectAll("g.axis--x").remove()
    g.append("g")
        .attr("class", "axis axis--x")
        .attr("transform", "translate(0,"+(height+20)+")")
        .call(d3.axisBottom(x));

    g.selectAll("text.caption").data([cnt])
        .enter()
        .append("text")
        .attr("text-anchor","middle")
        .attr("class","caption")
        .attr("transform", "translate("+(width/2 + hmargin)+",0)")
        .text(function(d){
            var msg =  "Histogram of grid volumes under 1K VMT"
            if(d===1){
                msg =  "Histogram of grid volumes 1K VMT or above"
            }
            return msg
        })
}

/**
 * handleColor
 *
 * @param {Array[Array]} data the data to color.  An array of arrays.
 *   Each of nested arrays has two values: first value is cellid, second
 *   is the float value to use for assigning a color
 * @param {} callback
 * @returns {}
 */
function handleColor(data, maxdomain, exponent, callback){
    console.log('here in handle color with maxdomain = ',maxdomain, ' exponent = ', exponent )
    // passed array of arrays.  Array[0] is cellid, array[1] is value
    var c = d3.scalePow().exponent(exponent)
            .domain([0, maxdomain]) // on one particular
    // hour,156354,
    // so doubling that
            .range([0,1])

    var rainbow = d3.interpolateViridis
    var colormap = {}
    // apply c.interpolateViridis to each incoming thing
    var histdata_under1k = []
    var histdata_over1k = []
    data.forEach(function(row){
        // records of id:String, value:Numeric
        var cellid = row[0]
        var sum = row[1]
        if(sum < 1000 ){
            histdata_under1k.push(sum)
        }else{
            histdata_over1k.push(sum)
        }
        colormap[cellid] = rainbow(c(sum))
    })
    // console.log(maxsum)

    make_histogram(histdata_under1k,"low",0)
    make_histogram(histdata_over1k,"high",1)
    return callback(null,colormap)
}
module.exports = handleColor
